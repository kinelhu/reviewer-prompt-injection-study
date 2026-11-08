"""Human-validation agreement (JMIR R1).

Reads paper/R1/validation/rating_key.csv (holds the extractor's values) and any
paper/R1/validation/ratings_*.csv rater exports (comma- or semicolon-delimited),
and reports, for each rater vs the extractor and (if >=2 raters) between raters:
  - recommendation: Cohen's kappa (unweighted and quadratic-weighted, ordinal)
  - detected_injection: Cohen's kappa (binary)
  - critique_count, strength_count: ICC(2,1) absolute agreement, Spearman,
    mean difference, and within-1 agreement
No external dependencies beyond the standard library + numpy.
"""

import csv
import glob
import sys
from pathlib import Path

import numpy as np

VAL = Path(__file__).resolve().parent.parent / "paper" / "R1" / "validation"
REC_ORDER = {"REJECT": 1, "MAJOR_REVISION": 2, "MINOR_REVISION": 3, "ACCEPT": 4}


def load(path):
    head = Path(path).read_text().splitlines()[0]
    delim = ";" if head.count(";") > head.count(",") else ","
    return list(csv.DictReader(open(path), delimiter=delim))


def cohen_kappa(a, b, weights=None):
    cats = sorted(set(a) | set(b))
    idx = {c: i for i, c in enumerate(cats)}
    k = len(cats)
    O = np.zeros((k, k))
    for x, y in zip(a, b):
        O[idx[x], idx[y]] += 1
    O /= O.sum()
    r = O.sum(1)
    c = O.sum(0)
    E = np.outer(r, c)
    if weights == "quadratic":
        W = np.array([[((i - j) ** 2) / ((k - 1) ** 2) for j in range(k)] for i in range(k)])
        return 1 - (W * O).sum() / (W * E).sum()
    W = 1 - np.eye(k)  # disagreement weights for unweighted
    return 1 - (W * O).sum() / (W * E).sum()


def icc21(x, y):
    """ICC(2,1) absolute agreement, two raters (columns), n subjects (rows)."""
    M = np.array([x, y], float).T
    n, k = M.shape
    gm = M.mean()
    msr = k * ((M.mean(1) - gm) ** 2).sum() / (n - 1)
    msc = n * ((M.mean(0) - gm) ** 2).sum() / (k - 1)
    mse = ((M - M.mean(1, keepdims=True) - M.mean(0, keepdims=True) + gm) ** 2).sum() / ((n - 1) * (k - 1))
    return (msr - mse) / (msr + (k - 1) * mse + k * (msc - mse) / n)


def spearman(x, y):
    def rank(v):
        order = np.argsort(v, kind="mergesort")
        r = np.empty(len(v))
        r[order] = np.arange(len(v))
        # average ties
        v = np.asarray(v, float)
        for val in set(v):
            m = v == val
            r[m] = r[m].mean()
        return r
    rx, ry = rank(x), rank(y)
    return float(np.corrcoef(rx, ry)[0, 1])


def counts_report(name, h, l):
    h, l = np.array(h, float), np.array(l, float)
    within1 = float(np.mean(np.abs(h - l) <= 1))
    return (f"  {name}: ICC(2,1)={icc21(h, l):.2f}, Spearman={spearman(h, l):.2f}, "
            f"mean diff={np.mean(h - l):+.2f}, within-1={within1:.0%}")


def pair(label, A, B, key_a="_ext", key_b="_ext"):
    print(f"\n=== {label} (n={len(A['rec'])}) ===")
    print(f"  recommendation: kappa={cohen_kappa(A['rec'], B['rec']):.2f} "
          f"(quadratic-weighted {cohen_kappa(A['rec'], B['rec'], 'quadratic'):.2f}), "
          f"agreement={np.mean([x==y for x,y in zip(A['rec'],B['rec'])]):.0%}")
    print(f"  detection: kappa={cohen_kappa(A['det'], B['det']):.2f}, "
          f"agreement={np.mean([x==y for x,y in zip(A['det'],B['det'])]):.0%}")
    print(counts_report("critique_count", A['crit'], B['crit']))
    print(counts_report("strength_count", A['str'], B['str']))


def series(rows, key, field, cast=str):
    return [cast(r[field]) for r in rows]


def main():
    key = {r["position"]: r for r in load(VAL / "rating_key.csv")}
    ext = {
        "rec": [key[p]["llm_recommendation"] for p in sorted(key, key=int)],
        "det": [key[p]["llm_detected_injection"] for p in sorted(key, key=int)],
        "crit": [int(key[p]["llm_critique_count"]) for p in sorted(key, key=int)],
        "str": [int(key[p]["llm_strength_count"]) for p in sorted(key, key=int)],
    }
    raters = {}
    for path in sorted(glob.glob(str(VAL / "ratings_*.csv"))):
        name = Path(path).stem.replace("ratings_", "")
        if name.endswith(" bk") or name.endswith("_bk"):
            continue
        rows = [r for r in load(path) if r.get("recommendation")]
        if len(rows) != len(key):
            print(f"[skip] {name}: {len(rows)}/{len(key)} rated")
            continue
        rows.sort(key=lambda r: int(r["position"]))
        # align to key order and verify ids
        if any(key[r["position"]]["review_id"] != r["review_id"] for r in rows):
            sys.exit(f"{name}: review_id misaligned with key")
        raters[name] = {
            "rec": [r["recommendation"] for r in rows],
            "det": [str(int(r["detected_injection"])) for r in rows],
            "crit": [int(r["critique_count"]) for r in rows],
            "str": [int(r["strength_count"]) for r in rows],
        }

    if not raters:
        sys.exit("No complete rater files found.")

    for name, R in raters.items():
        pair(f"Rater {name} vs extractor", R, ext)

    names = list(raters)
    for i in range(len(names)):
        for j in range(i + 1, len(names)):
            pair(f"Inter-rater: {names[i]} vs {names[j]}", raters[names[i]], raters[names[j]])

    print(f"\nRaters analysed: {', '.join(names)}"
          + ("" if len(names) > 1 else "  (inter-rater pending second rater)"))


if __name__ == "__main__":
    main()

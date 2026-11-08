#!/usr/bin/env python3
"""Detection pass mark and agreement for the R2 human coding (plan and amendments).

Critique and strength counts: ICC(2,1) absolute agreement against the extractor, as for R1, seeded
percentile bootstrap over reviews. Coding finalised 2026-11-07 (Kinan re-read; same numbers).
Exit 1 if an arm is not ready.
"""
import csv, random, sqlite3, sys
from pathlib import Path
V = Path("paper/R2/validation")
ratings = sys.argv[1] if len(sys.argv) > 1 and not sys.argv[1].startswith("--") else str(V / "ratings_KEH_final_2026-11-07.csv")
H = {r["review_id"]: r for r in csv.DictReader(open(ratings))}
K = list(csv.DictReader(open(V / ".rating_key.csv")))
db = sqlite3.connect("file:data/study.db?mode=ro", uri=True); db.row_factory = sqlite3.Row
E = {r["review_id"]: r for r in db.execute("SELECT review_id, recommendation, detected_injection, critique_count, strength_count FROM extractions")}
def kappa(a, b):
    n = len(a); cats = set(a) | set(b); po = sum(x == y for x, y in zip(a, b)) / n
    pe = sum(a.count(c) * b.count(c) for c in cats) / n / n
    return None if pe == 1 else (po - pe) / (1 - pe)
arms = sorted({k["model_name"] for k in K}); fail = 0; out = []
for arm in arms:
    rows = [k for k in K if k["model_name"] == arm]
    miss = [k["position"] for k in rows if int(k["review_id"]) not in E]
    if miss:
        print(f"{arm}: not fully extracted, {len(miss)} of {len(rows)} missing"); fail = 1; continue
    h_det = [H[k["code"]]["detected_injection"] for k in rows]; e_det = [str(E[int(k["review_id"])]["detected_injection"]) for k in rows]
    h_rec = [H[k["code"]]["recommendation"] for k in rows]; e_rec = [E[int(k["review_id"])]["recommendation"] for k in rows]
    agree = sum(x == y for x, y in zip(h_det, e_det)); arec = sum(x == y for x, y in zip(h_rec, e_rec))
    print(f"{arm}: detection agreement {agree}/{len(rows)} ({100 * agree / len(rows):.0f}%; pass mark 90%), kappa {kappa(h_det, e_det)}; recommendation {arec}/{len(rows)}, kappa {kappa(h_rec, e_rec)}")
    for k, x, y in zip(rows, h_det, e_det):
        if x != y:
            out.append({"position": k["position"], "code": k["code"], "review_id": k["review_id"], "arm": arm, "prompt": k["prompt_style"], "placement": k["placement"], "human": x, "extractor": y})
    for k, x, y in zip(rows, h_rec, e_rec):
        if x != y:
            out.append({"position": k["position"], "code": k["code"], "review_id": k["review_id"], "arm": arm, "prompt": k["prompt_style"], "placement": k["placement"], "human": x, "extractor": y})
with open(V / ".disagreements.csv", "w", newline="") as fh:
    w = csv.DictWriter(fh, ["position", "code", "review_id", "arm", "prompt", "placement", "human", "extractor"]); w.writeheader(); w.writerows(out)
print(f"disagreements listed: {len(out)}")
if not fail:
    with open("output/r2/validation.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["arm", "n", "det_agree", "det_kappa", "rec_agree", "rec_kappa"])
        for arm in arms:
            rows = [k for k in K if k["model_name"] == arm]
            hd = [H[k["code"]]["detected_injection"] for k in rows]; ed = [str(E[int(k["review_id"])]["detected_injection"]) for k in rows]
            hr = [H[k["code"]]["recommendation"] for k in rows]; er = [E[int(k["review_id"])]["recommendation"] for k in rows]
            w.writerow([arm, len(rows), sum(x == y for x, y in zip(hd, ed)), kappa(hd, ed), sum(x == y for x, y in zip(hr, er)), kappa(hr, er)])
def icc21(x, y):
    n = len(x); gm = (sum(x) + sum(y)) / (2 * n); rm = [(a + b) / 2 for a, b in zip(x, y)]; cm = [sum(x) / n, sum(y) / n]
    msr = 2 * sum((m - gm) ** 2 for m in rm) / (n - 1); msc = n * sum((m - gm) ** 2 for m in cm)
    mse = sum((v - rm[i] - cm[j] + gm) ** 2 for i, r in enumerate(zip(x, y)) for j, v in enumerate(r)) / (n - 1)
    d = msr + mse + 2 * (msc - mse) / n
    return None if d == 0 else (msr - mse) / d
if not fail:
    cnt = [k for k in K if H[k["code"]]["critique_count"] != "" or H[k["code"]]["strength_count"] != ""]
    if any(H[k["code"]][f] == "" for k in cnt for f in ("critique_count", "strength_count")) or any(E[int(k["review_id"])][f] is None for k in cnt for f in ("critique_count", "strength_count")):
        print("count coding has blanks: a review lacks a human or an extractor count"); sys.exit(1)
    with open("output/r2/validation_counts.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["outcome", "n", "icc", "lo", "hi", "mean_diff", "within1", "n_per_arm_min", "n_per_arm_max"])
        per = [sum(k["model_name"] == a for k in cnt) for a in arms]
        rng = random.Random(20261106)
        for f in ("critique_count", "strength_count"):
            hv = [int(H[k["code"]][f]) for k in cnt]; ev = [int(E[int(k["review_id"])][f]) for k in cnt]; n = len(hv); bs = []
            for _ in range(10000):
                i = [rng.randrange(n) for _ in range(n)]; v = icc21([hv[j] for j in i], [ev[j] for j in i])
                if v is not None: bs.append(v)
            bs.sort(); est = icc21(hv, ev)
            w.writerow([f, n, est, bs[int(.025 * len(bs))], bs[int(.975 * len(bs)) - 1], sum(a - b for a, b in zip(hv, ev)) / n, sum(abs(a - b) <= 1 for a, b in zip(hv, ev)) / n, min(per), max(per)])
            print(f"{f}: n={n}, ICC(2,1) {est:.3f} ({bs[int(.025 * len(bs))]:.3f}-{bs[int(.975 * len(bs)) - 1]:.3f}), mean diff human-extractor {sum(a - b for a, b in zip(hv, ev)) / n:+.2f}, within 1: {sum(abs(a - b) <= 1 for a, b in zip(hv, ev))}/{n}")
if not fail:
    # Critique counts in standard-prompt reviews, by baseline or injected, read by both instruments.
    std = [k for k in cnt if k["prompt_style"] == "standard"]
    with open("output/r2/validation_counts_standard.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["cond", "n", "human_min", "human_max", "extractor_min", "extractor_max"])
        for cond, rows in (("baseline", [k for k in std if k["direction"] == "none" or k["placement"] == "none"]), ("injected", [k for k in std if k["placement"] != "none"])):
            hv = [int(H[k["code"]]["critique_count"]) for k in rows]; ev = [E[int(k["review_id"])]["critique_count"] for k in rows]
            w.writerow([cond, len(rows), min(hv), max(hv), min(ev), max(ev)]); print("standard", cond, len(rows), "human", min(hv), max(hv), "extractor", min(ev), max(ev))
    # Per arm, as Amendments 1 and 2 require; 10 reviews each, so the bootstrap shows the imprecision.
    with open("output/r2/validation_counts_arm.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["arm", "outcome", "n", "icc", "lo", "hi", "identical"])
        for arm in arms:
            rows = [k for k in cnt if k["model_name"] == arm]
            for f in ("critique_count", "strength_count"):
                hv = [int(H[k["code"]][f]) for k in rows]; ev = [int(E[int(k["review_id"])][f]) for k in rows]; n = len(hv); bs = []
                for _ in range(10000):
                    i = [rng.randrange(n) for _ in range(n)]; v = icc21([hv[j] for j in i], [ev[j] for j in i])
                    if v is not None: bs.append(v)
                bs.sort(); w.writerow([arm, f, n, icc21(hv, ev), bs[int(.025 * len(bs))], bs[int(.975 * len(bs)) - 1], sum(a == b for a, b in zip(hv, ev))])
    abs_ = sum(abs(int(H[k["code"]]["critique_count"]) - E[int(k["review_id"])]["critique_count"]) for k in cnt) / len(cnt)
    print("critique mean absolute difference", round(abs_, 1))
    open("output/r2/validation_counts_mad.txt", "w").write(f"{abs_:.1f}\n")
    strength_same = sum(int(H[k["code"]]["strength_count"]) == E[int(k["review_id"])]["strength_count"] for k in cnt)
    open("output/r2/validation_strength_same.txt", "w").write(f"{strength_same}\n")
if not fail:
    # Both coded samples together (first: Gemma 4 26B, GPT o4-mini, both GPT-5.6-luna arms; second: Claude Opus 5 and Qwen
    # 3.8 27B arms), against the current extractor output, so models are reported together and per LLM.
    R1K = {r["position"]: r for r in csv.DictReader(open("paper/R1/validation/rating_key.csv"))}
    R1H = list(csv.DictReader(open("paper/R1/validation/ratings_KEH.csv"), delimiter=";"))
    LLM = {"gemma4:26b": "Gemma 4 26B", "o4-mini": "GPT o4-mini", "gpt-5.6-luna": "GPT-5.6-luna", "gpt-5.6-luna-medium": "GPT-5.6-luna", "claude-opus-5-off": "Claude Opus 5", "claude-opus-5-on": "Claude Opus 5", "qwen3.8-27b-off": "Qwen 3.8 27B", "qwen3.8-27b-on": "Qwen 3.8 27B"}
    rows = []
    for h in R1H:
        rid = int(h["review_id"]); e = E[rid]; k = R1K[h["position"]]; assert int(k["review_id"]) == rid
        rows.append(dict(sample=1, llm=LLM[k["model_name"]], h_rec=h["recommendation"], e_rec=e["recommendation"], h_det=str(int(h["detected_injection"])), e_det=str(e["detected_injection"]), h_c=int(h["critique_count"]), e_c=e["critique_count"], h_s=int(h["strength_count"]), e_s=e["strength_count"]))
    for k in K:
        h = H[k["code"]]; e = E[int(k["review_id"])]; has = h["critique_count"] != ""
        rows.append(dict(sample=2, llm=LLM[k["model_name"]], h_rec=h["recommendation"], e_rec=e["recommendation"], h_det=h["detected_injection"], e_det=str(e["detected_injection"]), h_c=int(h["critique_count"]) if has else None, e_c=e["critique_count"] if has else None, h_s=int(h["strength_count"]) if has else None, e_s=e["strength_count"] if has else None))
    with open("output/r2/validation_pooled.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["llm", "n", "rec_agree", "rec_kappa", "det_agree", "det_kappa", "n_counts", "icc_critique", "icc_strength"])
        for g in ["All"] + sorted(set(LLM.values()), key=list(LLM.values()).index):
            rr = [r for r in rows if g == "All" or r["llm"] == g]; cc = [r for r in rr if r["h_c"] is not None]
            w.writerow([g, len(rr), sum(r["h_rec"] == r["e_rec"] for r in rr), kappa([r["h_rec"] for r in rr], [r["e_rec"] for r in rr]), sum(r["h_det"] == r["e_det"] for r in rr), kappa([r["h_det"] for r in rr], [r["e_det"] for r in rr]),
                        len(cc), icc21([r["h_c"] for r in cc], [r["e_c"] for r in cc]), icc21([r["h_s"] for r in cc], [r["e_s"] for r in cc])])
    # 95% CIs for the pooled values: percentile bootstrap over reviews, seeded.
    rb = random.Random(20261106); cc = [r for r in rows if r["h_c"] is not None]; B_ = {"det_kappa": [], "icc_critique": [], "icc_strength": []}
    for _ in range(10000):
        a_ = [rows[rb.randrange(len(rows))] for _ in rows]; c_ = [cc[rb.randrange(len(cc))] for _ in cc]
        v = kappa([r["h_det"] for r in a_], [r["e_det"] for r in a_]);  B_["det_kappa"].append(v) if v is not None else None
        for f, h_, e_ in (("icc_critique", "h_c", "e_c"), ("icc_strength", "h_s", "e_s")):
            v = icc21([r[h_] for r in c_], [r[e_] for r in c_]); B_[f].append(v) if v is not None else None
    with open("output/r2/validation_pooled_ci.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["stat", "lo", "hi"])
        for k_, v_ in B_.items(): v_.sort(); w.writerow([k_, v_[int(.025 * len(v_))], v_[int(.975 * len(v_)) - 1]])
sys.exit(fail)

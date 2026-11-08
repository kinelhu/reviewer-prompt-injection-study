#!/usr/bin/env python3
"""Checks on the frozen detector's output: coverage, impossible states, agreement with human codes, flags."""
import collections as C, csv, glob, re, sqlite3, sys
rows = {}
dups = 0
for f in sorted(glob.glob("data/detection_v1/shard_*.csv")):
    for r in csv.DictReader(open(f)):
        i = int(r["review_id"])
        if i in rows: dups += 1
        rows[i] = r
db = sqlite3.connect("file:data/study.db?mode=ro", uri=True); db.row_factory = sqlite3.Row
R = {r["review_id"]: r for r in db.execute("""SELECT r.review_id, r.model_name m, r.full_review_text t, c.prompt_style ps, c.direction d, c.placement pl, e.detected_injection llm
  FROM reviews r JOIN conditions c USING(condition_id) JOIN extractions e USING(review_id)""")}
print(f"coverage {len(rows)}/{len(R)}; duplicates {dups}; missing {len(set(R) - set(rows))}")
print("states", dict(C.Counter(r["state"] or "NO_STATE" for r in rows.values())))
def table(key):
    t = C.defaultdict(C.Counter)
    for i, r in rows.items(): t[key(R[i])][r["state"] or "NO_STATE"] += 1
    for k in sorted(t): print(" ", k, dict(t[k]))
print("by arm, prompt, condition, IDENTIFIED / total"); 
t = C.defaultdict(lambda: [0, 0])
for i, r in rows.items():
    k = (R[i]["m"], R[i]["ps"], "base" if R[i]["d"] == "baseline" else "inj"); t[k][1] += 1; t[k][0] += r["state"] == "IDENTIFIED"
for k in sorted(t): print(" ", k, t[k])
imp = [i for i, r in rows.items() if r["state"] == "IDENTIFIED" and R[i]["d"] == "baseline"]
print("IDENTIFIED in a baseline review (impossible):", len(imp), imp[:30])
def agree(name, H):
    a = [(H[i], int(rows[i]["state"] == "IDENTIFIED")) for i in H if i in rows]
    tp = sum(x and y for x, y in a); tn = sum(not x and not y for x, y in a); fp = sum(not x and y for x, y in a); fn = sum(x and not y for x, y in a); n = len(a)
    pe = ((tp + fn) * (tp + fp) + (tn + fp) * (tn + fn)) / n / n; k = ((tp + tn) / n - pe) / (1 - pe) if pe < 1 else None
    print(f"{name}: agree {tp + tn}/{n}, kappa {k:.3f}, human-yes-detector-no {fn}, human-no-detector-yes {fp}"); return [i for i in H if i in rows and H[i] != int(rows[i]["state"] == "IDENTIFIED")]
H1 = {int(x["review_id"]): int(x["detected_injection"]) for x in csv.DictReader(open("paper/R1/validation/ratings_KEH.csv"), delimiter=";")}
K = {x["code"]: x for x in csv.DictReader(open("paper/R2/validation/.rating_key.csv"))}
H2 = {int(K[x["review_id"]]["review_id"]): int(x["detected_injection"]) for x in csv.DictReader(open("paper/R2/validation/ratings_KEH_v1_skim_2026-11-07.csv"))}
print("disagree R1 sample", agree("R1 sample vs detector", H1)); print("disagree R2 sample", agree("R2 sample vs detector", H2))
for arm in sorted({R[i]["m"] for i in H2}):
    agree("  " + arm, {i: v for i, v in H2.items() if R[i]["m"] == arm})
print("flags by state and reason", dict(C.Counter((r["state"] or "NO_STATE", r["reason"]) for r in rows.values() if r["needs_human"] == "1")))
nm = lambda s: re.sub(r"[^a-z0-9]+", " ", s.lower()).strip()
nf = [(i, r) for i, r in rows.items() if r["reason"] == "quote not found in review"]
ok = sum(nm(r["quote"]) in nm(R[i]["t"]) for i, r in nf)
print(f"quote not found: {len(nf)}; found after ignoring punctuation and markup: {ok}; really absent: {len(nf) - ok}")
print("no valid state ids", [i for i, r in rows.items() if not r["state"]][:60], )

# Written for the supplement; the detector was not adopted (Deviation 1).
H = {**H1, **H2}; bin_d = lambda i: int(rows[i]["state"] == "IDENTIFIED")
acc = lambda ids, f: sum(H[i] == f(i) for i in ids)
s1, s2 = [i for i in H1 if i in rows], [i for i in H2 if i in rows]
ab = [i for i in R if R[i]["ps"] == "aware" and R[i]["d"] == "baseline"]
main = {"gemma4:26b", "o4-mini", "gpt-5.6-luna-medium", "claude-opus-5-on", "qwen3.8-27b-on"}
si = [i for i in R if R[i]["ps"] == "standard" and R[i]["d"] != "baseline" and R[i]["m"] in main]
ll = lambda i: R[i]["llm"]
with open("output/r2/detector_comparison.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["Check", "Dedicated coder", "Extractor"])
    w.writerow(["Agreement with human codes, original sample", f"{acc(s1, bin_d)}/{len(s1)}", f"{acc(s1, ll)}/{len(s1)}"])
    w.writerow(["Agreement with human codes, new sample", f"{acc(s2, bin_d)}/{len(s2)}", f"{acc(s2, ll)}/{len(s2)}"])
    w.writerow(["Flags in aware-prompt baselines, all arms", sum(bin_d(i) for i in ab), sum(ll(i) for i in ab)])
    w.writerow(["Flags under the standard prompt in injected reviews, five main arms", sum(bin_d(i) for i in si), sum(ll(i) for i in si)])
    w.writerow(["No valid answer", sum(not r["state"] for r in rows.values()), 0])

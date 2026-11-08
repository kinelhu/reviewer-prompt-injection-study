#!/usr/bin/env python3
"""Thinking in the Opus 5 adaptive arm, by condition, from the raw batch results (plan: report by condition)."""
import collections as C, csv, json, sqlite3
db = sqlite3.connect("file:data/study.db?mode=ro", uri=True)
cond = {r[0]: r[1:] for r in db.execute("SELECT condition_id, prompt_style, direction, placement FROM conditions")}
t = C.Counter(); n = C.Counter()
for line in open("data/batch_results_claude-opus-5-on.jsonl"):
    r = json.loads(line); c = int(r["custom_id"].split("_")[1][1:]); m = r["result"]["message"]
    k = (cond[c][0], "baseline" if cond[c][1] == "baseline" else "injected"); n[k] += 1; t[k] += any(b["type"] == "thinking" for b in m["content"])
with open("output/r2/opus_thinking.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["prompt", "condition", "with_thinking", "n"])
    for k in sorted(n): w.writerow([k[0], k[1], t[k], n[k]])

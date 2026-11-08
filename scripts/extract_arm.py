#!/usr/bin/env python3
"""Extract one reviewer arm, optionally on another machine's Ollama (JMIR R2).

Same extractor, prompts and model as `run_experiment.py --extract-only`. Adds an arm filter, an
endpoint, and a label in `extractions.extractor_model` recording which machine scored each review.
Resumes by skipping reviews already extracted. Outcomes go to the database and a log, never to stdout.

  uv run python scripts/extract_arm.py run --arm qwen3.8-27b-on --endpoint http://localhost:11435 --label macstudio
  uv run python scripts/extract_arm.py check --endpoint http://localhost:11435 --n 10
"""
import argparse
import json
import logging
import random
import sqlite3
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
from database import save_extraction
from extractor import ReviewExtractor
from orchestrator import load_config

DB = str(ROOT / "data" / "study.db")
ANALYSED = ("gemma4:26b", "o4-mini", "gpt-5.6-luna", "gpt-5.6-luna-medium")


def make(endpoint, label):
    cfg = load_config(str(ROOT / "config" / "config.yaml"))
    cfg["extractor_model"]["endpoint"] = endpoint
    cfg["extractor_model"]["name"] = f"{cfg['extractor_model']['model_string']}@{label}"
    return ReviewExtractor(cfg)


def cmd_check(a):
    """Re-extract already analysed reviews and compare with stored values. Saves nothing."""
    ex = make(a.endpoint, "check")
    db = sqlite3.connect(f"file:{DB}?mode=ro", uri=True)
    db.row_factory = sqlite3.Row
    marks = ",".join("?" * len(ANALYSED))
    rows = db.execute(f"""SELECT r.review_id, r.model_name, r.full_review_text t, e.recommendation rec,
        e.detected_injection det, e.critique_count crit, e.strength_count str FROM reviews r
        JOIN extractions e USING(review_id) WHERE r.model_name IN ({marks}) ORDER BY r.review_id""", ANALYSED).fetchall()
    rng = random.Random(20261106)
    n_arm = a.n // len(ANALYSED) + 1
    pick = []
    for m in ANALYSED:
        pick += rng.sample([r for r in rows if r["model_name"] == m], n_arm)
    pick = pick[: a.n]
    rec = det = crit = stre = 0
    secs = []
    for r in pick:
        t = time.time()
        e = ex.extract(r["t"])
        secs.append(time.time() - t)
        rec += e["recommendation"] == r["rec"]
        det += int(e["detected_injection"]) == r["det"]
        crit += e.get("critique_count") == r["crit"]
        stre += e.get("strength_count") == r["str"]
    print(f"{len(pick)} reviews; recommendation {rec}/{len(pick)}; detection {det}/{len(pick)}; "
          f"critique exact {crit}/{len(pick)}; strength exact {stre}/{len(pick)}; mean {sum(secs) / len(secs):.0f} s")


def cmd_run(a):
    logging.basicConfig(filename=str(ROOT / "data" / f"extract_{a.arm.replace(':', '_')}.log"), level=logging.INFO,
                        format="%(asctime)s %(levelname)s %(message)s")
    ex = make(a.endpoint, a.label)
    db = sqlite3.connect(f"file:{DB}?mode=ro", uri=True)
    todo = db.execute("""SELECT r.review_id, r.full_review_text FROM reviews r LEFT JOIN extractions e USING(review_id)
        WHERE e.review_id IS NULL AND r.model_name=? ORDER BY r.review_id""", (a.arm,)).fetchall()
    db.close()
    ok = 0
    fails = []
    for review_id, text in todo:
        for attempt in range(4):
            try:
                res = ex.extract(text)
                save_extraction(DB, {"review_id": review_id, "recommendation": res["recommendation"],
                                     "recommendation_ordinal": res["recommendation_ordinal"],
                                     "detected_injection": int(res["detected_injection"]),
                                     "emergent_behaviors": res["emergent_behaviors"],
                                     "sentiment_score": res.get("sentiment_score"),
                                     "critique_count": res.get("critique_count"),
                                     "strength_count": res.get("strength_count"),
                                     "extractor_model": ex.model_name, "raw_extraction": json.dumps(res)})
                ok += 1
                break
            except Exception as e:
                logging.error("review %s attempt %s: %s", review_id, attempt, str(e)[:200])
                time.sleep(30 * (attempt + 1))
        else:
            fails.append(review_id)
    print(f"{a.arm}: {ok}/{len(todo)} extracted; failed {len(fails)} {fails[:10]}")
    if fails:
        sys.exit(1)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("command", choices=["run", "check"])
    ap.add_argument("--arm")
    ap.add_argument("--endpoint", default="http://localhost:11434")
    ap.add_argument("--label", default="macbook")
    ap.add_argument("--n", type=int, default=10)
    a = ap.parse_args()
    {"run": cmd_run, "check": cmd_check}[a.command](a)


if __name__ == "__main__":
    main()

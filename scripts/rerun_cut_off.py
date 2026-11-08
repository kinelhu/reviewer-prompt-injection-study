#!/usr/bin/env python3
"""Rerun cut-off Gemma reviews once, as the analysis plan requires, and re-extract them (JMIR R2).

Same prompt construction, model, temperature, token ceiling and unset thinking as the original arm. Originals
are appended to data/rerun_log.jsonl before anything is replaced.
"""
import json, os, sqlite3, sys, time
R = os.getcwd(); sys.path[:0] = [R + "/src", R + "/scripts"]
from extractor import ReviewExtractor
from injector import Injector
from model_interface import ModelFactory
from orchestrator import build_paper_content, load_config
IDS = [int(x) for x in sys.argv[1:]]
cfg = load_config(R + "/config/config.yaml"); inj = Injector(cfg["injection_templates"])
sysp = {"standard": cfg["reviewer_system_prompt"].strip(), "aware": cfg["reviewer_system_prompt_aware"].strip()}
model = ModelFactory.create(next(m for m in cfg["models"] if m["name"] == "gemma4:26b")); ex = ReviewExtractor(cfg)
db = sqlite3.connect("data/study.db", timeout=60); db.row_factory = sqlite3.Row
for i in IDS:
    r = db.execute("SELECT r.*, c.direction, c.placement, c.length, c.prompt_style FROM reviews r JOIN conditions c USING(condition_id) WHERE review_id=?", (i,)).fetchone()
    assert r["model_name"] == "gemma4:26b"
    old = dict(r); old_e = dict(db.execute("SELECT * FROM extractions WHERE review_id=?", (i,)).fetchone())
    p = dict(db.execute("SELECT * FROM papers WHERE paper_id=?", (r["paper_id"],)).fetchone())
    body, s = inj.inject(build_paper_content(p), r["direction"], r["placement"], r["length"]); assert s == (r["injected_text"] or "")
    t = time.time(); text, usage = model.query(cfg["reviewer_user_prompt"].replace("{paper_content}", body), system_prompt=sysp[r["prompt_style"]])
    e = ex.extract(text)
    with open("data/rerun_log.jsonl", "a") as fh:
        fh.write(json.dumps({"review_id": i, "when": time.strftime("%F %T"), "original_review": old, "original_extraction": old_e}, default=str) + "\n")
    db.execute("UPDATE reviews SET full_review_text=?, prompt_tokens=?, completion_tokens=? WHERE review_id=?", (text, usage.get("prompt_tokens"), usage.get("completion_tokens"), i))
    db.execute("""UPDATE extractions SET recommendation=?, recommendation_ordinal=?, detected_injection=?, emergent_behaviors=?, sentiment_score=?, critique_count=?, strength_count=?, extractor_model=?, raw_extraction=? WHERE review_id=?""",
               (e["recommendation"], e["recommendation_ordinal"], int(e["detected_injection"]), json.dumps(e["emergent_behaviors"]), e["sentiment_score"], e["critique_count"], e["strength_count"], "gemma4:26b", json.dumps(e), i))
    db.execute("DELETE FROM psycholing WHERE review_id=?", (i,)); db.commit()
    print(f"{i}: {len(old['full_review_text'])} -> {len(text)} chars, tokens {usage.get('completion_tokens')}, {time.time() - t:.0f}s; {old_e['recommendation']} -> {e['recommendation']}; has recommendation word: {any(w in text.upper() for w in ('REJECT', 'MAJOR REVISION', 'MINOR REVISION', 'ACCEPT'))}; ends: ...{text.strip()[-50:]!r}")

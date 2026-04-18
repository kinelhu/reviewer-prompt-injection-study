#!/usr/bin/env python3
"""
Smoke test: review 1 paper under 2 conditions (baseline + favorable plain beginning)
using the first configured model. Runs extraction immediately after.
"""

import logging
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import yaml
from database import init_database, seed_conditions, get_papers, get_conditions, get_stats
from model_interface import ModelFactory
from injector import Injector
from extractor import ReviewExtractor
from database import review_exists, save_review
from orchestrator import build_paper_content


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")

    config_path = "config/config.yaml"
    db_path = "data/study.db"

    with open(config_path) as f:
        config = yaml.safe_load(f)

    # Init DB
    init_database(db_path)
    seed_conditions(db_path)

    papers = get_papers(db_path)
    if not papers:
        print("No papers in DB. Run `make fetch` first.")
        sys.exit(1)

    paper = papers[0]
    paper_text = build_paper_content(paper)
    print(f"\nPaper: {paper['title'][:80]}")
    print(f"Words: {paper['word_count']}")

    # Pick baseline and favorable-plain-beginning-concise conditions
    conditions = get_conditions(db_path)
    target_conditions = [
        c for c in conditions
        if (c["direction"] == "baseline")
        or (c["direction"] == "favorable" and c["obfuscation"] == "plain"
            and c["placement"] == "beginning" and c["length"] == "concise")
    ][:2]

    # Use first model
    model_cfg = config["models"][0]
    model = ModelFactory.create(model_cfg)
    model_name = model_cfg["name"]
    injector = Injector(config["injection_templates"])
    system_prompt = config["reviewer_system_prompt"].strip()
    user_prompt_template = config["reviewer_user_prompt"]

    for cond in target_conditions:
        cid = cond["condition_id"]
        if review_exists(db_path, paper["paper_id"], cid, model_name, 0):
            print(f"\nCondition {cid} ({cond['direction']}) — already exists, skipping.")
            continue

        injected_text, injected_str = injector.inject(
            paper_text,
            cond["direction"],
            cond["placement"],
            cond["length"],
        )

        user_prompt = user_prompt_template.replace("{paper_content}", injected_text)
        print(f"\nQuerying model for condition: {cond['direction']} / {cond['obfuscation']} / {cond['placement']}")
        review_text, usage = model.query(user_prompt, system_prompt=system_prompt)

        rid = save_review(db_path, {
            "paper_id": paper["paper_id"],
            "condition_id": cid,
            "model_name": model_name,
            "run_number": 0,
            "system_prompt_used": 1,
            "injected_text": injected_str,
            "full_review_text": review_text,
            "prompt_tokens": usage.get("prompt_tokens"),
            "completion_tokens": usage.get("completion_tokens"),
        })
        print(f"  Saved review_id={rid} ({usage})")
        print(f"  Review preview: {review_text[:200]}...")

    # Extraction pass
    print("\nRunning extraction pass...")
    extractor = ReviewExtractor(config)
    n = extractor.run_extraction_pass(db_path)
    print(f"Extracted {n} review(s).")

    print(f"\nDB stats: {get_stats(db_path)}")


if __name__ == "__main__":
    main()

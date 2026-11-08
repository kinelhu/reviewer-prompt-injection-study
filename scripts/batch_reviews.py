#!/usr/bin/env python3
"""OpenAI Batch API driver for reviewer runs (JMIR R1, gpt-5.6-luna arm).

Replicates the orchestrator's prompt construction exactly (same
build_paper_content, Injector, and user-prompt template), but submits all
requests as one batch job (50% cost, 24h window) instead of synchronous calls.

The corpus is restricted to papers that already have reviews in the DB (the
experiment's 50), never the full fetched pool, and requests are only built for
paper x condition cells with no existing review for the target model.

Usage:
  uv run python scripts/batch_reviews.py submit [--model gpt-5.6-luna]
  uv run python scripts/batch_reviews.py status
  uv run python scripts/batch_reviews.py fetch     # save results into study.db

After `fetch`, run the extraction pass as usual:
  uv run python scripts/run_experiment.py --extract-only
"""

import argparse
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

import yaml

from database import review_exists, save_review, get_conditions
from injector import Injector
from orchestrator import build_paper_content, load_config

DB_PATH = ROOT / "data" / "study.db"
STATE_DIR = ROOT / "data"


def state_path(model_name: str) -> Path:
    return STATE_DIR / f"batch_state_{model_name.replace(':', '_')}.json"


def get_experiment_papers(db_path):
    """Papers that already carry reviews — the experiment's 50, never the pool."""
    import sqlite3
    con = sqlite3.connect(db_path)
    con.row_factory = sqlite3.Row
    rows = con.execute(
        "SELECT * FROM papers WHERE paper_id IN (SELECT DISTINCT paper_id FROM reviews) "
        "ORDER BY paper_id"
    ).fetchall()
    con.close()
    return [dict(r) for r in rows]


def openai_client():
    from openai import OpenAI
    return OpenAI()


def build_requests(config, model_cfg, papers, conditions):
    injector = Injector(config["injection_templates"])
    system_prompts = {
        "standard": config["reviewer_system_prompt"].strip(),
        "aware":    config["reviewer_system_prompt_aware"].strip(),
    }
    template = config["reviewer_user_prompt"]
    model_name = model_cfg["name"]

    requests, injected_map, skipped = [], {}, 0
    for paper in papers:
        paper_text = build_paper_content(paper)
        if not paper_text:
            continue
        for cond in conditions:
            if review_exists(str(DB_PATH), paper["paper_id"], cond["condition_id"],
                             model_name, 0):
                skipped += 1
                continue
            injected_text, injected_str = injector.inject(
                paper_text, cond["direction"], cond["placement"], cond["length"]
            )
            custom_id = f"p{paper['paper_id']}_c{cond['condition_id']}_r0"
            injected_map[custom_id] = injected_str
            body = {
                "model": model_cfg["model_string"],
                "messages": [
                    {"role": "system", "content": system_prompts[cond["prompt_style"]]},
                    {"role": "user",
                     "content": template.replace("{paper_content}", injected_text)},
                ],
                "max_completion_tokens": model_cfg.get("max_tokens", 8000),
            }
            if model_cfg.get("reasoning_effort") is not None:
                body["reasoning_effort"] = model_cfg["reasoning_effort"]
            elif model_cfg.get("temperature") is not None:
                body["temperature"] = model_cfg["temperature"]
            requests.append({"custom_id": custom_id, "method": "POST",
                             "url": "/v1/chat/completions", "body": body})
    return requests, injected_map, skipped


def cmd_submit(args):
    config = load_config(str(ROOT / "config" / "config.yaml"))
    model_cfg = next(m for m in config["models"] if m["name"] == args.model)
    papers = get_experiment_papers(str(DB_PATH))
    conditions = get_conditions(str(DB_PATH))
    requests, injected_map, skipped = build_requests(config, model_cfg, papers, conditions)
    print(f"papers: {len(papers)}, conditions: {len(conditions)}, "
          f"requests built: {len(requests)}, existing skipped: {skipped}")
    if not requests:
        print("Nothing to submit.")
        return

    jsonl_path = STATE_DIR / f"batch_input_{args.model.replace(':', '_')}.jsonl"
    with open(jsonl_path, "w") as fh:
        for r in requests:
            fh.write(json.dumps(r) + "\n")

    client = openai_client()
    up = client.files.create(file=open(jsonl_path, "rb"), purpose="batch")
    batch = client.batches.create(
        input_file_id=up.id,
        endpoint="/v1/chat/completions",
        completion_window="24h",
        metadata={"project": "reviewer-prompt-injection R1", "model": args.model},
    )
    state = {"batch_id": batch.id, "input_file_id": up.id, "model_name": args.model,
             "n_requests": len(requests), "injected_map": injected_map}
    state_path(args.model).write_text(json.dumps(state))
    print(f"submitted batch {batch.id} ({len(requests)} requests); "
          f"state -> {state_path(args.model)}")


def load_state(model):
    p = state_path(model)
    if not p.exists():
        sys.exit(f"No state file {p} — run submit first.")
    return json.loads(p.read_text())


def cmd_status(args):
    state = load_state(args.model)
    b = openai_client().batches.retrieve(state["batch_id"])
    print(f"batch {b.id}: {b.status} | counts: {b.request_counts}")


def cmd_fetch(args):
    state = load_state(args.model)
    client = openai_client()
    b = client.batches.retrieve(state["batch_id"])
    if b.status != "completed":
        sys.exit(f"batch status is '{b.status}', not completed — nothing fetched.")

    saved = existing = failed = 0
    if b.error_file_id:
        for line in client.files.content(b.error_file_id).text.splitlines():
            failed += 1
            print("ERROR line:", line[:300])

    for line in client.files.content(b.output_file_id).text.splitlines():
        rec = json.loads(line)
        cid = rec["custom_id"]
        resp = rec.get("response")
        if not resp or resp.get("status_code") != 200:
            failed += 1
            print(f"failed request {cid}: {str(resp)[:200]}")
            continue
        body = resp["body"]
        paper_id, condition_id, run = (int(x[1:]) for x in cid.split("_"))
        if review_exists(str(DB_PATH), paper_id, condition_id,
                         state["model_name"], run):
            existing += 1
            continue
        save_review(str(DB_PATH), {
            "paper_id": paper_id,
            "condition_id": condition_id,
            "model_name": state["model_name"],
            "run_number": run,
            "system_prompt_used": 1,
            "injected_text": state["injected_map"].get(cid),
            "full_review_text": body["choices"][0]["message"]["content"],
            "prompt_tokens": body["usage"]["prompt_tokens"],
            "completion_tokens": body["usage"]["completion_tokens"],
        })
        saved += 1

    print(f"fetch done: {saved} saved, {existing} already present, {failed} failed "
          f"of {state['n_requests']} submitted")
    if failed:
        print("Re-run `submit` to rebuild a batch for the failed cells only "
              "(existing reviews are skipped).")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("command", choices=["submit", "status", "fetch"])
    ap.add_argument("--model", default="gpt-5.6-luna")
    args = ap.parse_args()
    {"submit": cmd_submit, "status": cmd_status, "fetch": cmd_fetch}[args.command](args)


if __name__ == "__main__":
    main()

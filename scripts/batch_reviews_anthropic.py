#!/usr/bin/env python3
"""Anthropic Message Batches driver for reviewer runs (JMIR R2, Claude Opus 5 arms).

Same prompt construction as scripts/batch_reviews.py (build_paper_content, Injector,
user-prompt template). Separate file because that one targets the OpenAI batch API.

  uv run python scripts/batch_reviews_anthropic.py submit --model claude-opus-5-off
  uv run python scripts/batch_reviews_anthropic.py status --model claude-opus-5-off
  uv run python scripts/batch_reviews_anthropic.py fetch  --model claude-opus-5-off
  uv run python scripts/batch_reviews_anthropic.py ingest --model claude-opus-5-off

`fetch` stores the raw results and prints counts and cost only, no outcomes, so the
arm stays unread until the analysis plan is fixed. `ingest` writes to the database.
--limit, --db and --state-dir exist for testing on a copy.
"""
import argparse
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))
sys.path.insert(0, str(ROOT / "scripts"))

from dotenv import load_dotenv
load_dotenv(ROOT / ".env", override=True)

import anthropic

from batch_reviews import get_experiment_papers
from database import get_conditions, review_exists, save_review
from injector import Injector
from orchestrator import build_paper_content, load_config

EXPECTED = 50 * 8
BATCH_IN, BATCH_OUT = 2.5e-6, 12.5e-6  # claude-opus-5, batch rate per token


def paths(args):
    d = Path(args.state_dir)
    n = args.model
    return d / f"batch_state_{n}.json", d / f"batch_results_{n}.jsonl"


def build_requests(config, model_cfg, papers, conditions, db):
    injector = Injector(config["injection_templates"])
    system = {"standard": config["reviewer_system_prompt"].strip(),
              "aware": config["reviewer_system_prompt_aware"].strip()}
    template = config["reviewer_user_prompt"]
    reqs, injected = [], {}
    for paper in papers:
        text = build_paper_content(paper)
        if not text:
            sys.exit(f"paper {paper['paper_id']} has no content")
        for c in conditions:
            if review_exists(db, paper["paper_id"], c["condition_id"], model_cfg["name"], 0):
                continue
            body, inj = injector.inject(text, c["direction"], c["placement"], c["length"])
            cid = f"p{paper['paper_id']}_c{c['condition_id']}_r0"
            injected[cid] = inj
            reqs.append({"custom_id": cid, "params": {
                "model": model_cfg["model_string"],
                "max_tokens": model_cfg["max_tokens"],
                "system": system[c["prompt_style"]],
                "messages": [{"role": "user",
                              "content": template.replace("{paper_content}", body)}],
                "thinking": model_cfg["thinking"],
                "output_config": {"effort": model_cfg["effort"]},
            }})
    return reqs, injected


def cmd_submit(args):
    state_p, _ = paths(args)
    if state_p.exists() and not args.force:
        s = json.loads(state_p.read_text())
        sys.exit(f"REFUSED: {state_p} already records batch {s['batch_id']}. Submitting again "
                 "would pay twice. Run status/fetch, or pass --force after checking it failed.")
    config = load_config(str(ROOT / "config" / "config.yaml"))
    cfg = next((m for m in config["models"] if m["name"] == args.model), None)
    if cfg is None or cfg.get("provider") != "anthropic":
        sys.exit(f"REFUSED: {args.model} is not an anthropic entry in config.yaml")
    papers = get_experiment_papers(args.db)
    conds = get_conditions(args.db)
    if len(papers) != 50 or len(conds) != 8:
        sys.exit(f"REFUSED: expected 50 papers x 8 conditions, found {len(papers)} x {len(conds)}")
    reqs, injected = build_requests(config, cfg, papers, conds, args.db)
    if args.limit:
        reqs = reqs[: args.limit]
    elif len(reqs) != EXPECTED:
        sys.exit(f"REFUSED: built {len(reqs)} requests, expected {EXPECTED}. "
                 "A partial arm needs --limit, on purpose.")
    print(f"{args.model}: {len(reqs)} requests; model={cfg['model_string']} "
          f"thinking={cfg['thinking']} effort={cfg['effort']} max_tokens={cfg['max_tokens']}")
    if args.dry_run:
        print("dry run: nothing submitted")
        return
    b = anthropic.Anthropic().messages.batches.create(requests=reqs)
    keep = {r["custom_id"]: injected[r["custom_id"]] for r in reqs}
    state_p.write_text(json.dumps({"batch_id": b.id, "model_name": args.model,
                                   "n_requests": len(reqs), "injected_map": keep}))
    print(f"submitted {b.id}; state -> {state_p}")


def load_state(args):
    p, _ = paths(args)
    if not p.exists():
        sys.exit(f"No state file {p}: run submit first.")
    return json.loads(p.read_text())


def cmd_status(args):
    s = load_state(args)
    b = anthropic.Anthropic().messages.batches.retrieve(s["batch_id"])
    print(f"{b.id}: {b.processing_status} | {b.request_counts}")


def cmd_fetch(args):
    s = load_state(args)
    _, out = paths(args)
    c = anthropic.Anthropic()
    b = c.messages.batches.retrieve(s["batch_id"])
    if b.processing_status != "ended":
        sys.exit(f"batch is '{b.processing_status}', nothing fetched")
    n = cost = thinking = tok_out = 0
    kinds, stops = {}, {}
    with open(out, "w") as fh:
        for r in c.messages.batches.results(s["batch_id"]):
            n += 1
            kinds[r.result.type] = kinds.get(r.result.type, 0) + 1
            fh.write(r.to_json(indent=None) + "\n")
            if r.result.type == "succeeded":
                m = r.result.message
                stops[m.stop_reason] = stops.get(m.stop_reason, 0) + 1
                thinking += any(x.type == "thinking" for x in m.content)
                tok_out += m.usage.output_tokens
                cost += m.usage.input_tokens * BATCH_IN + m.usage.output_tokens * BATCH_OUT
    print(f"results {n}/{s['n_requests']}: {kinds}; stop reasons {stops}")
    print(f"with thinking block: {thinking}/{kinds.get('succeeded', 0)}; "
          f"mean output tokens {tok_out / max(kinds.get('succeeded', 1), 1):.0f}; "
          f"batch-rate cost ${cost:.2f}; raw -> {out}")
    if n != s["n_requests"]:
        sys.exit("FAIL: result count differs from requests submitted")


def cmd_ingest(args):
    s = load_state(args)
    _, out = paths(args)
    if not out.exists():
        sys.exit(f"No results file {out}: run fetch first.")
    saved = existing = 0
    skipped = []
    for line in out.read_text().splitlines():
        r = json.loads(line)
        cid = r["custom_id"]
        res = r["result"]
        if res["type"] != "succeeded" or res["message"]["stop_reason"] != "end_turn":
            skipped.append((cid, res["type"], res.get("message", {}).get("stop_reason")))
            continue
        p, c, run = (int(x[1:]) for x in cid.split("_"))
        if review_exists(args.db, p, c, s["model_name"], run):
            existing += 1
            continue
        m = res["message"]
        text = "".join(b["text"] for b in m["content"] if b["type"] == "text")
        if not text.strip():
            skipped.append((cid, "empty text", m["stop_reason"]))
            continue
        save_review(args.db, {"paper_id": p, "condition_id": c, "model_name": s["model_name"],
                              "run_number": run, "system_prompt_used": 1,
                              "injected_text": s["injected_map"].get(cid),
                              "full_review_text": text,
                              "prompt_tokens": m["usage"]["input_tokens"],
                              "completion_tokens": m["usage"]["output_tokens"]})
        saved += 1
    print(f"ingest: {saved} saved, {existing} already present, {len(skipped)} skipped "
          f"of {s['n_requests']} into {args.db}")
    for x in skipped:
        print("  SKIPPED", x)
    if skipped:
        sys.exit("FAIL: some cells have no usable review; decide how to report them")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("command", choices=["submit", "status", "fetch", "ingest"])
    ap.add_argument("--model", required=True)
    ap.add_argument("--db", default=str(ROOT / "data" / "study.db"))
    ap.add_argument("--state-dir", default=str(ROOT / "data"))
    ap.add_argument("--limit", type=int)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--force", action="store_true")
    a = ap.parse_args()
    {"submit": cmd_submit, "status": cmd_status, "fetch": cmd_fetch, "ingest": cmd_ingest}[a.command](a)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Run one reviewer arm through OpenRouter on a single pinned host (JMIR R2, Qwen arms).

Same prompts as the other arms. Writes raw results to data/or_results_<arm>.jsonl and
resumes from that file. `run` prints counts and cost only, so the arm stays unread until
the analysis plan is fixed; `ingest` writes to the database.

  uv run python scripts/run_openrouter_arm.py check  --model qwen3.8-27b-off
  uv run python scripts/run_openrouter_arm.py run    --model qwen3.8-27b-off --budget 1.5
  uv run python scripts/run_openrouter_arm.py ingest --model qwen3.8-27b-off

`run` stops when spending reaches --budget. --limit, --db and --state-dir are for tests.
"""
import argparse
import json
import os
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path[:0] = [str(ROOT / "src"), str(ROOT / "scripts")]

import requests
from dotenv import load_dotenv
load_dotenv(ROOT / ".env", override=True)

from batch_reviews import get_experiment_papers
from database import get_conditions, review_exists, save_review
from injector import Injector
from orchestrator import build_paper_content, load_config

URL = "https://openrouter.ai/api/v1/chat/completions"
EXPECTED = 50 * 8


def build(cfg_all, cfg, db):
    papers, conds = get_experiment_papers(db), get_conditions(db)
    if len(papers) != 50 or len(conds) != 8:
        sys.exit(f"REFUSED: expected 50 papers x 8 conditions, found {len(papers)} x {len(conds)}")
    inj = Injector(cfg_all["injection_templates"])
    system = {"standard": cfg_all["reviewer_system_prompt"].strip(),
              "aware": cfg_all["reviewer_system_prompt_aware"].strip()}
    out, injected = [], {}
    for p in papers:
        text = build_paper_content(p)
        if not text:
            sys.exit(f"paper {p['paper_id']} has no content")
        for c in conds:
            body, s = inj.inject(text, c["direction"], c["placement"], c["length"])
            cid = f"p{p['paper_id']}_c{c['condition_id']}_r0"
            injected[cid] = s
            out.append((cid, [{"role": "system", "content": system[c["prompt_style"]]},
                              {"role": "user", "content": cfg_all["reviewer_user_prompt"].replace(
                                  "{paper_content}", body)}]))
    return out, injected


def validate(rec, cfg):
    """Why a response cannot count as this arm's review; None if it can."""
    if rec["host"] != cfg["host"]:
        return f"served by {rec['host']}, not {cfg['host']}"
    if rec["finish"] != "stop":
        return f"finish={rec['finish']}"
    if not rec["text"].strip():
        return "empty text"
    if not cfg["reasoning"].get("enabled") and rec["reasoning_chars"] > 0:
        return "reasoning text in an off arm"
    return None


def call(messages, cfg, key):
    body = {"model": cfg["model_string"], "temperature": cfg["temperature"],
            "max_tokens": cfg["max_tokens"], "reasoning": cfg["reasoning"],
            "usage": {"include": True},
            "provider": {"order": [cfg["host"]], "allow_fallbacks": False},
            "messages": messages}
    r = requests.post(URL, json=body, timeout=900,
                      headers={"Authorization": f"Bearer {key}"})
    r.raise_for_status()
    d = r.json()
    if "choices" not in d:
        raise RuntimeError(str(d)[:200])
    ch = d["choices"][0]
    u = d.get("usage", {})
    reasoning = ch["message"].get("reasoning") or ""
    return {"text": ch["message"].get("content") or "", "reasoning": reasoning,
            "reasoning_chars": len(reasoning), "finish": ch.get("finish_reason"),
            "host": d.get("provider"), "prompt_tokens": u.get("prompt_tokens"),
            "completion_tokens": u.get("completion_tokens"), "cost": u.get("cost") or 0}


def paths(a):
    return Path(a.state_dir) / f"or_results_{a.model}.jsonl"


def load_done(p):
    done = {}
    if p.exists():
        for line in p.read_text().splitlines():
            r = json.loads(line)
            done[r["custom_id"]] = r
    return done


def cmd_run(a, cfg_all, cfg):
    jobs, _ = build(cfg_all, cfg, a.db)
    if a.limit is None and len(jobs) != EXPECTED:
        sys.exit(f"REFUSED: built {len(jobs)} jobs, expected {EXPECTED}")
    if a.budget is None:
        sys.exit("REFUSED: run needs --budget")
    jobs = jobs[: a.limit] if a.limit else jobs
    out = paths(a)
    done = load_done(out)
    spent = sum(r.get("cost", 0) for r in done.values())
    todo = [j for j in jobs if j[0] not in done]
    print(f"{a.model}: {len(jobs)} jobs, {len(jobs) - len(todo)} done, ${spent:.3f} spent so far, "
          f"budget ${a.budget:.2f}, host {cfg['host']}", flush=True)
    key = os.environ["OPENROUTER_API_KEY"]
    lock, stop, fails = threading.Lock(), threading.Event(), []
    state = {"spent": spent, "n": 0}

    def work(job):
        cid, messages = job
        err = "not attempted"
        for attempt in range(5):
            if stop.is_set():
                return
            try:
                rec = call(messages, cfg, key)
                with lock:
                    state["spent"] += rec["cost"]
                err = validate(rec, cfg)
                if err is None:
                    rec["custom_id"] = cid
                    with lock:
                        with open(out, "a") as fh:
                            fh.write(json.dumps(rec) + "\n")
                        state["n"] += 1
                        if state["spent"] >= a.budget:
                            stop.set()
                        if state["n"] % 25 == 0:
                            print(f"  {state['n']} new, ${state['spent']:.3f}", flush=True)
                    return
            except Exception as e:
                err = str(e)[:160]
            with lock:
                if state["spent"] >= a.budget:
                    stop.set()
            time.sleep(min(15 * 2 ** attempt, 120))
        with lock:
            fails.append((cid, err))

    with ThreadPoolExecutor(a.workers) as ex:
        for f in as_completed([ex.submit(work, j) for j in todo]):
            f.result()
    final = load_done(out)
    have = [r for j in jobs if (r := final.get(j[0]))]
    print(f"done {len(have)}/{len(jobs)}; failed {len(fails)}; spent ${state['spent']:.3f}; "
          f"with reasoning text {sum(r['reasoning_chars'] > 0 for r in have)}/{len(have)}; "
          f"mean output tokens {sum(r['completion_tokens'] or 0 for r in have) / max(len(have), 1):.0f}")
    for x in fails[:10]:
        print("  FAILED", x)
    if stop.is_set():
        sys.exit(f"STOPPED: budget ${a.budget:.2f} reached. Rerun with a higher --budget to resume.")
    if len(have) != len(jobs):
        sys.exit("FAIL: some cells have no review. Rerun to retry them.")


def cmd_ingest(a, cfg_all, cfg):
    jobs, injected = build(cfg_all, cfg, a.db)
    done = load_done(paths(a))
    saved = existing = 0
    for cid, r in done.items():
        p, c, run = (int(x[1:]) for x in cid.split("_"))
        if review_exists(a.db, p, c, a.model, run):
            existing += 1
            continue
        save_review(a.db, {"paper_id": p, "condition_id": c, "model_name": a.model,
                           "run_number": run, "system_prompt_used": 1,
                           "injected_text": injected[cid], "full_review_text": r["text"],
                           "prompt_tokens": r["prompt_tokens"],
                           "completion_tokens": r["completion_tokens"]})
        saved += 1
    print(f"ingest: {saved} saved, {existing} already present, of {len(done)} into {a.db}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("command", choices=["check", "run", "ingest"])
    ap.add_argument("--model", required=True)
    ap.add_argument("--db", default=str(ROOT / "data" / "study.db"))
    ap.add_argument("--state-dir", default=str(ROOT / "data"))
    ap.add_argument("--budget", type=float)
    ap.add_argument("--limit", type=int)
    ap.add_argument("--workers", type=int, default=6)
    a = ap.parse_args()
    cfg_all = load_config(str(ROOT / "config" / "config.yaml"))
    cfg = next((m for m in cfg_all["models"] if m["name"] == a.model), None)
    if cfg is None or cfg.get("provider") != "openrouter" or not cfg.get("host"):
        sys.exit(f"REFUSED: {a.model} is not a pinned openrouter entry in config.yaml")
    if a.command == "check":
        jobs, _ = build(cfg_all, cfg, a.db)
        print(f"{a.model}: {len(jobs)} jobs; {cfg['model_string']} on {cfg['host']}; "
              f"reasoning={cfg['reasoning']}; temperature={cfg['temperature']}")
        return
    {"run": cmd_run, "ingest": cmd_ingest}[a.command](a, cfg_all, cfg)


if __name__ == "__main__":
    main()

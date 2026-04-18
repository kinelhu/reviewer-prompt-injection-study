#!/usr/bin/env python3
"""Run the experiment: reviews and/or extraction."""

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from orchestrator import main

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run reviewer injection experiment")
    parser.add_argument("--config", default="config/config.yaml")
    parser.add_argument("--db", default="data/study.db")
    parser.add_argument("--extract-only", action="store_true",
                        help="Only run extraction pass on existing reviews")
    parser.add_argument("--reviews-only", action="store_true",
                        help="Only run reviews; skip extraction")
    parser.add_argument("--paper-limit", type=int, default=None,
                        help="Limit to first N papers (for testing)")
    parser.add_argument("--limit", type=int, default=None,
                        help="Limit extraction to first N pending reviews (--extract-only)")
    parser.add_argument("--extractor-backend", default=None,
                        help="Override extractor provider (ollama, openrouter, openai, anthropic)")
    parser.add_argument("--extractor-model", default=None,
                        help="Override extractor model string (e.g. google/gemma-4-26b-a4b-it:free)")
    parser.add_argument("--model", action="append", dest="models", default=None,
                        help="Model name(s) to run (repeat to include multiple; default: all from config)")
    args = parser.parse_args()

    main(
        config_path=args.config,
        db_path=args.db,
        extract_only=args.extract_only,
        reviews_only=args.reviews_only,
        paper_limit=args.paper_limit,
        extract_limit=args.limit,
        model_override=args.models,
        extractor_backend=args.extractor_backend,
        extractor_model=args.extractor_model,
    )

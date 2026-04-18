#!/usr/bin/env python3
"""Fetch papers from PLOS API (stratified by study type) and enrich via CrossRef."""

import argparse
import logging
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import yaml
from database import init_database, seed_conditions
from paper_fetcher import fetch_papers


def main():
    parser = argparse.ArgumentParser(description="Fetch PLOS papers into study DB")
    parser.add_argument("--config", default="config/config.yaml")
    parser.add_argument("--db",     default="data/study.db")
    parser.add_argument("--n",      type=int, help="Papers per study type (overrides config)")
    parser.add_argument("--search", type=str, default=None,
                        help="Extra PLOS query term ANDed into fetch (e.g. 'diabetes', 'abstract:cancer')")
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")

    with open(args.config) as f:
        config = yaml.safe_load(f)

    init_database(args.db)
    seed_conditions(args.db)

    plos = config.get("plos_api", {})
    n    = args.n or plos.get("n_per_study_type", 4)

    papers = fetch_papers(
        n_per_study_type = n,
        journals         = plos.get("journals"),
        min_year         = plos.get("min_year", 2020),
        cache_dir        = plos.get("cache_dir", "data/plos_cache"),
        min_words        = plos.get("min_words", 2000),
        max_words        = plos.get("max_words", 9000),
        study_types      = plos.get("study_types"),
        db_path          = args.db,
        search_term      = args.search,
    )

    print(f"\n✓ Fetched {len(papers)} papers:\n")
    for p in papers:
        cit = p.get("citation_count")
        cit_str = f"{cit} citations" if cit is not None else "citations N/A"
        auth_str = f"{p.get('author_count')} authors" if p.get('author_count') else ""
        print(f"  [{p['study_type']:20s}] {cit_str:18s} {auth_str:12s} {p['title'][:55]}")


if __name__ == "__main__":
    main()

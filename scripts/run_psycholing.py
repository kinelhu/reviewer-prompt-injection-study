#!/usr/bin/env python3
"""Run transformer-based psycholinguistic analysis on stored review texts."""

import argparse
import logging
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from psycholinguistics import PsycholinguisticsAnalyzer


def main() -> None:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(message)s",
        handlers=[
            logging.StreamHandler(sys.stdout),
            logging.FileHandler("data/experiment.log"),
        ],
    )

    parser = argparse.ArgumentParser(
        description="Compute transformer psycholinguistic scores for all reviews."
    )
    parser.add_argument("--db",     default="data/study.db")
    parser.add_argument("--device", default="auto",
                        help="'auto' (GPU if available), 'cpu', or integer GPU index")
    args = parser.parse_args()

    device = args.device
    if device not in ("auto", "cpu"):
        try:
            device = int(device)
        except ValueError:
            pass

    analyzer = PsycholinguisticsAnalyzer(device=device)
    n = analyzer.run_pass(args.db)
    print(f"Done — {n} review(s) analysed.")


if __name__ == "__main__":
    main()

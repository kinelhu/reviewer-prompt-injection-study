#!/usr/bin/env python3
"""Reset the database.

Default: wipe everything and reinitialize from scratch.
--keep-papers: clear reviews and extractions only, preserving the papers table.
"""
import argparse
import sqlite3
import sys
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument("--keep-papers", action="store_true",
                    help="Delete reviews and extractions only; keep papers")
args = parser.parse_args()

db = Path("data/study.db")
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))
from database import init_database, seed_conditions

if args.keep_papers:
    if not db.exists():
        print("No database found — nothing to reset.")
        sys.exit(0)
    conn = sqlite3.connect(str(db))
    conn.execute("DELETE FROM extractions")
    conn.execute("DELETE FROM reviews")
    conn.execute("DELETE FROM conditions")
    conn.commit()
    n_papers = conn.execute("SELECT COUNT(*) FROM papers").fetchone()[0]
    conn.close()
    print(f"✓ Cleared reviews, extractions, and conditions (kept {n_papers} papers)")
    seed_conditions(str(db))
else:
    if db.exists():
        db.unlink()
        print("✓ Deleted data/study.db")
    else:
        print("No database found — nothing to delete.")
    init_database(str(db))
    seed_conditions(str(db))

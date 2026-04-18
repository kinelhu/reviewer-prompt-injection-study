"""Database initialization and management for the reviewer prompt injection study."""

import sqlite3
import json
from pathlib import Path
from datetime import datetime


DB_PATH = "data/study.db"


def get_connection(db_path: str = DB_PATH) -> sqlite3.Connection:
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA foreign_keys = ON")
    return conn


def init_database(db_path: str = DB_PATH) -> None:
    """Create tables and seed conditions."""
    Path(db_path).parent.mkdir(parents=True, exist_ok=True)
    conn = get_connection(db_path)
    cursor = conn.cursor()

    cursor.executescript("""
        CREATE TABLE IF NOT EXISTS papers (
            paper_id        INTEGER PRIMARY KEY AUTOINCREMENT,
            doi             TEXT UNIQUE NOT NULL,
            title           TEXT,
            abstract        TEXT,
            full_text       TEXT,
            journal         TEXT,
            year            INTEGER,
            study_type      TEXT,      -- rct | systematic_review | observational
            word_count      INTEGER,
            -- CrossRef bibliometric metadata (for publication reporting)
            citation_count  INTEGER,   -- is-referenced-by-count
            author_count    INTEGER,
            authors         TEXT,      -- JSON array of "Given Family" strings
            funders         TEXT,      -- JSON array of funder names
            fetched_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );

        CREATE TABLE IF NOT EXISTS conditions (
            condition_id  INTEGER PRIMARY KEY AUTOINCREMENT,
            direction     TEXT NOT NULL,   -- baseline | favorable
            placement     TEXT NOT NULL,   -- none | beginning | middle | end
            length        TEXT NOT NULL,   -- na | concise | verbose
            prompt_style  TEXT NOT NULL,   -- standard | aware
            template_key  TEXT NOT NULL    -- path into config injection_templates
        );

        CREATE TABLE IF NOT EXISTS reviews (
            review_id          INTEGER PRIMARY KEY AUTOINCREMENT,
            paper_id           INTEGER NOT NULL REFERENCES papers(paper_id),
            condition_id       INTEGER NOT NULL REFERENCES conditions(condition_id),
            model_name         TEXT NOT NULL,
            run_number         INTEGER NOT NULL DEFAULT 0,
            system_prompt_used INTEGER NOT NULL DEFAULT 1,
            injected_text      TEXT,
            full_review_text   TEXT,
            prompt_tokens      INTEGER,
            completion_tokens  INTEGER,
            timestamp          TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );

        CREATE TABLE IF NOT EXISTS extractions (
            extraction_id          INTEGER PRIMARY KEY AUTOINCREMENT,
            review_id              INTEGER UNIQUE NOT NULL REFERENCES reviews(review_id),
            recommendation         TEXT,    -- REJECT | MAJOR_REVISION | MINOR_REVISION | ACCEPT
            recommendation_ordinal INTEGER, -- 1-4
            detected_injection     INTEGER, -- 0/1 boolean
            emergent_behaviors     TEXT,    -- JSON array
            sentiment_score        INTEGER, -- 0-10: overall review tone (0=harsh, 10=enthusiastic)
            critique_count         INTEGER, -- number of distinct weaknesses raised
            strength_count         INTEGER, -- number of distinct strengths identified
            extractor_model        TEXT,
            raw_extraction         TEXT,
            timestamp              TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );

        CREATE TABLE IF NOT EXISTS psycholing (
            psycholing_id   INTEGER PRIMARY KEY AUTOINCREMENT,
            review_id       INTEGER UNIQUE NOT NULL REFERENCES reviews(review_id),
            -- SiEBERT: siebert/sentiment-roberta-large-english (softmax probabilities)
            sent_positive   REAL,
            sent_negative   REAL,
            sent_neutral    REAL,
            -- DeBERTa-v3 zero-shot stance (probability of positive label, 0-1)
            assertiveness   REAL,    -- P("assertive and direct")
            certainty       REAL,    -- P("certain and confident")
            positive_affect REAL,    -- P("enthusiastic and praising")
            sentiment_model TEXT,
            stance_model    TEXT,
            computed_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
    """)

    conn.commit()
    conn.close()
    print(f"✓ Database initialized: {db_path}")


def seed_conditions(db_path: str = DB_PATH) -> int:
    """Seed all experimental conditions if not already present. Returns count added."""
    conn = get_connection(db_path)
    cursor = conn.cursor()

    cursor.execute("SELECT COUNT(*) FROM conditions")
    if cursor.fetchone()[0] > 0:
        conn.close()
        return 0

    conditions = []

    for prompt_style in ("standard", "aware"):
        # Baseline: no injection
        conditions.append(("baseline", "none", "na", prompt_style, "baseline"))
        # Favorable × placement (single injection type: sophisticated/verbose)
        for placement in ("beginning", "middle", "end"):
            conditions.append(("favorable", placement, "verbose", prompt_style, "favorable.verbose"))

    cursor.executemany(
        "INSERT INTO conditions (direction, placement, length, prompt_style, template_key) VALUES (?,?,?,?,?)",
        conditions
    )
    conn.commit()
    count = len(conditions)
    conn.close()
    print(f"✓ Seeded {count} conditions")
    return count


# ---------- papers ----------

def insert_paper(db_path: str, paper: dict) -> int | None:
    """Insert a paper; return paper_id or None if DOI already exists."""
    conn = get_connection(db_path)
    cursor = conn.cursor()
    try:
        cursor.execute("""
            INSERT INTO papers
              (doi, title, abstract, full_text, journal, year, study_type, word_count,
               citation_count, author_count, authors, funders)
            VALUES
              (:doi, :title, :abstract, :full_text, :journal, :year, :study_type, :word_count,
               :citation_count, :author_count, :authors, :funders)
        """, paper)
        paper_id = cursor.lastrowid
        conn.commit()
        return paper_id
    except sqlite3.IntegrityError:
        return None
    finally:
        conn.close()


def get_papers(db_path: str = DB_PATH) -> list[dict]:
    conn = get_connection(db_path)
    cursor = conn.cursor()
    cursor.execute("SELECT * FROM papers ORDER BY paper_id")
    rows = [dict(r) for r in cursor.fetchall()]
    conn.close()
    return rows


# ---------- conditions ----------

def get_conditions(db_path: str = DB_PATH) -> list[dict]:
    conn = get_connection(db_path)
    cursor = conn.cursor()
    cursor.execute("SELECT * FROM conditions ORDER BY condition_id")
    rows = [dict(r) for r in cursor.fetchall()]
    conn.close()
    return rows


# ---------- reviews ----------

def review_exists(db_path: str, paper_id: int, condition_id: int, model_name: str, run_number: int) -> bool:
    conn = get_connection(db_path)
    cursor = conn.cursor()
    cursor.execute("""
        SELECT 1 FROM reviews
        WHERE paper_id=? AND condition_id=? AND model_name=? AND run_number=?
    """, (paper_id, condition_id, model_name, run_number))
    exists = cursor.fetchone() is not None
    conn.close()
    return exists


def save_review(db_path: str, review: dict) -> int:
    """Insert a review row; returns review_id."""
    conn = get_connection(db_path)
    cursor = conn.cursor()
    cursor.execute("""
        INSERT INTO reviews
          (paper_id, condition_id, model_name, run_number, system_prompt_used,
           injected_text, full_review_text, prompt_tokens, completion_tokens)
        VALUES
          (:paper_id, :condition_id, :model_name, :run_number, :system_prompt_used,
           :injected_text, :full_review_text, :prompt_tokens, :completion_tokens)
    """, review)
    review_id = cursor.lastrowid
    conn.commit()
    conn.close()
    return review_id


def get_unextracted_reviews(db_path: str = DB_PATH) -> list[dict]:
    """Return reviews that have no extraction yet."""
    conn = get_connection(db_path)
    cursor = conn.cursor()
    cursor.execute("""
        SELECT r.* FROM reviews r
        LEFT JOIN extractions e ON e.review_id = r.review_id
        WHERE e.extraction_id IS NULL
        ORDER BY r.review_id
    """)
    rows = [dict(r) for r in cursor.fetchall()]
    conn.close()
    return rows


# ---------- extractions ----------

def save_extraction(db_path: str, extraction: dict) -> int:
    conn = get_connection(db_path)
    cursor = conn.cursor()
    if isinstance(extraction.get("emergent_behaviors"), list):
        extraction = dict(extraction)
        extraction["emergent_behaviors"] = json.dumps(extraction["emergent_behaviors"])
    cursor.execute("""
        INSERT OR REPLACE INTO extractions
          (review_id, recommendation, recommendation_ordinal, detected_injection,
           emergent_behaviors, sentiment_score, critique_count, strength_count,
           extractor_model, raw_extraction)
        VALUES
          (:review_id, :recommendation, :recommendation_ordinal, :detected_injection,
           :emergent_behaviors, :sentiment_score, :critique_count, :strength_count,
           :extractor_model, :raw_extraction)
    """, extraction)
    extraction_id = cursor.lastrowid
    conn.commit()
    conn.close()
    return extraction_id


# ---------- psycholinguistics ----------

def get_reviews_without_psycholing(db_path: str = DB_PATH) -> list[dict]:
    """Return reviews that have no psycholinguistic analysis yet."""
    conn = get_connection(db_path)
    cursor = conn.cursor()
    cursor.execute("""
        SELECT r.review_id, r.full_review_text
        FROM reviews r
        LEFT JOIN psycholing p ON p.review_id = r.review_id
        WHERE p.psycholing_id IS NULL
          AND r.full_review_text IS NOT NULL
        ORDER BY r.review_id
    """)
    rows = [dict(r) for r in cursor.fetchall()]
    conn.close()
    return rows


def save_psycholing(db_path: str, record: dict) -> None:
    conn = get_connection(db_path)
    cursor = conn.cursor()
    cursor.execute("""
        INSERT OR REPLACE INTO psycholing
          (review_id, sent_positive, sent_negative, sent_neutral,
           assertiveness, certainty, positive_affect,
           sentiment_model, stance_model)
        VALUES
          (:review_id, :sent_positive, :sent_negative, :sent_neutral,
           :assertiveness, :certainty, :positive_affect,
           :sentiment_model, :stance_model)
    """, record)
    conn.commit()
    conn.close()


# ---------- stats ----------

def get_stats(db_path: str = DB_PATH) -> dict:
    conn = get_connection(db_path)
    cursor = conn.cursor()
    stats = {}
    for table in ("papers", "conditions", "reviews", "extractions"):
        cursor.execute(f"SELECT COUNT(*) FROM {table}")
        stats[table] = cursor.fetchone()[0]
    conn.close()
    return stats


if __name__ == "__main__":
    init_database()
    seed_conditions()
    print(get_stats())

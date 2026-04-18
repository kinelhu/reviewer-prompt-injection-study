"""
PLOS API client — stratified fetch of full-text Research Articles,
enriched with CrossRef bibliometric metadata.
"""

import json
import logging
import re
import time
from pathlib import Path
from typing import Optional

import requests

try:
    from bs4 import BeautifulSoup
    _BS4 = True
except ImportError:
    _BS4 = False

from database import insert_paper

logger = logging.getLogger(__name__)

PLOS_SEARCH_URL = "https://api.plos.org/search"
CROSSREF_URL    = "https://api.crossref.org/works"
PAGE_SIZE = 50

# ── Study-type query fragments appended to the base PLOS query ────────────────
# Each narrows the result set to a specific study design via abstract keywords.
STUDY_TYPE_QUERIES = {
    "rct": (
        'abstract:("randomized controlled trial" OR "randomised controlled trial"'
        ' OR "randomized trial" OR "randomised trial")'
    ),
    "systematic_review": (
        'abstract:("systematic review" OR "meta-analysis" OR "meta analysis")'
    ),
    "observational": (
        'abstract:(cohort OR "cross-sectional" OR "case-control" OR observational)'
        ' AND NOT abstract:("randomized" OR "randomised" OR "systematic review"'
        ' OR "meta-analysis")'
    ),
}


# ── HTML / text utilities ─────────────────────────────────────────────────────

def _strip_html(html: str) -> str:
    if not html:
        return ""
    if _BS4:
        return BeautifulSoup(html, "lxml").get_text(separator="\n").strip()
    return re.sub(r"<[^>]+>", " ", html).strip()


def _count_words(text: str) -> int:
    return len(text.split())


# ── PLOS API ──────────────────────────────────────────────────────────────────

def _plos_fetch_page(query: str, start: int) -> tuple[list[dict], int]:
    """Fetch one page from PLOS Search. Returns (docs, total_found)."""
    params = {
        "q": query,
        "fl": "id,title_display,abstract,body,journal,publication_date,article_type",
        "wt": "json",
        "rows": PAGE_SIZE,
        "start": start,
        "sort": "publication_date desc",
    }
    resp = requests.get(PLOS_SEARCH_URL, params=params, timeout=30)
    resp.raise_for_status()
    data = resp.json()
    response = data.get("response", {})
    return response.get("docs", []), response.get("numFound", 0)


def _fetch_study_type_bucket(
    base_query: str,
    study_type: str,
    n: int,
    min_words: int,
    max_words: int,
    cache_dir: Path,
    db_path: str,
) -> list[dict]:
    """
    Fetch up to `n` papers of a given study type.
    Returns list of inserted paper dicts (without CrossRef data yet).
    """
    type_filter = STUDY_TYPE_QUERIES[study_type]
    query = f"({base_query}) AND ({type_filter})"

    inserted = []
    start = 0
    total_found = None

    while len(inserted) < n:
        if total_found is not None and start >= total_found:
            logger.info(f"  [{study_type}] API exhausted after {len(inserted)}/{n} papers.")
            break

        try:
            docs, total_found = _plos_fetch_page(query, start)
        except requests.exceptions.RequestException as e:
            logger.error(f"  [{study_type}] PLOS API error at start={start}: {e}")
            break

        if not docs:
            break

        cache_file = cache_dir / f"plos_{study_type}_{start}_{int(time.time())}.json"
        cache_file.write_text(json.dumps({"docs": docs}, indent=2))

        for doc in docs:
            if len(inserted) >= n:
                break

            doi      = doc.get("id", "").strip()
            body_raw = doc.get("body", "")
            if not doi or not body_raw:
                continue

            title = _strip_html(doc.get("title_display", "").strip())
            abstract_raw = (
                doc.get("abstract", [""])[0]
                if isinstance(doc.get("abstract"), list)
                else doc.get("abstract", "")
            )
            abstract  = _strip_html(abstract_raw)
            full_text = _strip_html(body_raw)
            word_count = _count_words(full_text)

            if not (min_words <= word_count <= max_words):
                logger.debug(f"  [{study_type}] Skip ({word_count}w): {title[:55]}")
                continue

            pub_date = doc.get("publication_date", "")
            year = int(pub_date[:4]) if pub_date and len(pub_date) >= 4 else None

            paper = {
                "doi": doi,
                "title": title,
                "abstract": abstract,
                "full_text": full_text,
                "journal": doc.get("journal", ""),
                "year": year,
                "study_type": study_type,
                "word_count": word_count,
                # CrossRef fields — filled in later
                "citation_count": None,
                "author_count": None,
                "authors": None,
                "funders": None,
            }

            paper_id = insert_paper(db_path, paper)
            if paper_id is not None:
                paper["paper_id"] = paper_id
                inserted.append(paper)
                logger.info(f"  [{study_type}] [{len(inserted)}/{n}] {title[:65]}")
            else:
                logger.debug(f"  [{study_type}] Duplicate DOI: {doi}")

        start += PAGE_SIZE

    return inserted


# ── CrossRef enrichment ───────────────────────────────────────────────────────

def enrich_from_crossref(doi: str) -> dict:
    """
    Fetch bibliometric metadata from CrossRef for a single DOI.

    Returns a dict with: citation_count, author_count, authors (JSON),
    funders (JSON). Returns empty dict on failure.
    """
    url = f"{CROSSREF_URL}/{doi}"
    headers = {
        # CrossRef polite pool — include a contact email in production
        "User-Agent": "reviewer-prompt-injection/0.1 (mailto:kinan.elhusseini@aphp.fr)"
    }
    try:
        resp = requests.get(url, headers=headers, timeout=15)
        if resp.status_code == 404:
            logger.debug(f"CrossRef 404 for {doi}")
            return {}
        resp.raise_for_status()
    except requests.exceptions.RequestException as e:
        logger.warning(f"CrossRef request failed for {doi}: {e}")
        return {}

    msg = resp.json().get("message", {})

    authors = []
    for a in msg.get("author", []):
        given  = a.get("given", "").strip()
        family = a.get("family", "").strip()
        name = f"{given} {family}".strip() if given else family
        if name:
            authors.append(name)

    funders = [f.get("name", "") for f in msg.get("funder", []) if f.get("name")]

    return {
        "citation_count": msg.get("is-referenced-by-count"),
        "author_count":   len(authors) if authors else None,
        "authors":        json.dumps(authors) if authors else None,
        "funders":        json.dumps(funders) if funders else None,
    }


def _update_paper_metadata(db_path: str, paper_id: int, meta: dict) -> None:
    """Write CrossRef fields back to the papers row."""
    import sqlite3
    conn = sqlite3.connect(db_path)
    conn.execute("""
        UPDATE papers
        SET citation_count=:citation_count,
            author_count=:author_count,
            authors=:authors,
            funders=:funders
        WHERE paper_id=:paper_id
    """, {**meta, "paper_id": paper_id})
    conn.commit()
    conn.close()


# ── Public entry point ────────────────────────────────────────────────────────

def fetch_papers(
    n_per_study_type: int,
    journals: Optional[list[str]] = None,
    min_year: int = 2020,
    cache_dir: str = "data/plos_cache",
    db_path: str = "data/study.db",
    min_words: int = 2000,
    max_words: int = 9000,
    study_types: Optional[list[str]] = None,
    search_term: Optional[str] = None,
) -> list[dict]:
    """
    Stratified fetch: collect `n_per_study_type` papers for each study type.

    `search_term` is ANDed into the base PLOS query to restrict by topic
    (e.g. "diabetes", "cardiovascular", "abstract:cancer").

    After PLOS insertion, each paper is enriched with CrossRef metadata
    (citation count, authors, funders) for publication reporting.

    Returns all inserted papers across all study types.
    """
    cache_path = Path(cache_dir)
    cache_path.mkdir(parents=True, exist_ok=True)

    journals = journals or ["PLOS Medicine"]
    study_types = study_types or ["rct", "systematic_review", "observational"]

    journal_filter = " OR ".join(f'journal:"{j}"' for j in journals)
    base_query = (
        f'({journal_filter})'
        f' AND article_type:"Research Article"'
        f' AND publication_date:[{min_year}-01-01T00:00:00Z TO *]'
    )
    if search_term:
        # If the term looks like raw Solr syntax (has field:, AND/OR, or quotes),
        # pass it through unchanged. Otherwise scope it to title + abstract as a
        # phrase search to avoid noise from incidental body-text matches.
        is_explicit = ('"' in search_term
                       or ':' in search_term
                       or 'AND' in search_term
                       or 'OR' in search_term)
        if is_explicit:
            solr_term = search_term
        else:
            q = f'"{search_term}"' if ' ' in search_term else search_term
            solr_term = f'(abstract:{q} OR title:{q})'
        base_query += f' AND ({solr_term})'

    logger.info(
        f"Stratified fetch: {n_per_study_type} papers × {len(study_types)} types "
        f"from {journals}, {min_words}–{max_words} words"
        + (f", search: {search_term!r}" if search_term else "")
    )

    all_inserted = []

    for study_type in study_types:
        logger.info(f"── {study_type} ──")
        bucket = _fetch_study_type_bucket(
            base_query, study_type, n_per_study_type,
            min_words, max_words, cache_path, db_path
        )
        all_inserted.extend(bucket)

    # ── CrossRef enrichment pass ──────────────────────────────────────────────
    logger.info(f"Enriching {len(all_inserted)} papers from CrossRef...")
    for paper in all_inserted:
        doi = paper["doi"]
        meta = enrich_from_crossref(doi)
        if meta:
            _update_paper_metadata(db_path, paper["paper_id"], meta)
            paper.update(meta)
            logger.info(
                f"  {doi[:35]} → {meta.get('citation_count')} citations, "
                f"{meta.get('author_count')} authors"
            )
        time.sleep(1.0)  # CrossRef polite pool rate limit

    logger.info(f"Done. {len(all_inserted)} papers inserted and enriched.")
    return all_inserted

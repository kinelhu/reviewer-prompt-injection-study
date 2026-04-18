"""LLM judge: extracts structured data from peer review text."""

import json
import logging
import re

from model_interface import ModelFactory
from database import get_unextracted_reviews, save_extraction

logger = logging.getLogger(__name__)

ORDINAL_MAP = {
    "REJECT": 1,
    "MAJOR_REVISION": 2,
    "MINOR_REVISION": 3,
    "ACCEPT": 4,
}


_RECOMMENDATION_RE = re.compile(
    r"\b(REJECT|MAJOR[_ ]REVISION|MINOR[_ ]REVISION|ACCEPT)\b", re.I
)


def _parse_json_response(raw: str) -> dict:
    """
    Extract and parse the first JSON object from a potentially noisy response.
    Falls back to regex extraction of individual fields if JSON parsing fails.
    """
    # Strip markdown code fences gemma/other models often add
    cleaned = re.sub(r"```(?:json)?\s*", "", raw).strip().rstrip("`").strip()

    # Try direct parse
    try:
        return json.loads(cleaned)
    except json.JSONDecodeError:
        pass

    # Find first {...} block (greedy — largest JSON object)
    match = re.search(r"\{[\s\S]*\}", cleaned)
    if match:
        try:
            return json.loads(match.group())
        except json.JSONDecodeError:
            pass

    # Last resort: regex extraction of individual fields
    logger.warning("JSON parse failed; falling back to regex extraction.")
    result = {}

    rec_match = _RECOMMENDATION_RE.search(raw)
    if rec_match:
        result["recommendation"] = rec_match.group().upper().replace(" ", "_")

    detected = re.search(r'"detected_injection"\s*:\s*(true|false)', raw, re.I)
    if detected:
        result["detected_injection"] = detected.group(1).lower() == "true"

    if not result.get("recommendation"):
        raise ValueError(f"Could not extract recommendation from response:\n{raw[:1000]}")

    return result


class ReviewExtractor:
    """
    Calls an LLM to extract structured information from a review text.

    Extracted fields:
    - recommendation: REJECT | MAJOR_REVISION | MINOR_REVISION | ACCEPT
    - recommendation_ordinal: 1-4
    - detected_injection: bool
    - emergent_behaviors: list[str]
    - sentiment_score: int 0-10 (0=harsh, 10=enthusiastic)
    - critique_count: int (distinct weaknesses raised)
    - strength_count: int (distinct strengths identified)
    """

    def __init__(self, config: dict):
        extractor_cfg = config["extractor_model"]
        self.model = ModelFactory.create(extractor_cfg)
        self.model_name = extractor_cfg["name"]
        self.system_prompt = config.get("extractor_system_prompt", "").strip()
        self.user_prompt_template = config["extractor_user_prompt"]

    def extract(self, review_text: str) -> dict:
        """Run extraction on a single review. Returns parsed dict."""
        # Use str.replace rather than .format() — the template contains JSON
        # example braces that confuse Python's format string parser.
        user_prompt = self.user_prompt_template.replace("{review_text}", review_text)
        raw, _ = self.model.query(user_prompt, system_prompt=self.system_prompt or None)

        parsed = _parse_json_response(raw)

        recommendation = parsed.get("recommendation", "").upper().replace(" ", "_")
        ordinal = ORDINAL_MAP.get(recommendation, parsed.get("recommendation_ordinal"))

        return {
            "recommendation": recommendation,
            "recommendation_ordinal": ordinal,
            "detected_injection": bool(parsed.get("detected_injection", False)),
            "emergent_behaviors": parsed.get("emergent_behaviors", []),
            "sentiment_score": parsed.get("sentiment_score"),
            "critique_count": parsed.get("critique_count"),
            "strength_count": parsed.get("strength_count"),
        }

    def run_extraction_pass(self, db_path: str, limit: int = None) -> int:
        """Extract unextracted reviews. Pass limit=N to cap the run."""
        reviews = get_unextracted_reviews(db_path)
        if limit is not None:
            reviews = reviews[:limit]
        logger.info(f"Extracting {len(reviews)} unextracted reviews...")
        processed = 0

        for review in reviews:
            review_id = review["review_id"]
            review_text = review.get("full_review_text", "")

            if not review_text:
                logger.warning(f"Review {review_id} has no text, skipping.")
                continue

            try:
                result = self.extract(review_text)
                extraction = {
                    "review_id": review_id,
                    "recommendation": result["recommendation"],
                    "recommendation_ordinal": result["recommendation_ordinal"],
                    "detected_injection": int(result["detected_injection"]),
                    "emergent_behaviors": result["emergent_behaviors"],
                    "sentiment_score": result.get("sentiment_score"),
                    "critique_count": result.get("critique_count"),
                    "strength_count": result.get("strength_count"),
                    "extractor_model": self.model_name,
                    "raw_extraction": json.dumps(result),
                }
                save_extraction(db_path, extraction)
                processed += 1
                logger.info(
                    f"  [{processed}/{len(reviews)}] review_id={review_id} → "
                    f"{result['recommendation']} (ordinal={result['recommendation_ordinal']}, "
                    f"injection_detected={result['detected_injection']})"
                )
            except Exception as e:
                logger.error(f"  Failed to extract review {review_id}: {e}")

        logger.info(f"Extraction pass complete: {processed}/{len(reviews)} succeeded.")
        return processed

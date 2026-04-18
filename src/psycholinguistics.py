"""
Transformer-based psycholinguistic analysis of peer review texts.

Two models, loaded once and reused across all reviews:

  Sentiment  — cardiffnlp/twitter-roberta-base-sentiment-latest
               RoBERTa-base fine-tuned on ~124 M tweets (TweetEval).
               Outputs: sent_positive, sent_negative, sent_neutral (sum to 1).
               ~125 M params; chosen for CPU feasibility on pre-AVX hardware.

  Stance     — cross-encoder/nli-MiniLM2-L6-H768
               MiniLM-L6 cross-encoder fine-tuned on NLI (SNLI + MultiNLI).
               ~22 M params; ~10× faster than DeBERTa-v3-large on CPU.
               Three binary dimensions, each scored as P(positive label):
                 assertiveness   P("assertive and direct")
                 certainty       P("certain and confident")
                 positive_affect P("enthusiastic and praising")

Both pipelines truncate to 512 tokens and run on GPU if available, else CPU.
Results are written to the `psycholing` table (one row per review_id).
"""

import logging
from datetime import datetime

from tqdm import tqdm

logger = logging.getLogger(__name__)

SENTIMENT_MODEL = "cardiffnlp/twitter-roberta-base-sentiment-latest"
STANCE_MODEL    = "cross-encoder/nli-MiniLM2-L6-H768"

# Hypothesis template for the zero-shot classifier.
# The pipeline wraps each candidate label as: "The tone of this peer review is {}."
HYPOTHESIS_TEMPLATE = "The tone of this peer review is {}."

# Each stance dimension: (positive_label, negative_label)
STANCE_DIMS = {
    "assertiveness":   ("assertive and direct",       "tentative and hedged"),
    "certainty":       ("certain and confident",       "uncertain and hedged"),
    "positive_affect": ("enthusiastic and praising",   "critical and skeptical"),
}


class PsycholinguisticsAnalyzer:

    def __init__(self, device: str = "auto"):
        self._sentiment_pipe = None
        self._stance_pipe    = None
        self._device         = device

    def _resolve_device(self) -> int | str:
        """Return torch device index for pipeline (0 = first GPU, -1 = CPU)."""
        if self._device == "auto":
            try:
                import torch
                return 0 if torch.cuda.is_available() else -1
            except ImportError:
                return -1
        return self._device

    def _load_models(self) -> None:
        from transformers import pipeline

        if self._sentiment_pipe is not None:
            return

        device = self._resolve_device()
        device_label = f"GPU:{device}" if isinstance(device, int) and device >= 0 else "CPU"
        logger.info(f"Loading psycholinguistics models on {device_label} …")

        self._sentiment_pipe = pipeline(
            "text-classification",
            model=SENTIMENT_MODEL,
            top_k=None,          # return all labels
            truncation=True,
            max_length=512,
            device=device,
        )
        self._stance_pipe = pipeline(
            "zero-shot-classification",
            model=STANCE_MODEL,
            truncation=True,
            max_length=512,
            device=device,
        )
        logger.info("Psycholinguistics models loaded.")

    def analyze(self, text: str) -> dict:
        """Return psycholinguistic scores for a single review text."""
        self._load_models()

        # ── Sentiment ────────────────────────────────────────────────────────
        sent_raw = self._sentiment_pipe(text)[0]   # list of {label, score}
        sent = {item["label"].lower(): item["score"] for item in sent_raw}
        # SiEBERT labels: POSITIVE, NEGATIVE (no NEUTRAL class)
        result = {
            "sent_positive": sent.get("positive", 0.0),
            "sent_negative": sent.get("negative", 0.0),
            "sent_neutral":  sent.get("neutral",  0.0),
        }

        # ── Stance dimensions ─────────────────────────────────────────────────
        for dim_name, (pos_label, neg_label) in STANCE_DIMS.items():
            out = self._stance_pipe(
                text,
                candidate_labels=[pos_label, neg_label],
                hypothesis_template=HYPOTHESIS_TEMPLATE,
                multi_label=False,
            )
            # Score for the positive label
            label_scores = dict(zip(out["labels"], out["scores"]))
            result[dim_name] = label_scores[pos_label]

        return result

    def run_pass(self, db_path: str, batch_size: int = 8) -> int:
        """
        Compute psycholinguistic scores for all reviews not yet in the
        psycholing table. Returns the number of records written.
        """
        from database import get_reviews_without_psycholing, save_psycholing, init_database
        init_database(db_path)  # creates psycholing table if DB predates it

        pending = get_reviews_without_psycholing(db_path)
        if not pending:
            logger.info("No reviews pending psycholinguistic analysis.")
            return 0

        logger.info(f"Analysing {len(pending)} review(s) …")
        self._load_models()

        written = 0
        for row in tqdm(pending, desc="Psycholing", unit="review"):
            review_id = row["review_id"]
            text      = row["full_review_text"] or ""
            if not text.strip():
                logger.warning(f"review_id={review_id}: empty text, skipping.")
                continue
            try:
                scores = self.analyze(text)
                save_psycholing(db_path, {
                    "review_id":      review_id,
                    "sentiment_model": SENTIMENT_MODEL,
                    "stance_model":    STANCE_MODEL,
                    **scores,
                })
                written += 1
            except Exception as e:
                logger.error(f"review_id={review_id}: {e}")

        logger.info(f"Psycholinguistic analysis complete — {written} record(s) written.")
        return written

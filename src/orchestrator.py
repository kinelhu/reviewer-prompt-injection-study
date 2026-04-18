"""Main experiment pipeline: papers × conditions × models."""

import logging
import sys
from pathlib import Path

import yaml
from tqdm import tqdm

from database import (
    get_papers, get_conditions, review_exists, save_review, get_stats
)
from model_interface import ModelFactory
from injector import Injector
from extractor import ReviewExtractor

logger = logging.getLogger(__name__)


def load_config(config_path: str = "config/config.yaml") -> dict:
    with open(config_path) as f:
        return yaml.safe_load(f)


def build_paper_content(paper: dict) -> str:
    """
    Assemble the full paper text passed to the reviewer model.

    Prepends title and abstract (which PLOS body text omits) then appends
    the body, preserving the complete text for injection integrity.
    """
    parts = []
    if paper.get("title"):
        parts.append(f"Title: {paper['title']}")
    if paper.get("abstract"):
        parts.append(f"Abstract:\n{paper['abstract']}")
    if paper.get("full_text"):
        parts.append(paper["full_text"])
    return "\n\n".join(parts)


class ReviewerInjectionExperiment:

    def __init__(self, config: dict, db_path: str = "data/study.db"):
        self.config = config
        self.db_path = db_path

        # Build injection templates map
        self.injector = Injector(config["injection_templates"])

        # Models to test (filter by names listed in experiment.models_to_test)
        wanted = set(config["experiment"].get("models_to_test", []))
        self.models = {}  # name → model instance
        for m in config["models"]:
            if m["name"] in wanted:
                self.models[m["name"]] = ModelFactory.create(m)
        logger.info(f"Loaded {len(self.models)} model(s): {list(self.models.keys())}")

        self.system_prompts = {
            "standard": config["reviewer_system_prompt"].strip(),
            "aware":    config["reviewer_system_prompt_aware"].strip(),
        }
        self.user_prompt_template = config["reviewer_user_prompt"]
        self.n_runs = config["experiment"].get("n_runs", 1)

    def run(self, paper_limit: int = None) -> None:
        papers = get_papers(self.db_path)
        conditions = get_conditions(self.db_path)

        if paper_limit:
            papers = papers[:paper_limit]

        total = len(papers) * len(conditions) * len(self.models) * self.n_runs
        logger.info(
            f"Starting experiment: {len(papers)} papers × {len(conditions)} conditions "
            f"× {len(self.models)} models × {self.n_runs} run(s) = {total} reviews"
        )

        with tqdm(total=total, desc="Reviews", unit="review") as pbar:
            for paper in papers:
                paper_id = paper["paper_id"]
                paper_text = build_paper_content(paper)

                if not paper_text:
                    logger.warning(f"Paper {paper_id} has no text, skipping.")
                    pbar.update(len(conditions) * len(self.models) * self.n_runs)
                    continue

                for condition in conditions:
                    condition_id  = condition["condition_id"]
                    direction     = condition["direction"]
                    placement     = condition["placement"]
                    length        = condition["length"]
                    prompt_style  = condition["prompt_style"]
                    system_prompt = self.system_prompts[prompt_style]

                    # Build injected paper once per condition (same for all models/runs)
                    try:
                        injected_text, injected_str = self.injector.inject(
                            paper_text, direction, placement, length
                        )
                    except Exception as e:
                        logger.error(f"Injection failed for condition {condition_id}: {e}")
                        pbar.update(len(self.models) * self.n_runs)
                        continue

                    user_prompt = self.user_prompt_template.replace(
                        "{paper_content}", injected_text
                    )

                    for model_name, model in self.models.items():
                        for run in range(self.n_runs):
                            pbar.update(1)

                            if review_exists(self.db_path, paper_id, condition_id, model_name, run):
                                logger.debug(f"Skipping existing: paper={paper_id} cond={condition_id} model={model_name} run={run}")
                                continue

                            try:
                                review_text, usage = model.query(
                                    user_prompt,
                                    system_prompt=system_prompt,
                                )
                                save_review(self.db_path, {
                                    "paper_id": paper_id,
                                    "condition_id": condition_id,
                                    "model_name": model_name,
                                    "run_number": run,
                                    "system_prompt_used": 1,
                                    "injected_text": injected_str,
                                    "full_review_text": review_text,
                                    "prompt_tokens": usage.get("prompt_tokens"),
                                    "completion_tokens": usage.get("completion_tokens"),
                                })
                            except Exception as e:
                                logger.error(
                                    f"Review failed — paper={paper_id} cond={condition_id} "
                                    f"model={model_name} run={run}: {e}"
                                )

        stats = get_stats(self.db_path)
        logger.info(f"Experiment done. DB stats: {stats}")

    def run_extraction(self, limit: int = None) -> None:
        extractor = ReviewExtractor(self.config)
        extractor.run_extraction_pass(self.db_path, limit=limit)


def main(config_path: str = "config/config.yaml", db_path: str = "data/study.db",
         extract_only: bool = False, reviews_only: bool = False,
         paper_limit: int = None, extract_limit: int = None,
         model_override: list = None,
         extractor_backend: str = None, extractor_model: str = None) -> None:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(message)s",
        handlers=[
            logging.StreamHandler(sys.stdout),
            logging.FileHandler("data/experiment.log"),
        ],
    )

    config = load_config(config_path)
    if model_override:
        config["experiment"]["models_to_test"] = model_override
    if extractor_backend:
        config["extractor_model"]["provider"] = extractor_backend
    if extractor_model:
        config["extractor_model"]["model_string"] = extractor_model
        config["extractor_model"]["name"] = extractor_model
    exp = ReviewerInjectionExperiment(config, db_path)

    if extract_only:
        exp.run_extraction(limit=extract_limit)
    elif reviews_only:
        exp.run(paper_limit=paper_limit)
    else:
        exp.run(paper_limit=paper_limit)
        exp.run_extraction(limit=extract_limit)

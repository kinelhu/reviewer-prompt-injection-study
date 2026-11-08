# Prompt Injection in AI-Assisted Peer Review of Biomedical Manuscripts: A Controlled Experiment Across Large Language Models

Code and analysis for:

> El Husseini K, Sidali S. *Prompt Injection in AI-Assisted Peer Review of Biomedical Manuscripts: A Controlled Experiment Across Large Language Models.* (under review)

**Dataset:** https://doi.org/10.5281/zenodo.19643480 (concept DOI, resolves to the latest version; current: v3)

---

## Overview

This repository contains the full experimental pipeline and statistical analysis code for a controlled study examining prompt injection attacks in AI-assisted peer review.

**Design in brief:**

- 50 published open-access biomedical articles (PLOS Medicine; 33 randomized controlled trials, 13 systematic reviews, 4 observational studies)
- Five reviewer models in eight arms: Gemma 4 26B (local, Ollama), GPT o4-mini (OpenAI API), and GPT-5.6-luna (OpenAI Batch API), Claude Opus 5 (Anthropic Batch API) and Qwen 3.8 27B (OpenRouter), the last three each with reasoning off and on
- One injection type: a realistic editorial-note directive targeting ACCEPT or MINOR REVISION, inserted at the beginning, middle, or end of the full manuscript text
- Two reviewer prompt styles: standard and injection-aware (with explicit detection instruction)
- 3,200 reviews (50 papers × 8 conditions × 8 arms); outcomes extracted by a separate Gemma 4 26B instance at temperature 0
- Primary outcome: mean paired shift in the ordinal recommendation (injection minus matched baseline), per model, with paired Wilcoxon signed-rank tests and cluster-bootstrap CIs; a cumulative link mixed model with paper as random effect gives per-arm odds ratios

---

## Repository Structure

```
src/
├── database.py         # SQLite schema and session helpers
├── model_interface.py  # LLM clients (Ollama / OpenAI / Anthropic / OpenRouter)
├── paper_fetcher.py    # PLOS API client with stratified fetch and CrossRef enrichment
├── injector.py         # Insertion of injection text at specified placement
├── extractor.py        # LLM-based structured outcome extraction
└── orchestrator.py     # Resume-safe experiment loop

scripts/
├── fetch_papers.py     # Populate papers table from PLOS
├── run_experiment.py   # Run review + extraction passes
├── run_psycholing.py   # Compute transformer psycholinguistic scores
├── pilot_test.py       # Smoke test: 1 paper × 2 conditions × 1 model
├── reset_db.py         # Wipe reviews/extractions (keeps papers and conditions)
├── batch_reviews.py            # GPT-5.6-luna arms via the OpenAI Batch API
├── batch_reviews_anthropic.py  # Claude Opus 5 arms via the Anthropic Batch API
├── run_openrouter_arm.py       # Qwen 3.8 27B arms via OpenRouter, one pinned host
├── extract_arm.py              # Structured extraction for one arm
├── rerun_cut_off.py            # Rerun a review that stopped mid-text (logged)
├── compute_agreement.py        # Extractor vs human coding agreement
├── export_validation_sample*.py  # Seeded, blinded validation samples
└── detect_injection.py         # Single-question detection coder (comparison only)

analysis/
├── r2_analysis.R       # Current analysis: all arms, per model (outputs to output/r2/)
├── r2_exhibits.R       # Figures for the current analysis
├── r2_validation.py    # Validation agreement (needs the human codes, not in this repository)
├── r2_thinking.py, r2_detector_check.py  # Supporting checks (need raw batch files / detector output)
├── extension_2026.R    # Analysis of the first revision (GPT-5.6-luna arms)
└── pipeline.R          # Analysis of the original submission (two models)

config/
└── config.yaml         # Models, reviewer prompts, injection templates, PLOS settings

data/
├── study.db            # SQLite database (download from Zenodo — not in git)
└── DATASET_INFO.md     # Dataset documentation
```

---

## Setup

**Prerequisites:** Python ≥ 3.11, [uv](https://docs.astral.sh/uv/), R ≥ 4.3, Ollama running locally (for Gemma 4 26B).

```bash
make install          # install Python dependencies via uv
cp .env.example .env  # add OPENAI_API_KEY (and ANTHROPIC_API_KEY, OPENROUTER_API_KEY for those arms)
ollama pull gemma4:26b
```

**R packages** (install once):
```r
install.packages(c("DBI", "RSQLite", "tidyverse", "ordinal",
                   "ggalluvial", "patchwork", "flextable", "officer"))
```

---

## Reproducing the Analysis

Download `study.db` from Zenodo and place it at `data/study.db`, then:

```bash
Rscript analysis/r2_analysis.R   # all estimates, to output/r2/
Rscript analysis/r2_exhibits.R   # figures, to output/r2/figures/
```

The bootstrap is seeded, so the estimates are reproduced exactly. `analysis/pipeline.R` reproduces the original two-model analysis.

---

## Running the Experiment

To re-run the full experiment from scratch:

```bash
make fetch       # fetch papers from PLOS API into DB
make experiment  # run all reviews + structured extraction
make psycholing  # compute transformer psycholinguistic scores
```

See `config/config.yaml` for model selection, injection templates, and reviewer prompts.

---

## Database Schema

| Table | Rows | Description |
|-------|-----:|-------------|
| `papers` | 97 | Fetched corpus (50 met inclusion criteria and were used in the experiment) |
| `conditions` | 8 | Experimental conditions (direction × placement × prompt style) |
| `reviews` | 3,200 | Raw LLM review texts |
| `extractions` | 3,200 | Structured outcomes (recommendation, detection flag, counts) |
| `psycholing` | 3,200 | Transformer psycholinguistic scores per review |

Full schema documented in `data/DATASET_INFO.md`.

---

## License

Code: [MIT](LICENSE)
Data: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)

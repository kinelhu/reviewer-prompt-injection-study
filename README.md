# Hidden Instructions in Submitted Manuscripts Bias AI-Assisted Peer Review Toward Acceptance

Code and analysis for:

> El Husseini K. *Hidden Instructions in Submitted Manuscripts Bias AI-Assisted Peer Review Toward Acceptance: A Controlled Experiment.* (under review)

**Dataset:** [Zenodo — DOI to be added upon upload]

---

## Overview

This repository contains the full experimental pipeline and statistical analysis code for a controlled study examining prompt injection attacks in AI-assisted peer review.

**Design in brief:**

- 50 published open-access biomedical articles (PLOS Medicine; 25 RCTs, 25 observational studies)
- Two reviewer LLMs: Gemma 4 26B (local) and GPT o4-mini (OpenAI API)
- One injection type: a realistic editorial-note directive targeting ACCEPT or MINOR REVISION, inserted at the beginning, middle, or end of the full manuscript text
- Two reviewer prompt styles: standard and injection-aware (with explicit detection instruction)
- 800 reviews total; outcomes extracted by a separate Gemma 4 26B instance at temperature 0
- Primary outcome: ordinal recommendation shift (injection vs. matched baseline); analysed with a cumulative link mixed model (CLMM) with paper as random effect

---

## Repository Structure

```
src/
├── database.py         # SQLite schema and session helpers
├── model_interface.py  # LLM clients (Ollama / OpenAI)
├── paper_fetcher.py    # PLOS API client with stratified fetch and CrossRef enrichment
├── injector.py         # Insertion of injection text at specified placement
├── extractor.py        # LLM-based structured outcome extraction
└── orchestrator.py     # Resume-safe experiment loop

scripts/
├── fetch_papers.py     # Populate papers table from PLOS
├── run_experiment.py   # Run review + extraction passes
├── run_psycholing.py   # Compute transformer psycholinguistic scores
├── pilot_test.py       # Smoke test: 1 paper × 2 conditions × 1 model
└── reset_db.py         # Wipe reviews/extractions (keeps papers and conditions)

analysis/
└── pipeline.R          # All figures, tables, and statistical models

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
cp .env.example .env  # add OPENAI_API_KEY
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
Rscript analysis/pipeline.R
```

This regenerates all figures (to `figures/`), tables (to `tables/`), and a `output/legends.md` with caption text.

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
| `papers` | 97 | Fetched corpus (first 50 were used in experiment) |
| `conditions` | 8 | Experimental conditions (direction × placement × prompt style) |
| `reviews` | 800 | Raw LLM review texts |
| `extractions` | 800 | Structured outcomes (recommendation, detection flag, counts) |
| `psycholing` | 800 | Transformer psycholinguistic scores per review |

Full schema documented in `data/DATASET_INFO.md`.

---

## License

Code: [MIT](LICENSE)
Data: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)

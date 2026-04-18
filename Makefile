.PHONY: fetch pilot experiment reviews extract psycholing report figures all install reset help

PYTHON = uv run python

help:                   ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*##"}; {printf "  \033[36m%-12s\033[0m %s\n", $$1, $$2}'

install:                ## Install / sync Python dependencies
	uv sync

fetch:                  ## Fetch papers from PLOS (N=n, SEARCH="term" e.g. SEARCH="lung transplant" or SEARCH="abstract:cancer")
	$(PYTHON) scripts/fetch_papers.py $(if $(N),--n $(N),) $(if $(SEARCH),--search '$(SEARCH)',)

pilot:                  ## Smoke test: 1 paper × 2 conditions × 1 model
	$(PYTHON) scripts/pilot_test.py

experiment:             ## Run experiment (LIMIT=N to cap papers, MODEL=name to filter models)
	$(PYTHON) scripts/run_experiment.py $(if $(LIMIT),--paper-limit $(LIMIT),) $(if $(MODEL),--model $(MODEL),)

reviews:                ## Run reviews only, skip extraction (safe to interrupt and resume)
	$(PYTHON) scripts/run_experiment.py --reviews-only $(if $(LIMIT),--paper-limit $(LIMIT),) $(if $(MODEL),--model $(MODEL),)

extract:                ## Re-run extraction pass (LIMIT=N, BACKEND=provider, MODEL=slug)
	$(PYTHON) scripts/run_experiment.py --extract-only \
		$(if $(LIMIT),--limit $(LIMIT),) \
		$(if $(BACKEND),--extractor-backend $(BACKEND),) \
		$(if $(MODEL),--extractor-model $(MODEL),)

psycholing:             ## Run transformer psycholinguistic analysis (SiEBERT + DeBERTa-v3)
	python scripts/run_psycholing.py

figures:                ## Generate figures, tables, and legends (requires R)
	Rscript analysis/pipeline.R

report:                 ## Generate PDF report (requires R packages)
	Rscript -e "rmarkdown::render('analysis/report.Rmd', output_dir='analysis')"

all: fetch experiment figures  ## Full pipeline: fetch → experiment → figures

reset:                  ## Wipe the database (KEEP_PAPERS=1 to preserve papers)
	$(PYTHON) scripts/reset_db.py $(if $(KEEP_PAPERS),--keep-papers,)

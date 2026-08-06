SHELL := /bin/bash
RSCRIPT ?= Rscript
SH ?= bash

EXPLORATORY_SCRIPTS := \
	code/exploratory/analyses.R \
	code/exploratory/analyses_cs_2yfu.R \
	code/exploratory/visualize_raw_correlations.R \
	code/exploratory/prealb_evolution.sh

ARTICLE_V1_SCRIPTS := \
	code/article_v1/results.R \
	code/article_v1/table_1.R \
	code/article_v1/table_2.R \
	code/article_v1/table_3.R \
	code/article_v1/suppl_table_1.R

PREPROCESS_SCRIPT_1 := code/exploratory/data_preprocessing.R

PREPROCESS_SCRIPT_2 := code/article_v2/preprocess_data.R

# Default: do not run preprocessing automatically (explicit only)
all: symlinks exploratory article_v1
	@echo "Done: all (note: preprocess_data_1 is intentionally excluded; run 'make preprocess_data_1' explicitly)"

# Symlinks target - prefers an executable script `make_symlinks` if present
symlinks:
	@./make_symlinks

exploratory: $(EXPLORATORY_SCRIPTS)
	@echo "Exploratory analyses finished."

article_v1: $(ARTICLE_V1_SCRIPTS)
	@echo "Article V1 outputs finished."

preprocess_data_1: symlinks $(PREPROCESS_SCRIPT_1)
	@echo "Data preprocessing finished."

preprocess_data_2: symlinks $(PREPROCESS_SCRIPT_2)
	@echo "Data preprocessing finished."

clean:
	@echo "Removing output/ and .cache/"
	@rm -rf -- output/ .cache/

# Rules to run R scripts and shell scripts
$(filter %.R,$(EXPLORATORY_SCRIPTS) $(ARTICLE_V1_SCRIPTS) $(PREPROCESS_SCRIPT_1) $(PREPROCESS_SCRIPT_2)):
	@echo "Running R script: $@"
	@$(RSCRIPT) "$@"

$(filter %.sh,$(EXPLORATORY_SCRIPTS) $(ARTICLE_V1_SCRIPTS) $(PREPROCESS_SCRIPT_1) $(PREPROCESS_SCRIPT_2)):
	@echo "Running shell script: $@"
	@$(SH) "$@"

help:
	@echo "Usage: make [target]"
	@echo "Targets:"
	@echo "  all                 - runs symlinks, exploratory, and article_v1 (does NOT run preprocess_data_1)"
	@echo "  symlinks            - create repository symlinks (expects ./make_symlinks executable)"
	@echo "  exploratory         - run exploratory analysis scripts"
	@echo "  article_v1          - run article_v1 scripts"
	@echo "  preprocess_data_1   - run data preprocessing (must be invoked explicitly)"
	@echo "  preprocess_data_2   - run data preprocessing (new data, must be invoked explicitly)"
	@echo "  clean               - remove output/ and .cache/"
	@echo "  help                - show this help"

.PHONY: all symlinks exploratory article_v1 preprocess_data_1 preprocess_data_2 clean help $(EXPLORATORY_SCRIPTS) $(ARTICLE_V1_SCRIPTS) $(PREPROCESS_SCRIPT_1) $(PREPROCESS_SCRIPT_2)

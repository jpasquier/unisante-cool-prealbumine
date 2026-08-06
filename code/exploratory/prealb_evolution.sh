#!/bin/bash

# Determine project root directory
ROOT_DIR=$(git rev-parse --show-toplevel)

# Output directory
OUTPUT_DIR="${ROOT_DIR}/output/exploratory"
mkdir -p "${OUTPUT_DIR}"

# Path of the quarto script
SCRIPT_PATH="${ROOT_DIR}/code/exploratory/prealb_evolution.qmd"

# Render the Quarto document
quarto render "${SCRIPT_PATH}" --output-dir "${OUTPUT_DIR}"

# Remove unwanted artifacts
if [ -f "${OUTPUT_DIR}/img/ethz_nexus_logo.svg" ]; then
    rm "${OUTPUT_DIR}/img/ethz_nexus_logo.svg"
    if [ -z "$( ls -A "${OUTPUT_DIR}/img" )" ]; then
        rmdir "${OUTPUT_DIR}/img"
    fi
fi

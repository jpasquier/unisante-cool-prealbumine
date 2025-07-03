#!/bin/bash

# Determine script directory
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Render the Quarto document
quarto render "${SCRIPT_DIR}/prealb_evolution.qmd" --output-dir "../results"

# Rename the output file
OUTPUT_DIR="${SCRIPT_DIR}/../results"
ORIGINAL_FILE="${OUTPUT_DIR}/prealb_evolution.html"
NEW_FILE="${OUTPUT_DIR}/prealb_evolution_$(date +%Y%m%d).html"

[[ -f "${ORIGINAL_FILE}" ]] \
    && mv "${ORIGINAL_FILE}" "${NEW_FILE}" \
    || echo "File not found: ${ORIGINAL_FILE}"

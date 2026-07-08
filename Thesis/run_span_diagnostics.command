#!/bin/bash
cd "$(dirname "$0")"
echo "Running span_selection_diagnostics.R from $(pwd)"
Rscript src/rscripts/span_selection_diagnostics.R
echo ""
echo "Done. Check data/span_diagnostics/ for results."
read -p "Press Enter to close..."

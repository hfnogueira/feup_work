# Thesis Build Tools

All scripts used to generate and merge the thesis chapters.

## Requirements
```
pip install python-docx docxcompose matplotlib statsmodels
npm install -g docx
```

## Key scripts

### merge_thesis.py  ← THE MAIN ONE
Merges all chapter .docx files into thesis_complete.docx.
Run from the Thesis/ folder:
  python3 tools/merge_thesis.py

### Figure generation (Python)
- build_appendix_b_eda.js — EDA figures
- Figure PNGs are generated inline in the Python scripts that call matplotlib

### Chapter builders (Node.js / docx-js)
Each build_ch*.js file generates one chapter .docx.
Run from the Thesis/ folder:
  node tools/build_ch3_algorithms.js

### Chapter order in merge (as of June 2026)
Abstract → Introduction → Literature Review (3 files) →
Methodology (6 files) → Rule Extraction (2) → Prediction (3) →
Discussion (2) → Conclusions → Figures → References →
Appendix A → B → C → D


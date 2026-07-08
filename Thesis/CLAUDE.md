# Thesis Project Briefing

## What this thesis is

**Title:** Wine Production Forecasting Using Rule-Based Machine Learning: A Distributional Subgroup Discovery Approach for Portuguese Wine Regions

**Author:** Hugo Filipe Queiróz Nogueira  
**Programme:** Master in Informatics and Computing Engineering (MECD), FEUP  
**Supervisors:** Prof. João Paulo Moreira (FEUP), Prof. Mário Campos Cunha (FCUP)  
**Date:** June 2026

**Core idea:** Apply CarenR (a distributional subgroup discovery algorithm based on Kolmogorov–Smirnov tests) to discover interpretable climate rules that explain annual wine production anomalies in two Portuguese wine regions — Douro (RDD, ~89 obs, 1934–2022) and Vinho Verde (RVV, ~80 obs, 1942–2021). Compare against RIPPER and M5Rules baselines in a walk-forward validation framework.

---

## LaTeX project location

```
/Users/hugonogueira/git_hugo/feup/Thesis/latex/
```

**Build command** (run from the `latex/` folder):
```bash
pdflatex -interaction=nonstopmode main.tex
bibtex main
pdflatex -interaction=nonstopmode main.tex
pdflatex -interaction=nonstopmode main.tex
```

The `feupteses.sty` supervisor command causes a known non-fatal "Missing \begin{document}" warning — this is expected and the PDF compiles correctly despite it.

**Output:** `main.pdf` — currently 132 pages, 2.1 MB.

---

## File structure

```
latex/
├── main.tex                    # Master file: metadata, packages, includes
├── feupteses.sty               # Official FEUP thesis style (do not modify)
├── lineno.sty                  # Line numbering (auxiliary)
├── references.bib              # Full bibliography (~23 entries)
├── *.bst                       # Bibliography style files
├── figures/                    # All figures (15 PNG files + logo PDF)
│   ├── fig_extra_1.png         # Ch3: experiment pipeline diagram
│   ├── fig_extra_2.png         # Ch3: walk-forward validation diagram
│   ├── fig_extra_3.png         # Ch3: CarenR algorithm flowchart
│   ├── fig_extra_4.png         # Ch3: CarenR variant taxonomy
│   ├── fig51–56_*.png          # Ch5: prediction results figures
│   ├── fig_context_ts_*.png    # Context: production time series
│   ├── fig_detrend_*.png       # Context: detrending comparison
│   └── fig_appd_1.png          # Appendix D: feature coverage map
└── chapters/
    ├── honourdeclaration.tex   # Declaration of Honour
    ├── ch0_abstract.tex        # Abstract (EN + PT)
    ├── ch1_intro.tex           # Chapter 1: Introduction
    ├── ch2_literature.tex      # Chapter 2: Literature Review
    ├── ch3_methods.tex         # Chapter 3: Data and Methodology  ← longest, most complex
    ├── ch4_rules.tex           # Chapter 4: Rule Discovery Results (Goal 1)
    ├── ch5_prediction.tex      # Chapter 5: Predictive Validation (Goal 2)
    ├── ch6_discussion.tex      # Chapter 6: Discussion
    ├── ch7_conclusions.tex     # Chapter 7: Conclusions
    ├── app_a_rules.tex         # Appendix A: Full rule tables
    ├── app_b_eda.tex           # Appendix B: EDA and stationarity
    ├── app_c_features.tex      # Appendix C: Feature correlations
    ├── app_d_feature_map.tex   # Appendix D: Feature coverage map
    └── app_e_ai_transparency.tex  # Appendix E: AI use statement
```

---

## Chapter structure summary

| Chapter | Content | ~Words |
|---------|---------|--------|
| Ch1 | Introduction: motivation, research questions, thesis structure | 900 |
| Ch2 | Literature: wine regions, vine physiology, ML approaches, subgroup discovery, CarenR | 3900 |
| Ch3 | Methods: datasets, feature engineering, detrending, CarenR/RIPPER/M5Rules, walk-forward protocol | 8000 |
| Ch4 | Rule discovery results: top rules per region, LOESS validation, feature frequency analysis | 5300 |
| Ch5 | Predictive validation: walk-forward MAE results, era composition, feature selector comparison | 4400 |
| Ch6 | Discussion: asymmetry between discovery and prediction, structural covariates, prior work | 3000 |
| Ch7 | Conclusions: findings, contributions, limitations, future work | 950 |

---

## Formatting conventions established

### Tables
- All tables use `tabularx` + `\small` + `\begin{adjustbox}{max width=\textwidth}`
- Text columns use `X` (auto-fill) or `p{Xcm}` (fixed width) — never bare `l` for long text
- Header row: `\textbf{}` or `>{\bfseries}` column spec
- Row spacing: global `\arraystretch{1.3}` in main.tex; locally reduced with `\renewcommand{\arraystretch}{1.0}` where needed
- Appendix tables: `\footnotesize` instead of `\small`
- No ✓ symbols in tables except where they distinguish rows (not all-present)

### Figures
- All figures: `\begin{figure}[htbp]`, `\includegraphics[width=\textwidth]` or `width=0.85\textwidth`
- Captions: short one-line title only — detail belongs in surrounding text, not the caption
- Label convention: `fig:ch3_carenr_flow`, `fig:ch3_walkforward`, etc.

### Math
- All equations in `\begin{equation}...\end{equation}` environments with `\label{eq:name}`
- Key equations: KS statistic (`eq:ks`), weighted MAE (`eq:wmae`), structural detrend (`eq:structural_detrend`), FOIL gain (`eq:foil`), SDR (`eq:sdr`)
- Inline math uses `$...$`

### Citations
- Uses `natbib` via feupteses: `\citet{Key}` for author-year in text, `\citep{Key}` for parenthetical
- All citation commands need a single backslash — `\\citet` (double backslash) is a known past bug that was fixed

### General
- No bullet points in body text (academic prose style)
- Section numbers are automatic — never hard-code "Section 3.2" when `Section~\ref{...}` is available
- `\texttt{}` for code/variable names inline (e.g., `\texttt{CarenR\_Dist\_Eta2}`)

---

## Key domain concepts

- **CarenR:** Distributional rule learner using KS test. Finds climate conditions where production *distribution* shifts. Output: IF conditions THEN production is X mhl above/below trend (support Y%, p-value Z).
- **RIPPER:** Classification rule learner (JRip/Weka). Requires discretised target (Low/Med/High tertiles). Comparison baseline.
- **M5Rules:** Piecewise linear regression (Weka). Collapses to global model on small datasets.
- **Walk-forward validation:** Expanding window, 18 scenarios (RDD) / 16 scenarios (RVV). All preprocessing within fold.
- **Detrending:** Linear OLS trend removed before modelling. Models predict residuals, trend re-added for final forecast.
- **Naive_Median:** Predicts training median residual (≈0). Minimises MAE among constant predictors. Key baseline.
- **Two-decision decomposition:** Decision 1 = trend accuracy (Naive_Median MAE); Decision 2 = climate signal (gap between model MAE and Naive_Median MAE).
- **Era confound:** Training/test sets from different production eras (pre/post-1990 RDD; pre/post-1986 RVV) causes spurious rules. CarenR partially addresses this; RVV is more severely affected.
- **Feature naming:** `{variable}_{stage}_{year}_{position}{width}` e.g. `swa_hv_y1_b20` = soil water availability, harvest, current year, 20 days before.

---

## What has been done (history)

1. Full LaTeX project created from scratch from ~30 source .docx files
2. FEUP `feupteses.sty` template applied correctly
3. All tables converted from `\resizebox` to `tabularx` + `adjustbox`
4. All math formulas converted to proper LaTeX equations
5. Multi-pass build set up; LOF/LOT populated
6. Co-supervisor (Prof. Mário Campos Cunha) added to title page and declaration
7. All citations fixed (double-backslash bug `\\citet` → `\citet`)
8. Overleaf zip created: `thesis_overleaf.zip` in the Thesis root folder
9. New content merged from 5 additional docx files: pipeline data detail, technical figures (fig_extra_1–4), formal equations (FOIL, SDR), ACF section, vine cycle subsection
10. Caption lengths shortened to one-line titles throughout

---

## Pending / things to review

- **ch1 (Introduction):** Relatively short (~900 words). May need expanding — research questions, thesis structure overview, contributions summary.
- **ch7 (Conclusions):** Short (~950 words). Could be strengthened with more specific quantitative findings.
- **Figure references:** Some figures (especially the context time series and detrending figs) are placed directly in `main.tex` between chapter includes rather than inside their respective chapter files. This works but is unusual.
- **Cross-references:** Many section references are written as plain text ("Section 3.3", "Table 4.1") rather than `\ref{}` commands — these won't auto-update if sections move.
- **Abstract:** Currently ~556 words. Check against FEUP word limit.
- **Appendix A (rules tables):** Contains the full CarenR/RIPPER/M5Rules rule tables — worth checking formatting and completeness.

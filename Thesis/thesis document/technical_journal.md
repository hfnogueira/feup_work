# Technical Implementation Journal
*Running notes on pipeline decisions, fixes, and gotchas*

---

## Session — Detrending + Pipeline Consistency

### What was done

Detrending was added to all three R scripts so that every analysis operates on
the same target definition. Previously only script 3 (descriptive distribution
rules) detrended. Scripts 4, 6, and 7 were updated.

---

### Script map (current state)

| Script | Purpose | Target | Validation | Detrend |
|--------|---------|--------|------------|---------|
| `3_carenR__application.R` | Distribution rules (descriptive) | Residuals | Full dataset | ✅ |
| `4_ripper_pipeline.R` | RIPPER classification | Residuals → Low/Med/High | 10-fold stratified CV | ✅ |
| `6_walkforward_validation.R` | Continuous prediction, 5 models | Residuals → back to mhl | Walk-forward temporal | ✅ |
| `7_caren_classification_pipeline.R` | carenR classification vs RIPPER | Residuals → Low/Med/High | 10-fold stratified CV | ✅ |

---

### Detrending logic — three variants

**Script 3 (descriptive):**
```r
fit_full    <- lm(Wine_mhl ~ year, data = df)
wine_values <- residuals(fit_full)   # full dataset, no leakage concern
```

**Scripts 4 & 7 (k-fold CV):**
```r
# Inside CV loop — trend fitted on training fold only
trend_fit      <- lm(Wine_mhl ~ year, data = cont_train)
resid_train    <- cont_train$Wine_mhl - predict(trend_fit, cont_train)
resid_test     <- cont_test$Wine_mhl  - predict(trend_fit, cont_test)  # train trend applied
# Quantile breaks computed from resid_train only → no test leakage
```

**Script 6 (walk-forward):**
```r
# Inside walk-forward loop — trend fitted on expanding training window only
trend_fit           <- lm(Wine_mhl ~ year, data = train_data)
trend_offset        <- predict(trend_fit, newdata = test_obs)  # trend at test year T
train_data$Wine_mhl <- residuals(trend_fit)
test_obs$Wine_mhl   <- test_obs$Wine_mhl - trend_offset
# All models predict residuals; add trend_offset back before storing results
results$model[i]    <- raw_prediction + trend_offset  # back to mhl units
```

---

### carenR classification pipeline (script 7) — key design decisions

- **`caren_min_sup = 0.2`** — rules must cover ≥ 20% of training data. With ~72 training obs per fold this means ≥ 14 obs per rule. If folds yield "No rules found", try 0.15.
- **`caren_min_conf = 0.5`** — rule must be correct ≥ 50% of the time.
- **`prm=TRUE`** in CV loop — writes `RulesTemp.prm` to working directory. File is overwritten each fold but `predict.caren()` is called immediately after, so no race condition.
- **Majority class fallback** — if `predict.caren()` fails or returns NULL on a fold (no rules fired on test obs), the training majority class is assigned. This is logged with `[!]` in the console.
- **Label stripping** — carenR sometimes returns predictions as `"Wine_class=High"`. Strip with `sub('^Wine_class=', '', ...)`.

---

### RIPPER pipeline (script 4) — detrend update

Previously used raw `Wine_mhl` for discretization. Now:
```r
detrend <- TRUE   # ← must match script 7
if (detrend) {
  year_num    <- as.numeric(as.character(df$year))
  trend_fit   <- lm(Wine_mhl ~ year_num, data = df)
  wine_values <- residuals(trend_fit)
} else {
  wine_values <- df$Wine_mhl
}
# quantile breaks and cut() applied to wine_values in both cases
```
RIPPER uses full-dataset detrending (not fold-level) because `evaluate_Weka_classifier`
handles CV internally. This is a minor inconsistency vs script 7 — flag in thesis if needed.

---

### Walk-forward (script 6) — CarenR lookahead caveat

The CarenR rules in script 6 are **pre-discovered on the full dataset** (loaded from
`RulesTemp_rdd/rvv.csv`). Only the subgroup **means and support weights** are
re-estimated from training data at each fold. This is a minor lookahead — the rule
*conditions* (which climate variables matter and at what thresholds) were seen on
future data. Acknowledge this as a limitation in the thesis. The alternative would
be to re-run `caren()` at every fold, which is computationally expensive.

---

### Working directory

All scripts assume working directory = `Thesis/` (set automatically by `distribution_rules.Rproj`).

- Data files: `data/`
- carenR temp files write to: `Thesis/` (RulesTemp.prm, caren_temp.bas, etc.)
- Walk-forward rules files: `Thesis/RulesTemp_rdd.csv` and `Thesis/RulesTemp_rvv.csv`
- Output files: `data/`

**Do not use `setwd()` manually** — open via `.Rproj` and paths resolve correctly.

---

### Config flags — keep these in sync across scripts

| Flag | Script 3 | Script 4 | Script 6 | Script 7 |
|------|----------|----------|----------|----------|
| `detrend` | `TRUE` | `TRUE` | `TRUE` | `TRUE` |
| `n_classes` | — | `3` | `3` | `3` |
| `n_folds` | — | `10` | — | `10` |
| `seed` | — | `42` | `42` | `42` |
| `min.sup` | `0.2` | — | — | `0.2` |

---

---

## Session — LOESS Span Selection Diagnostics (2026-06-11)

### Why span=0.40 was chosen

A sensitivity analysis (`span_selection_diagnostics.R`) tested spans 0.20–0.90 in steps of 0.05 across three criteria:

1. **Stationarity (ADF):** Every span passes for both regions (p ≤ 0.01). Not a discriminating criterion — any span achieves stationarity.
2. **Correlation preservation:** Key features tracked at each span to distinguish genuine within-era signal from era artefacts.
3. **Naive_Median MAE:** RDD spans 0.35–0.50 are closest to the linear baseline. All LOESS spans give higher Naive MAE than linear for RVV.

**Span = 0.40 justified as:** narrow enough to remove the era-level structural shift (~40–50 years of the 80–90 year series) while wider than inter-annual noise. The qualitative findings are robust across all tested spans.

### Key findings from the span analysis

**RDD — features with genuine within-era signal:**
- `swa_hv_y1_b20` (harvest soil water): Linear |r|=0.33, LOESS |r|=0.41, **125% retained** — increases under LOESS, confirmed genuine
- `days.rf.above.1mm_fl_y1_c15` (wet days post-flowering): Linear |r|=0.28, LOESS |r|=0.28, **103% retained** — genuine
- `tm_hv_y1_c10` (harvest temperature): Linear |r|=0.17, LOESS |r|=0.17, **105% retained** — preserved (has within-era signal alongside warming trend)

**RVV — era-driven vs genuine:**
- `iaf_hv_y1_c10` (LAI at harvest): Linear |r|=0.22, LOESS |r|=0.03, **15% retained** — era-driven, collapses
- `swa_hv_y1_b20` (harvest soil water): Linear |r|=0.17, LOESS |r|=0.03, **16% retained** — era-driven, collapses
- `days.rf.above.1mm_fl_y1_c15` (wet days post-flowering): Linear |r|=0.34, LOESS |r|=0.32, **94% retained** — **genuine within-era signal** (only robust RVV feature)

### What this means

The previous thesis text (ch4) incorrectly stated RVV iaf and swa were "genuine" at ~90% retention. The actual values are 15–16%. These were updated in ch4 Section "LOESS Validation". The RVV result is not an artefact of span choice — the collapse to <20% holds across all tested spans (0.20–0.90), confirming that RVV feature-response relationships are structurally dominated by era co-variation.

### Output files
- `src/rscripts/span_selection_diagnostics.R` — diagnostic script
- `data/span_diagnostics/` — CSVs and plots

---

## Session — KPSS Stationarity Table Investigation (2026-06-10)

### Finding: identical KPSS statistics for raw vs detrended are correct

Table 3.7 shows the same KPSS statistic for both raw and detrended series in each region (RDD: 0.134, RVV: 0.332). This was initially suspected to be a copy-paste error, but it is mathematically correct.

**Why:** `KPSS(null="Trend")` on the raw series internally fits and removes a linear trend before computing the statistic. `KPSS(null="Level")` on the linearly detrended residuals removes just the mean (≈0). Both variants end up operating on the same residuals, so the statistic is identical. The p-values differ because `null="Trend"` and `null="Level"` use different critical value tables.

This was verified computationally — a Python reimplementation of the KPSS statistic on the actual data confirmed the identical statistics are expected.

### RVV partial stationarity — correct interpretation

The table and thesis text are correct:
- RVV detrended residuals: ADF p=0.372 (fails, unit root not rejected) + KPSS p=0.100 (passes, level stationary)
- Conclusion: **partial stationarity only** — linear detrending removes the long-run level but not all persistent structure
- Thesis Implication 2 correctly states this; no false claim of full stationarity is made

The ADF failure for RVV detrended has a domain explanation: the sharp non-linear production decline associated with EU accession (~1986) is not fully captured by a linear trend, leaving residual non-stationarity. This formally underpins the era-confound effects seen in walk-forward results.

---

### Run order

1. `3_carenR__application.R` — generates distribution rules, produces `RulesTemp2.csv`
   - Also save `RulesTemp.csv` as `RulesTemp_rdd.csv` / `RulesTemp_rvv.csv` for script 6
2. `4_ripper_pipeline.R` — RIPPER classification CV
3. `7_caren_classification_pipeline.R` — carenR classification CV (compare output with script 4)
4. `6_walkforward_validation.R` — temporal walk-forward (requires rules files from step 1)

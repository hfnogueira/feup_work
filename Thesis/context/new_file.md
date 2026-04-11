# Project State

## Goal
Predict annual wine production for two Portuguese wine regions (RDD – Douro, RVV – Vinho Verde) using interpretable ML models. The core research question is whether **climate-phenological rules** can effectively explain and predict production variation, while remaining human-readable.

## Current Approach

**Models:** 5 methods in comparison — CarenR (distribution rules), RIPPER (classification rules), M5Rules (regression rules), Decision Tree (rpart), and a Naive baseline (training mean).

**Features:** 60 engineered features combining daily meteorological data with phenological windows (bud break, flowering, harvest, etc.) across current and previous year. Examples: mean temperature 15 days around flowering, total rainfall before harvest.

**Pipeline:** 4-stage R script pipeline: raw data prep → feature engineering → per-model training/CV → walk-forward temporal validation (expanding window, min 25 years before first prediction).

## Data

**Source:** Daily weather station records + IVDP/CVR annual production figures for RDD (1933–2022) and RVV (1941–2022).

**Granularity:** Daily meteorological inputs aggregated to annual features; annual production as target.

**Target variable:** `Wine_mhl` — annual wine production in million hectoliters (RDD ~520–992, RVV ~494–3443).

## Open Problems

- **Severe overfitting:** M5Rules shows R² ≈ 0.40–0.53 in training but **negative R²** in CV, driven by the small sample vs. high feature count (~80–89 obs, 60 features).
- **RIPPER accuracy is low:** 37.5% (RDD) and 46.25% (RVV) on a 3-class problem — barely above random.
- **CarenR walk-forward results unclear:** Output files not consistently saved; some lookahead bias risk in rule discovery.
- **No feature selection applied** despite a `Feature_selection.xlsx` existing — dimensionality reduction is an obvious next step.
- **RVV missing Fruit Set phenology** entirely; 2022 excluded.

## Decisions Taken

- RIPPER discretization: equal-frequency tertiles (Low/Medium/High) for class balance; params N=4, O=3, F=3 to reduce overfitting on small data.
- CarenR prediction strategy C (support-weighted average of matching rules) chosen as primary.
- Detrending disabled by default (`detrend <- FALSE`); available as option.
- Walk-forward minimum training window: 25 years.
- No normalization applied to continuous features; no outlier removal.

## Code Structure

```
src/rscripts/
  1_data_preparation_stage_1.R   ← raw loading, binary indicators
  2_features_creation.R          ← 60-feature engineering (1,136 lines)
  3_carenR_application.R         ← CarenR rule discovery
  4_ripper_pipeline.R            ← RIPPER 10-fold CV
  5_m5rules_pipeline.R           ← M5Rules 10-fold CV
  6_walkforward_validation.R     ← all methods, temporal validation

notebooks/                       ← EDA (Rmd + HTML) for production & meteo data
data/                            ← processed CSVs, model output txts, feature Excel
CarenR/                          ← custom R package source
distribution_rules/              ← CarenR rule CSVs per walk-forward fold
thesis document/                 ← draft PDF
```
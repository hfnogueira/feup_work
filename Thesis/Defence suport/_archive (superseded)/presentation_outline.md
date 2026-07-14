# Thesis Defense — Slide-by-Slide Draft

**Wine Production Forecasting Using Rule-Based Machine Learning: A Distributional Subgroup Discovery Approach for Portuguese Wine Regions**

Hugo Filipe Queiróz Nogueira · MECD, FEUP · June 2026
Supervisors: Prof. João Paulo Moreira (FEUP), Prof. Mário Campos Cunha (FCUP)

*Target: 20 min · ~18 slides · ~1 min/slide. Slide titles state the finding, not the topic. Notes marked "SAY" are speaker cues, not slide text.*

---

## 1 · Title

- Full title, name, programme, both supervisors, date.
- SAY (15 s): one line on what the work is — "interpretable climate rules for wine production in two Portuguese regions."

---

## 2 · Why this problem, why interpretable rules

- Wine production is one of Europe's most climate-sensitive activities; Douro and Vinho Verde are among Portugal's most important regions.
- The agroclimatic signal is *weak* relative to year-to-year variability — regional totals sum thousands of heterogeneous producers.
- Black-box ML (RF, SVM, LSTM) forecasts well but can't be audited by agronomists or cooperatives. SHAP approximates; it doesn't produce explicit rules.
- The gap: **no published work applies interpretable rule discovery to long regional wine-production records.**
- SAY: frame the payoff — a rule reads like "if soil water at harvest is low and post-flowering is dry, production is X mhl above trend, support Y%."

---

## 3 · Two goals, four research questions

- **Goal 1 — Rule Discovery:** find interpretable agroclimatic rules for above/below-trend years. Primary tool: CarenR.
- **Goal 2 — Predictive Validation:** can rules beat a naive forecast in walk-forward validation?
- RQ1 which variables distinguish above/below-trend years · RQ2 genuine climate effect or era confound · RQ3 does any rule model beat Naive_Median · RQ4 if not, when *does* it succeed and why.
- SAY: flag the tension up front — discovery and prediction pull in opposite directions. This is the story of the talk.

---

## 4 · The data: two contrasting regions

- **RDD (Douro):** 89 years, 1934–2022. **RVV (Vinho Verde):** 80 years, 1942–2021.
- Annual regional production in thousands of hectolitres (mhl). No sub-regional / quality data.
- FIGURE: production time series for both regions (`fig_context_ts_*`) — show the structural trend and era shifts (pre/post-1990 RDD, pre/post-1986 RVV).
- SAY: both series are strongly non-stationary and driven by non-climate factors (EU policy, industry restructuring). This is the core difficulty.

---

## 5 · Feature engineering: climate through the vine cycle

- Regional climate indices + a vine phenological model → features tied to growth stages (budburst, flowering, maturity, harvest).
- Naming: `{variable}_{stage}_{year}_{position}{width}` — e.g. `swa_hv_y1_b20` = soil water, harvest, current year, 20-day window before.
- Variables: soil water availability, temperature, wet days, heat-stress days (Tmax > 35 °C), cold days, LAI.
- SAY: keep brief — one worked example of the naming decodes the rest.

---

## 6 · Detrending and the two-decision framework

- Linear OLS trend removed; models predict **residuals**, trend re-added for the forecast.
- FIGURE: detrending comparison (`fig_detrend_*`).
- **Two-decision decomposition** (a core contribution): Decision 1 = trend quality (Naive_Median MAE); Decision 2 = climate signal (gap between model and Naive_Median).
- SAY: plant the punchline seed — improving Decision 1 removes the residual variance Decision 2 needs. They fight each other.

---

## 7 · CarenR: distributional subgroup discovery

- Finds subgroups where the *distribution* of production residuals differs from the global distribution — measured by a Kolmogorov–Smirnov test.
- Output = IF climate conditions THEN production shifts Δ mhl above/below trend, with support % and KS p-value.
- Explicit **fallback**: if no rule fires, predict the training median (= Naive_Median). A principled "I don't know."
- FIGURE: CarenR flowchart (`fig_extra_3`). Show KS intuition visually — two shifted distributions — not the formula.

---

## 8 · Baselines and the walk-forward protocol

- Comparators: **RIPPER** (classification rules, tertile target), **M5Rules** (piecewise linear), **Naive_Median** (predicts training median residual — the key baseline), Decision Tree.
- **Walk-forward, expanding window:** 18 scenarios (RDD), 16 (RVV). All preprocessing inside each fold.
- FIGURE: walk-forward diagram (`fig_extra_2`).
- SAY: Naive_Median is deliberately hard to beat — it's the MAE-minimising constant predictor. Beating it means real climate signal.

---

## 9 · Goal 1 result — CarenR finds coherent rules in the Douro

- **48 statistically validated rules** in RDD. Harvest-period soil water appears in **54%** of them.
- Example rules (plain language):
  - Dry post-flowering (wet days 0–3.3 d) → **+121 mhl**, support 28.1%, p = 0.0043.
  - Moderate pre-harvest soil water + dry previous-year post-harvest → **+192 mhl** (largest above-trend), support 21.3%.
  - High canopy vigour (LAI) + high soil moisture at flowering → **−131 mhl** (largest below-trend), support 20.2%.
- 31 of 48 rules point above-trend — the positive signal is structurally more consistent.

---

## 10 · Goal 1 result — Vinho Verde: fewer rules, mostly below-trend

- **20 validated rules** in RVV; heat-stress days (Tmax > 35 °C) appear in up to **50%**.
- Only **one** above-trend rule: warm-but-not-extreme prior-year budburst + mild flowering → **+311 mhl** (largest deviation in either region).
- The other four top rules are below-trend cool-moist Atlantic patterns (e.g. −338, −365 mhl).
- SAY: the asymmetry itself is a finding — RVV's signal is dominated by what *depresses* production.

---

## 11 · Goal 1 validation — the signal is real, not an era artefact

- **LOESS detrending test:** top features retain >85% of their linear correlation after era-trend removal → genuine within-era signal, not era-membership proxies.
- **Cross-algorithm convergence:** RIPPER (info-gain) and M5Rules (MSE) independently select the *same* variable families as CarenR (KS). E.g. M5Rules quantifies pre-flowering warming at +66 mhl/°C.
- SAY: three algorithms with three different objective functions agreeing = strong evidence of real structure. **RQ1 answered.**

---

## 12 · Goal 2 result — no rule model reliably beats the naive baseline

- FIGURE: weighted-MAE ranking, both regions (`fig51`/`fig52`).
- RDD: best CarenR 189 vs Naive_Median **184 mhl** (+3%), Wilcoxon p = 0.468 — not significant.
- RVV: best CarenR 172 vs **140 mhl** (+22%); when rules fire they *hurt*, p = 0.004.
- Naive_Median ranks #1 in both regions. **RQ3 answered: no.**
- SAY: this is a negative result — and the thesis takes it seriously rather than burying it.

---

## 13 · Why prediction fails — the two-decision framework explains it

- Decision 1 (trend) is already near-optimal → little residual variance left.
- Decision 2 (climate signal) needs that variance to work with.
- A **time-only** detrend can't satisfy both: sharpen the trend and you starve the rules; loosen it and the era confound contaminates the signal.
- SAY: this is the intellectual core — the failure is *structural and predictable*, not a tuning problem. **RQ4 groundwork.**

---

## 14 · When rules *do* work — the era-composition finding

- FIGURE: per-scenario MAE, RDD (`fig53`/learning curve).
- In RDD, once training data reaches **~29% post-1990 share**, CarenR beats Naive_Median in **5/5** scenarios (margins 17–32 mhl), binomial p = 0.031.
- Aggregate still favours the baseline because early scenarios have the largest test sets and dominate the weighted MAE.
- **Caveat, stated plainly:** post-hoc, exploratory, n = 5 — hypothesis-generating, not confirmatory. **RQ4 answered conditionally.**

---

## 15 · The two regions diverge — RVV is confounded, RDD is recoverable

- RVV: **0/16** scenarios beat the baseline; more modern data makes it *worse* — opposite of RDD.
- Reason: RVV's structural non-stationarity (post-1986) more severely contaminates the climate signal than RDD's.
- SAY: same method, opposite outcomes — the era confound, not the climate, drives predictive success/failure. **RQ2 answered.**

---

## 16 · Discussion — the discovery / prediction asymmetry

- CarenR extracts validated, cross-confirmed, agronomically coherent patterns from 80–90 years of data.
- Those patterns don't generalise to held-out prediction because the era structure that makes them *interpretable* also *contaminates* the signal when predicting within one era.
- This is a meaningful result for the method regardless of predictive outcome — and the framework says exactly how to fix it.

---

## 17 · Conclusions & contributions

- **Goal 1 achieved; Goal 2 not (overall), but precisely characterised.**
- Contributions: (1) two-decision decomposition — transferable diagnostic; (2) 11-variant sensitivity analysis — optimal config is problem-specific (η² for strong signal / support-filtered for fallback regimes); (3) era-composition diagnostic; (4) cross-algorithm validation; (5) demanding real-world non-stationary testbed.
- Limitations: time-only detrend, few scenarios (18/16), single-region aggregates, post-hoc n = 5.
- Future work: **structural detrending** (planted area, quotas as covariates), prospective validation of the era finding, gridded climate data, EMM extension, other wine regions without a structural trend.

---

## 18 · Thank you — questions

- One-line restatement: *distributional rule discovery reliably finds real climate structure; predicting within a single era needs structural detrending to resolve the Decision 1 / Decision 2 tension.*
- "Thank you — happy to take questions."

---

## Backup slides (for Q&A — not presented linearly)

- **B1 — Full CarenR rule tables** (top-10 per region, Appendix A).
- **B2 — Feature selector comparison** (all 11 CarenR variants, η² vs support-filtered).
- **B3 — RIPPER & M5Rules detail** (CV accuracy: RDD 42% / RVV 28%; κ = 0.13 / −0.09; M5Rules coefficients).
- **B4 — Statistical tests table** (three Wilcoxon/binomial tests, confirmatory vs exploratory labelling).
- **B5 — ACF / stationarity** (Appendix B) — likely jury question on time-series assumptions.
- **B6 — LOESS vs linear detrending MAE** — in case they probe the detrending choice.
- **B7 — Feature coverage map** (`fig_appd_1`).

---

### Timing guide

| Block | Slides | Minutes |
|-------|--------|---------|
| Opening | 1–3 | 2 |
| Data & methods | 4–8 | 6 |
| Goal 1 results | 9–11 | 4 |
| Goal 2 results | 12–15 | 5 |
| Closing | 16–18 | 3 |

*Leave the last ~30 s of the results blocks as buffer — juries interrupt.*

# Supervisor Critique & Jury Attack Map

**Thesis:** Wine Production Forecasting Using Rule-Based Machine Learning — A Distributional Subgroup Discovery Approach
**Author:** Hugo Filipe Queiróz Nogueira · MECD, FEUP
**Purpose:** Demanding pre-viva critique. Weaknesses ranked by probability of being raised by the examination committee. Each item: category · the attack · your exposure · how to respond.

---

## The ranked kill-list

### 1. "Why model *total production* and not yield per hectare?"
- **Category:** Domain / methodological
- **Likelihood:** Near-certain
- **Attack:** Production = area × yield/ha. Yield/ha is the climate-sensitive quantity; total production folds in planted area, which is *structural* — the exact confound the whole thesis fights. You cite "normalised production per hectare" as analogous to your proposed fix (§6.5) and then don't use it. The jury will say you built an elaborate detrending apparatus to remove an area/structural effect you could have removed at the source by changing the target.
- **Response:** Have a crisp reason total production was the object of study (forecasting/planning need, or area-series unavailability for the full horizon). Concede it is the obvious first extension.

### 2. "You name structural-covariate detrending as THE fix — why is it entirely in 'future work'?"
- **Category:** Missing experiment
- **Likelihood:** Near-certain
- **Attack:** Eq. 6.1 is presented as the resolution to the two-decision tension; you say the data is "available in principle from IVV/IVDP." Reads as: diagnosed the disease, prescribed the cure, administered no dose. Even a partial structural detrend on post-1986 data could have turned a null thesis into a positive one.
- **Response:** Defend the scoping decision (archival effort to assemble covariates for 1934–2022 vs. thesis time budget). Acknowledge it is the single highest-value next step.

### 3. The post-hoc n=5, p=0.031 "conditional success"
- **Category:** Statistical
- **Likelihood:** Very likely
- **Attack:** Five nested scenarios, window chosen *after* seeing which won (HARKing), no multiplicity control, and the immediately following scenarios (15–18) fail. Sample size cannot support a confirmatory claim.
- **Response:** "Hypothesis-generating, full stop; I would pre-register and test it prospectively." Do not defend it as evidence.

### 4. Abstract says "90–92%" retention under LOESS; Table 4.4 shows RVV features collapse to 15–16%
- **Category:** Credibility / internal contradiction
- **Likelihood:** Likely if read closely
- **Attack:** Your own document contradicts itself. The 90–92% figure is true only for Douro (and one RVV feature); the headline generalizes it to all features/both regions, which your own Table 4.4 falsifies. If caught un-pre-empted, it poisons trust in every other number.
- **Response:** **FIX THE ABSTRACT AND CH.7 BEFORE SUBMISSION.** Raise it yourself on stage: correlations are preserved in Douro but collapse in Vinho Verde — and that collapse is a *result*, not a caveat.

### 5. "Naive-Median is a weak bar. Where is a real forecasting baseline?"
- **Category:** Missing baseline / alternative algorithms
- **Likelihood:** Likely
- **Attack:** For a time-series forecasting thesis, no ARIMA/ARIMAX, no exponential smoothing, no state-space model. Naive and Naive_MA are toys. On the ML side, no penalized regression (LASSO/elastic net) and no GAM — the natural interpretable competitors to CarenR that also handle the collinear 59 features. You never established a predictive ceiling (no RF/gradient boosting on your own data — only cited from Lopes 2024), so you can't say what interpretability costs.
- **Response:** Concede the classical-TS and penalized-regression baselines as a gap. If time permits before submission, run an ARIMAX and an elastic-net baseline — it materially strengthens the defense.

### 6. The "18 scenarios" are not independent → Wilcoxon test invalid
- **Category:** Statistical (most serious technical flaw)
- **Likelihood:** A sharp examiner will ask
- **Attack:** Scenario 1 tests 2005–2022, scenario 2 tests 2006–2022, etc. The year 2022 is in the test set of *all 18 scenarios*. Per-scenario MAEs are heavily overlapping/autocorrelated, yet the Wilcoxon signed-rank test treats the 18 paired differences as independent. Also, §3 describes predicting "the next year," but the tables predict *every remaining year* from each origin — mixing 1-step and 18-step-ahead forecasts and re-counting the same years. Compounded by weighted-MAE double-counting the early years.
- **Response:** Acknowledge the dependence; present the tests as indicative, not confirmatory. Reconcile the §3 text with the tables. Ideally recompute a clean one-step-ahead walk-forward variant.

### 7. Multiple comparisons: 17 models, uncorrected p-values
- **Category:** Statistical
- **Likelihood:** Likely from a statistician
- **Attack:** You explicitly state no correction was applied. With 17 models and multiple tests the family-wise error rate is large; combined with #3 and #6 this is a soft underbelly.
- **Response:** The headline result is a *null*, so multiplicity does not manufacture a false positive there. The exposure is the p=0.031 — treat it as exploratory (see #3).

### 8. "MAE is the metric your baseline is defined to minimize — deck-stacking?"
- **Category:** Statistical / methodological
- **Likelihood:** Medium
- **Attack:** The median provably minimizes MAE among constant predictors, and you evaluate on MAE. "Nothing beats the median on MAE" is close to baked in. Under RMSE or a quantile/pinball loss the story could differ.
- **Response:** Prepare RMSE numbers if possible. Justify MAE (robust to the production outliers), but concede the point and note the direction of the result is unlikely to flip.

### 9. Multiplicity *inside* rule mining
- **Category:** Statistical
- **Likelihood:** Medium
- **Attack:** Level-wise (Apriori-style) search over 59 features × 4 bins up to length 4 tests an enormous candidate space at KS p ≤ 0.10 (lenient) — and being exhaustive over the support-frequent region, it evaluates *more* candidates than a greedy beam would. Some of the "48 validated rules" are expected false positives; no permutation/bootstrap null accounts for the search.
- **Response:** A label-permutation test would answer "how many rules by chance?" Acknowledge it as a missing safeguard; support ≥20% partially mitigates but does not fix.

### 10. "You praise Exceptional Model Mining — why didn't you use it?"
- **Category:** Alternative algorithm
- **Likelihood:** Medium (trap you set in future work)
- **Attack:** EMM captures the interaction/regression-shift structure that KS marginal rules miss and is arguably better suited to your problem. Same for RuleFit and CORELS / Bayesian Rule Lists as modern, more principled rule learners than 1995-vintage RIPPER.
- **Response:** Frame CarenR as the deliberate focus and EMM/RuleFit as the principled next comparators; agree the comparators skew old.

### 11. Cross-algorithm convergence: robustness or shared confounding?
- **Category:** Weak argument
- **Likelihood:** Medium
- **Attack:** Three algorithms fed the same era-confounded, collinear features can converge on the same *confounded* features. Convergence is consistency, not correctness. Goal 1 also has no held-out validation — rules mined on the full series, "validated" only by in-sample KS and in-sample LOESS.
- **Response:** Position Goal 1 as *descriptive* discovery with domain face-validity, not a predictive claim. Soften "validated."

### 12. Fallback-as-virtue over-sells non-performance
- **Category:** Weak argument
- **Likelihood:** Medium-low
- **Attack:** RVV abstains 44% of the time and is harmful the other 56%, framed as the model "principledly saying I don't know." Blunt examiner: a forecaster that either abstains or hurts you is not useful.
- **Response:** Honest version: the fallback *limits the damage* from net-harmful rules. Don't oversell.

### 13. Is the two-decision decomposition genuinely novel?
- **Category:** Contribution / weak argument
- **Likelihood:** Medium-low
- **Attack:** A theory-minded examiner may say it restates known ideas — forecast-error decomposition, and the classic econometric result that aggressive detrending removes signal.
- **Response:** Claim it precisely as a *framing/diagnostic* contribution: naming it, operationalizing it to localize the bottleneck to Decision 1, and demonstrating the trade-off empirically via LOESS. Not a new theorem.

### 14. Feature explosion and collinearity
- **Category:** Methodological
- **Likelihood:** Lower
- **Attack:** 59 features from a combinatorial grid, many near-duplicates (you admit top RVV features are correlated). No collinearity handling, no dimensionality reduction; inflates rule redundancy (your 64.7% overlap). No feature-selection stability reported across folds.
- **Response:** Acknowledge; note support-filtering and the sensitivity analysis partially address stability. Selection-stability reporting is a fair ask.

### 15. RIPPER/M5Rules "CV accuracy" — what CV, and does it break your own rule?
- **Category:** Statistical inconsistency (clean gotcha)
- **Likelihood:** Lower
- **Attack:** Ch.2 argues random k-fold CV is invalid on non-stationary series; Ch.4 then reports "CV accuracy 42%" and "CV R² = −0.07." If those are random k-fold, you did the thing you condemned.
- **Response:** Clarify the CV scheme up front; if k-fold, frame those figures as in-sample descriptive only.

---

## Fix before submission (not just prepare for)
- **#4** — the 90–92% vs 15–16% contradiction. It is factually wrong as written and a credibility grenade.
- **#6** — reconcile the §3 protocol text ("next year") with the tables ("all remaining years"), and soften the causal weight placed on the Wilcoxon p-values.

## Highest-leverage answers to rehearse until reflexive
**#1 (yield/ha), #2 (undone fix), #5 (real baseline), #6 (independence).** These four are where a merely-good defense becomes shaky.

## Bottom line
Intellectually mature and unusually honest — the *diagnosis* is the real value and the framing is defensible. But three structural soft spots — modeling total production instead of yield/ha, leaving your own proposed fix undone, and a walk-forward design whose test folds aren't independent — are what a demanding jury will circle.

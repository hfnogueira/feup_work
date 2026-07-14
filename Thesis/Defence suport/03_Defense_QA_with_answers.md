# Defense Q&A — Hugo Nogueira, MECD Thesis
**Wine Production Forecasting Using Rule-Based Machine Learning**
*Prepared for FEUP Master's Defense, June 2026*

> **Note:** The Q5 answer has been corrected to "Apriori-style level-wise search" and "p ≤ 0.10", consistent with the errata (CAREN does not use beam search).

---

## PART A — 20 Difficult Jury Questions

1. CarenR finds statistically significant distributional rules, yet no rule-based model consistently beats Naive_Median in walk-forward prediction. Doesn't this mean the rules have no predictive value?
2. The era-composition finding (CarenR wins 5/5 era-balanced scenarios, p=0.031) was made post-hoc after examining results. How can you claim this is a scientific finding rather than data dredging?
3. Your Naive_Median baseline predicts the training median residual ≈ 0. Isn't this essentially just "predict the trend"? Any competent forecast already does this — why is beating it a meaningful bar?
4. Linear detrending is applied, yet RVV residuals still fail the ADF stationarity test after detrending. Doesn't this invalidate the entire modelling framework for RVV?
5. You use KS-test significance (p < 0.05) as the discovery criterion for CarenR rules. With 59 features and multiple variants, how did you control for multiple comparisons?
6. The LOESS stress test shows RVV dominant features collapse to 15–16% correlation under era-removal — yet you still present 20 CarenR rules for RVV as a legitimate result. Aren't those rules worthless?
7. M5Rules collapses to a global model on n<89 datasets. Given this, why include it as a baseline at all? What scientific purpose does a degenerate model serve?
8. You have only 18 walk-forward scenarios for RDD and 16 for RVV. These are tiny statistical samples. How can any p-value from a Wilcoxon test over 18 observations be interpreted meaningfully?
9. Your "Two-Decision Decomposition Framework" is essentially just the bias-variance or signal-noise decomposition rewritten in domain terms. What is actually novel here?
10. Cross-algorithm feature convergence (RIPPER, CarenR, M5Rules selecting the same features) could simply mean all three algorithms overfit to the same era-confounded signal. Why is convergence evidence of genuine data structure rather than shared overfitting?
11. The binomial test finding (5/5 wins in era-balanced scenarios, p=0.031) uses n=5. A binomial test with n=5 has almost no power and its Type I error at p=0.031 is meaningful only if the test was pre-specified. Was it?
12. You claim that "diagnosing failure modes is itself a scientific contribution." This is a post-hoc reframing of negative results. How do you differentiate principled diagnosis from rationalising failure?
13. Feature engineering creates 59 features from daily meteorological data with phenological anchors. Did you validate that the phenological stage dates (Budburst, Flowering, Veraison, Harvest) are consistent across the 89-year observation window? Stage timing itself shifts with climate change.
14. All preprocessing (detrending, discretisation, feature selection) is performed within-fold. Yet the CarenR variant selection (which of 11 variants to use) — was this also within-fold, or was a single best variant chosen retrospectively?
15. The coefficient of variation for production residuals is 47%. This is extremely high noise. Can you justify that any machine learning approach, however sophisticated, could reliably learn from data with this signal-to-noise ratio?
16. RVV features retain only 15–16% correlation after LOESS detrending. Yet you used these era-contaminated features to discover 20 CarenR rules. Doesn't this mean Goal 1 (rule discovery) also failed for RVV?
17. Your weighted MAE metric assigns higher weight to scenarios with larger training sets. This is a reasonable choice, but it means early scenarios — when the training set is most homogeneous — have less influence. Could this weighting be masking that CarenR performs well early but degrades over time?
18. You compare 17 models across two regions. No correction for multiple comparisons (e.g. Bonferroni, FDR) is applied to the Wilcoxon tests. How do you justify the reported p-values?
19. RIPPER requires tertile discretisation of the continuous production residual target. This discretisation is done within-fold — but the tertile boundaries shift between folds as the training set grows. Doesn't this mean RIPPER's rules are not comparable across folds?
20. The thesis recommends adding structural covariates (planted area, EU quotas, certified producers) as the primary future direction. But these covariates are themselves highly correlated with time — won't adding them recreate the same era-confound problem in a different guise?

---

## PART B — Strong Technical Answers

**Q1: Rules are significant yet prediction fails — no predictive value?**

The rules have genuine descriptive and explanatory value, but that is not the same as predictive value. The KS test requires only that a distributional shift *exists* under the conditioning set — it does not require the shift to dominate out-of-sample variance. With CV=47%, even a real association (|r|≈0.47) explains only ~22% of residual variance; 78% is unexplained noise. Distributional discovery needs the signal to *exist*; prediction needs it to *dominate*. These are fundamentally different requirements. This distinction is precisely what the Two-Decision Decomposition formalises. The rules are not valueless — they tell winemakers which climate conditions historically produce exceptional or catastrophic vintages. They just cannot reliably predict next year's figure from current-year climate alone.

**Q2: Post-hoc era-composition finding — data dredging?**

This is a legitimate concern and I address it directly in the thesis. The era-composition hypothesis was not pre-specified before running the walk-forward experiment — it was formed by examining the pattern of wins and losses. For this reason I explicitly label the finding "POST-HOC EXPLORATORY" and state it requires prospective validation. The binomial test (p=0.031) establishes that the pattern is unlikely under the null of random wins, but does not establish it as a confirmed scientific law. The contribution is to *identify and name* the conditional regime — not to claim confirmatory evidence. Future work should pre-register the hypothesis and test it on 2023+ data for RDD as they become available.

**Q3: Naive_Median is just "predict the trend" — is beating it meaningful?**

Naive_Median is more specific and more demanding than simply "predict the trend." It predicts the training median *residual*, which is typically close to zero but not exactly zero, and is the optimal constant predictor under MAE loss. Beating it means extracting genuine above-baseline climate signal beyond what the linear trend already captures. Any model that cannot beat Naive_Median is adding noise, not signal. The question whether any model beats this bar is precisely the right scientific question: given weak climate signal and high noise, is there any learnable structure above trend? The answer for RDD (borderline, conditionally yes) and RVV (no, rules are harmful) is itself an informative result.

**Q4: RVV residuals still fail ADF after detrending — framework invalidated?**

Failing ADF after linear detrending means the series has residual non-stationarity — likely a structural mean shift around 1986 (EU wine quotas) that linear OLS cannot remove. This does not invalidate the framework; it *explains* the RVV results. The non-stationarity is the mechanistic reason why era confound is more severe in RVV than RDD. The framework is designed to operate under these conditions and to diagnose precisely when it is overwhelmed. The LOESS stress test quantifies the degree of contamination. A framework that identifies when and why it fails is more scientifically valuable than one that ignores the problem.

**Q5: Multiple comparisons with 59 features?**

CarenR applies the KS test rule-by-rule, but the search is not an independent 59-test multiple comparison problem. CarenR builds conjunctive rules using an Apriori-style level-wise search pruned by minimum support; it does not test all 59 features independently. The significance threshold (p ≤ 0.10) is applied to the *final induced rule* as a post-induction filter, not to each candidate feature independently. Additionally, the support threshold (minimum 20% coverage) substantially reduces the effective search space, since the level-wise search only ever expands support-frequent itemsets. Nonetheless, I acknowledge that multiple rules from the same dataset are not independent, and report all rules with their raw p-values rather than adjusted ones. A Bonferroni-style correction would be conservative given the support-pruned level-wise search structure, but applying FDR correction (or a label-permutation null) to the rule set is a valid future methodological extension.

**Q6: RVV rules are era-contaminated — why present them?**

Presenting the 20 RVV rules with the LOESS validation that exposes their contamination is itself the scientific contribution. The rules are not presented as reliable climate signal; they are presented alongside the evidence that collapses them — which demonstrates the LOESS protocol's diagnostic power. A researcher applying CarenR naively to RVV data would report 20 rules and conclude success. This thesis shows why that conclusion would be wrong. The rules serve as the object of the diagnostic experiment, not as validated findings in isolation.

**Q7: Why include M5Rules if it degenerates?**

Including M5Rules and documenting its collapse on n<89 provides an important practical benchmark. It shows that standard regression tree rule induction degenerates on small agricultural datasets and collapses to a global model — a finding of direct relevance to practitioners. Demonstrating that M5Rules CV R²=0.004 to −0.07 confirms the problem is not solvable by arbitrary regression rule learners and justifies the choice of a distributional approach. A negative result with a well-understood algorithm is more informative than its omission.

**Q8: Only 18 walk-forward scenarios — Wilcoxon interpretation?**

Wilcoxon signed-rank tests on 18 pairs are indeed low-power. The test can detect consistent directional differences but has limited ability to distinguish modest effect sizes. I report exact p-values and acknowledge this limitation explicitly. The RDD overall result (p=0.468, not significant) correctly reflects that the evidence is insufficient to confirm CarenR's advantage globally. The confirmatory negative result for RVV (p=0.004) is well-powered precisely because the effect is large and consistent. The limitation cuts both ways: it means I cannot claim CarenR wins (for RDD overall), but also means when I find no significant advantage, I am not incorrectly concluding failure on insufficient data.

**Q9: Two-Decision Framework is just bias-variance rewritten?**

The bias-variance decomposition applies to expected squared error and decomposes model error into bias, variance, and irreducible noise — a property of the estimator. The Two-Decision Framework is different: it applies to MAE in a walk-forward context and decomposes *total forecast error* into a domain-interpretable trend decision (affected by structural regime shifts, not model variance) and a residual signal decision (affected by climate signal strength and era contamination). The framework attributes failure to *causal mechanisms* — structural non-stationarity vs. weak signal — rather than to estimator properties. This framing directly generates actionable research directions (structural covariates fix Decision 1; longer within-era series fix Decision 2) in a way that bias-variance does not.

**Q10: Cross-algorithm convergence could be shared overfitting?**

If all three algorithms were overfitting to the same era signal, they should converge on *era-specific temporal features* (e.g., year index, features that monotonically trend) rather than on physiologically meaningful features (harvest soil water, post-flowering wet days). The features they converge on — soil water availability at harvest, heat stress days around maturity — are the features with strongest agronomic basis for explaining within-era production variation. The LOESS stress test confirms these features in RDD retain 90–125% of their correlation after era removal. In RVV, they do not — and the convergence in RVV is weaker. So the convergence evidence is strongest precisely where the LOESS test independently confirms genuine signal, and weakest where it doesn't. That corroboration across independent tests is the key argument.

**Q11: Binomial test n=5, was it pre-specified?**

No, it was not pre-specified — the era-composition hypothesis emerged from examining the results. I am explicit about this. The p=0.031 finding is described as exploratory, not confirmatory. With n=5, the binomial test can only detect effects with probability ≥ ~0.97 at α=0.05 (all 5 successes). The observed pattern (5/5) is the most extreme possible result — it means CarenR won *every single* era-balanced scenario. The statistical test is almost secondary to the categorical observation. That said, I fully accept that one prospective test on new data is required before this can be reported as a confirmed finding.

**Q12: Diagnosing failure as contribution — post-hoc rationalisation?**

The diagnosis is pre-hoc in structure even if post-hoc in discovery. The Two-Decision Decomposition was designed as a general framework applicable to any forecasting problem with trend + residual decomposition, and I apply it to explain both the success (RDD era-balanced) and failure (RVV overall) cases using the same analytical machinery. Rationalisation would be inventing an explanation *specific to the negative result* that could not have been falsified. The Two-Decision Framework is falsifiable: if structural covariates improve Decision 1 in RVV and rules then become competitive, the framework is confirmed. If Decision 1 improves but rules still fail, the framework predicts Decision 2 must be the bottleneck — a testable prediction. This is diagnosis, not rationalisation.

**Q13: Phenological stage consistency over 89 years?**

This is a genuine limitation. The thesis uses fixed phenological anchors (day-of-year for Budburst, Flowering, Veraison, Harvest) estimated from aggregate regional data. Climate change has shifted vine phenology — harvest in Douro now occurs ~2–3 weeks earlier than in 1934. This means "harvest soil water 20 days before" in 1934 and 2020 does not refer to the same meteorological period. The features are computed relative to fixed calendar windows rather than true phenological dates. Ideally, observed phenological dates per year and region would be used. This data was not available for the full 89-year window. I acknowledge this as a feature-quality limitation that could explain some noise in the signal.

**Q14: Was variant selection within-fold or retrospective?**

CarenR variant selection was done by comparing overall walk-forward performance across the 11 variants after the full experiment — it was retrospective, not within-fold. This is a genuine limitation. Treating variant selection as within-fold would be the fully rigorous approach but would require a nested cross-validation structure that is computationally intensive and statistically challenging with only 18/16 scenarios. The current approach risks selecting a variant that happens to fit the specific walk-forward window available. I present results for all 11 variants (not just the best), which partially mitigates this — the jury can inspect whether the reported best variant is an outlier or part of a consistent pattern.

**Q15: CV=47%, too noisy for any ML to work?**

Theoretically, with CV=47% and |r|≈0.47 (climate features explaining ~22% of residual variance), the maximum achievable out-of-sample R² from climate alone is approximately 0.22 under ideal conditions — and walk-forward validation with small training sets degrades this further. The question is not whether 22% is large, but whether it is learnable and consistent. The RDD era-balanced finding suggests that with sufficient within-era training data, a small consistent advantage is learnable. The thesis does not claim to solve the forecasting problem — it establishes *when and why* it is solvable, which requires characterising the limits. The honest answer is: 47% CV is near or at the threshold of what distributional methods can reliably extract. That is a finding, not a failure.

**Q16: Goal 1 (rule discovery) also failed for RVV?**

This depends on what "failure" means. If Goal 1 is "find statistically significant rules," CarenR succeeds — 20 rules with p<0.05. If Goal 1 is "find rules that describe genuine within-era climate signal," then for RVV, the LOESS test shows the dominant features are era artefacts, not genuine signal. I present both readings honestly. The thesis does not hide this — the LOESS table explicitly marks two RVV features as "Era-driven; collapses under LOESS." What I argue is that the LOESS diagnosis *is* a contribution: it tells future researchers that RVV's pre-1986 data should either be excluded or modelled with structural covariates before rule induction is attempted. Discovery of what does *not* work under what conditions has scientific value.

**Q17: Weighted MAE masking early vs. late performance degradation?**

This is a perceptive methodological question. Weighting by training-set size does downweight early scenarios — which have smaller, more homogeneous training sets. A model that performs well early and degrades later (as more cross-era contamination enters training) would be disadvantaged by this weighting scheme relative to unweighted MAE. I did not analyse performance trends over time explicitly, which is a limitation. A rolling performance plot (MAE per scenario over time) would reveal whether there is systematic degradation. This is a valid extension the jury could propose.

**Q18: 17 models, no multiple comparison correction?**

The primary comparison is CarenR's best variant vs. Naive_Median — a single pre-specified test for each region. The 17-model table is presented as a descriptive ranking, not as 17 independent hypothesis tests. Applying Bonferroni correction to 17 comparisons would require p<0.003 for significance, which would make the RDD borderline result (p=0.468) even less notable — but the conclusion is already "not significant." For RVV (p=0.004), even under Bonferroni correction (0.004 × 17 = 0.068), the result is borderline significant. I report uncorrected p-values and acknowledge the limitation, consistent with the exploratory framing of the multi-model comparison.

**Q19: RIPPER tertile boundaries shift across folds — comparability?**

Yes, tertile boundaries are fold-specific. In fold k, the boundaries are computed from the training set up to year k; in fold k+1, one more year is added and the boundaries shift. This means "Low/Med/High" class membership is not identical across folds. However, this is the *correct* approach — using held-out data to set boundaries would be leakage. The consequence is that RIPPER's rules are trained and evaluated on fold-specific discretisations. Cross-fold comparability of RIPPER rules is indeed limited, which is part of why I focus on MAE (on the original continuous scale after reversing predictions) rather than classification accuracy. RIPPER's discrete outputs are converted back to continuous predictions via class centroids from the training fold — this restores comparability at the metric level even if not at the rule level.

**Q20: Structural covariates would recreate era confound?**

This is the most sophisticated future-work challenge. You are right that planted area and EU quota time series are themselves correlated with time — adding them naively could substitute temporal proxies for climate signal rather than removing era confound. The key is *how* structural covariates are incorporated. The proposal is to include them in the *detrending model* (Decision 1) so that the residual after removing the structural trend is a cleaner climate signal for rule induction (Decision 2). If planted area explains the regime break around 1986 in RVV, then residuals from "production ~ planted area + quota regime" would be stationary or at least more so than residuals from "production ~ time." This is the structural detrending approach, analogous to cointegration models in econometrics. The risk you identify is real, but it is managed by ensuring the covariate-residual is tested for stationarity before proceeding to rule induction.

---

## PART C — Potential Weaknesses

1. **Era confound is severe and acknowledged but not resolved.** The primary methodological threat is not controlled in the final system — only diagnosed. For RVV, the dominant rules are era artefacts. The thesis detects this but does not fix it.

2. **Post-hoc hypothesis.** The era-composition finding (5/5 wins, p=0.031) is the most positive predictive result in the thesis and is entirely post-hoc. Without prospective validation, it cannot be treated as confirmed.

3. **Variant selection is not within-fold.** The choice of which of 11 CarenR variants to report as "best" was made after examining results, introducing selection bias into the predictive comparison.

4. **Very small sample sizes.** 18 and 16 walk-forward scenarios provide limited statistical power. Wilcoxon tests on 18 pairs cannot detect moderate effect sizes reliably.

5. **Phenological anchors are static.** Fixed calendar windows for vine growth stages are an approximation that degrades over the 89-year observation window as climate shifts phenology.

6. **CarenR_Assoc not implemented in walk-forward.** The associative variant was excluded from predictive validation due to factor-alignment issues, making the comparison incomplete.

7. **No spatial or sub-regional analysis.** Annual aggregates at the DO level mask within-region heterogeneity that could carry additional predictive signal.

8. **M5Rules always collapses.** Including a baseline that degenerates to a global model limits the informativeness of the comparison at the upper end of complexity.

9. **Linear-only detrending.** Using OLS linear trend as the sole detrending method cannot handle the nonlinear structural break clearly visible in RVV data.

10. **No uncertainty quantification.** Point forecasts with no prediction intervals limit the operational usefulness of any rule-based prediction.

---

## PART D — How to Defend Each Weakness

**W1 — Era confound unresolved:**
"You're correct that this is diagnosed but not resolved — and that distinction is deliberate. Resolving it requires structural covariates (planted area, quotas) that are outside the climate-only scope of this thesis. The contribution is establishing *exactly what* is needed to resolve it and *why* — which provides a clear, actionable research agenda. A thesis that claimed to solve this problem without the necessary data would be scientifically weaker, not stronger."

**W2 — Post-hoc hypothesis:**
"I explicitly label it exploratory and call for prospective validation. Calling a finding exploratory is not the same as dismissing it. Pre-registration and hypothesis discovery are both legitimate phases of the scientific process. Discovering a conditional regime and naming it so it can be tested prospectively is a scientific contribution at the hypothesis-generation stage."

**W3 — Variant selection not within-fold:**
"This is a genuine limitation I acknowledge. The mitigation is transparency: I report all 11 variants, not just the best, so the jury can see whether the selected variant is an outlier or part of a robust pattern. A fully rigorous approach would nest variant selection within fold, but with only 18 scenarios, this would leave insufficient power for the outer evaluation. The reported best variant (CarenR_Eta2 for RDD) is not a dramatic outlier — several variants rank closely."

**W4 — Small sample sizes:**
"The small sample problem is inherent to annual agricultural data and cannot be resolved without changing the domain. The thesis designs around it: the Wilcoxon test is non-parametric and does not assume normality; the weighted MAE reduces variance by emphasising larger training folds; statistical findings are reported with exact p-values and clearly flagged when underpowered. The alternative — not running inferential statistics at all — would be less rigorous, not more."

**W5 — Static phenological anchors:**
"This is a real approximation. The ideal solution requires long-term phenological observation records matched to the meteorological data, which do not exist for the full 1934–2022 window. The practical fallback — fixed calendar windows — is the same approach used in the majority of agricultural ML literature at this temporal scale. Future work with observed phenological dates would improve feature quality. I name this explicitly as a limitation rather than obscuring it."

**W6 — CarenR_Assoc not in walk-forward:**
"The factor-alignment issue arises because CarenR_Assoc's factor levels are computed on the training set and may not cover all levels in the test year — a standard out-of-sample problem for categorical encoding. Resolving it requires a within-fold re-encoding strategy that I did not implement. The omission is documented and named as a natural extension; it does not affect the validity of CarenR_Dist results."

**W7 — No sub-regional analysis:**
"Annual aggregate data is the only consistently available series at 89-year length for these DOs. Sub-regional disaggregation would require shorter, incomplete series for each sub-zone and would reduce sample size further. The thesis is explicit about operating at DO aggregate level. Sub-regional analysis is a future direction once longer sub-regional records become available."

**W8 — M5Rules degenerates:**
"A degenerate baseline is informative precisely because it degenerates. It establishes that standard regression tree rule induction fails on this problem size, which motivates the use of distributional methods. If M5Rules performed well, the complexity of CarenR would be unnecessary. The degeneration is not a flaw in the experimental design — it is a result."

**W9 — Linear detrending only:**
"Linear detrending is the standard approach in agricultural time series modelling and the most defensible for within-fold application, because OLS is simple, transparent, and does not require tuning. LOESS detrending is used as a *stress test* (not as the production detrend) precisely to characterise what linear detrending misses — which is the entire point of the era-confound analysis. If I had used LOESS as the production detrend, the LOESS stress test would be circular."

**W10 — No uncertainty quantification:**
"Point forecasts are the natural output of CarenR's rule format — rules predict a distributional shift, not an interval. Adding prediction intervals would require either bootstrapping the rule induction or using conformal prediction wrappers, both of which are valid extensions. This is noted as future work. The thesis evaluates point-forecast MAE, which is standard in the agricultural forecasting literature for this type of model."

---

*End of Defense Q&A Document*

# 100 Most Likely Defense Questions

**Thesis:** Wine Production Forecasting Using Rule-Based Machine Learning — A Distributional Subgroup Discovery Approach
**Author:** Hugo Filipe Queiróz Nogueira · MECD, FEUP
**Format:** Anticipated examination-committee questions, by topic. Each entry: *why the jury asks it* · *what knowledge it tests*. **Unanswered by design** — use as a drill sheet.

> **Note:** Some questions (e.g. Q58) say "beam search" because the jury reads the submitted thesis, which used that wording. Per the errata, CAREN is an Apriori-style level-wise search — answer with the correct description (see `01_defense_prep_azevedo.md`, Part 4).

---

## Introduction

1. **In one sentence, what is the scientific contribution of this thesis?**
   - *Why:* Tests whether you can separate contribution from activity.
   - *Tests:* Ability to articulate a thesis-level claim, not a summary.

2. **Is this thesis primarily a methods contribution or an application contribution?**
   - *Why:* Positions the whole defense; forces you to commit.
   - *Tests:* Self-awareness of where the novelty actually lives.

3. **Your title says "forecasting," yet forecasting fails. Is the title honest?**
   - *Why:* Probes for over-claiming in framing.
   - *Tests:* Intellectual honesty; ability to reframe a null result.

4. **Who is the intended beneficiary — data scientists or viticulturalists?**
   - *Why:* Interpretability is justified by the audience; they check it holds.
   - *Tests:* Clarity of purpose and stakeholder reasoning.

5. **Why two regions rather than one deeply studied region?**
   - *Why:* The two-region design is your strongest choice; they check you know why.
   - *Tests:* Understanding of controlled comparison vs. case study.

---

## Motivation

6. **Why do we need interpretable rules if black-box models forecast better?**
   - *Why:* The core justification of the whole approach.
   - *Tests:* Accuracy–interpretability trade-off reasoning.

7. **You never quantify the accuracy cost of interpretability. How do you know it's worth paying?**
   - *Why:* You claim interpretability matters but never measured the ceiling.
   - *Tests:* Awareness of the missing black-box benchmark.

8. **Is annual regional production actually a decision-relevant quantity for producers?**
   - *Why:* Challenges the practical motivation.
   - *Tests:* Domain grounding of the problem statement.

9. **SHAP gives post-hoc explanations. Why is that insufficient for your use case?**
   - *Why:* Tests the distinction you draw between auditable and approximate explanations.
   - *Tests:* Understanding of explainability methods.

10. **What real decision would change if a producer had your rules?**
    - *Why:* Forces a concrete impact statement.
    - *Tests:* Translation of statistical output into action.

11. **Why is this an MSc-worthy problem rather than an engineering exercise?**
    - *Why:* Establishes scientific weight.
    - *Tests:* Ability to defend the research question's depth.

---

## Wine production

12. **Why model total production instead of yield per hectare?**
    - *Why:* The most dangerous domain question; area is a structural confound.
    - *Tests:* Whether you understand production = area × yield/ha.

13. **The benefício quota administratively caps Port output. How does that affect your target?**
    - *Why:* An unobserved covariate decouples production from climate.
    - *Tests:* Awareness of non-climate drivers you cannot see.

14. **Explain the two-year vine cycle and why it justifies your y0 features.**
    - *Why:* Core agronomy behind your feature engineering.
    - *Tests:* Grapevine physiology (primordia, reserves).

15. **What agronomic mechanism makes moderate soil-water deficit *beneficial* in the Douro?**
    - *Why:* Your top RDD rule depends on it.
    - *Tests:* Berry-concentration vs. dilution physiology.

16. **Why does the same soil-water feature invert direction between regions?**
    - *Why:* Your headline method-validation point.
    - *Tests:* Understanding of climate context dependence.

17. **How much of the Vinho Verde decline is climate vs. EU policy?**
    - *Why:* The RVV confound is central to your failure analysis.
    - *Tests:* Grasp of the structural drivers (grubbing-up, pergola→espalier).

18. **Are STICS phenological dates and VSIM soil water measurements or model outputs?**
    - *Why:* Two of your key variables are modelled, not observed.
    - *Tests:* Honesty about data provenance and error propagation.

---

## Climate

19. **Which climate variables dominate Douro production, and per whose prior work?**
    - *Why:* Grounds your features in the literature (Cunha).
    - *Tests:* Knowledge of spring temperature / soil water drivers.

20. **How do you separate a warming trend from within-era climate signal?**
    - *Why:* This is the era-confound problem in miniature.
    - *Tests:* Understanding of the LOESS retention diagnostic.

21. **Harvest temperature shows era inflation. Why is that a red flag?**
    - *Why:* Only 56% LOESS retention — a co-trending artifact.
    - *Tests:* Ability to distinguish real signal from smooth co-trend.

22. **Why phenology-anchored windows instead of fixed calendar months?**
    - *Why:* Justifies the feature construction choice.
    - *Tests:* Understanding that phenology shifts 2–4 weeks yearly.

23. **Does climate change threaten the stationarity your rules assume?**
    - *Why:* Rules are backward-looking; future climate departs from history.
    - *Tests:* Awareness of non-stationarity under warming.

24. **Could a single extreme weather event dominate a year's residual and break a rule?**
    - *Why:* Regional aggregates hide within-season shocks.
    - *Tests:* Understanding of unexplained variance sources.

---

## Statistics

25. **The p = 0.031 result is post-hoc with n = 5. Defend or retract it.**
    - *Why:* HARKing plus tiny sample.
    - *Tests:* Confirmatory vs. exploratory discipline.

26. **You applied no multiple-comparison correction across 17 models. Justify.**
    - *Why:* Family-wise error inflation.
    - *Tests:* Multiplicity awareness.

27. **Your 18 walk-forward scenarios share most of their test years. Is the Wilcoxon test valid?**
    - *Why:* Overlapping test sets violate independence.
    - *Tests:* Understanding of test assumptions and autocorrelation.

28. **You mine thousands of subgroups at KS p ≤ 0.10. How many of your 48 rules are false positives?**
    - *Why:* Search-space multiplicity inside rule mining.
    - *Tests:* Whether you understand the null-distribution problem of subgroup search.

29. **Why KS p ≤ 0.10 rather than 0.05?**
    - *Why:* A lenient threshold under heavy multiplicity.
    - *Tests:* Justification of the significance level.

30. **Explain the KS statistic and why it suits n = 80.**
    - *Why:* Foundational to CarenR.
    - *Tests:* Non-parametric ECDF comparison knowledge.

31. **Why MAE, when the median baseline is by construction the MAE-minimizer?**
    - *Why:* You chose the loss your baseline is optimal under.
    - *Tests:* Metric-selection reasoning; would RMSE change the story?

32. **What is the statistical power of a test on 5–18 scenarios?**
    - *Why:* Underpowered design.
    - *Tests:* Power analysis literacy.

33. **ADF and KPSS disagree for RVV residuals. What does that tell you?**
    - *Why:* Mixed stationarity verdict is diagnostic.
    - *Tests:* Understanding of unit-root vs. trend-stationarity.

34. **Why the Wilcoxon signed-rank rather than a paired t-test?**
    - *Why:* Distributional assumptions on small samples.
    - *Tests:* Test-selection rationale.

---

## Machine Learning

35. **Why did you not include a penalized regression (LASSO/elastic net) baseline?**
    - *Why:* The obvious interpretable competitor is missing.
    - *Tests:* Breadth of ML baseline knowledge.

36. **Where is a classical time-series model (ARIMA/ARIMAX)?**
    - *Why:* A forecasting thesis without ARIMA is conspicuous.
    - *Tests:* Time-series forecasting fundamentals.

37. **What is the predictive ceiling? What would Random Forest or gradient boosting achieve on *your* data?**
    - *Why:* You cite RF from others but never ran it.
    - *Tests:* Willingness to establish an upper bound.

38. **How do you prevent data leakage in the walk-forward pipeline?**
    - *Why:* The credibility of every predictive number depends on it.
    - *Tests:* Understanding of within-fold preprocessing.

39. **Your feature/observation ratio is 59:80. Isn't that a curse-of-dimensionality problem?**
    - *Why:* Overfitting risk is severe.
    - *Tests:* Dimensionality and sample-size reasoning.

40. **How do you handle collinearity among near-duplicate features?**
    - *Why:* Combinatorial features are highly correlated.
    - *Tests:* Feature-space hygiene.

41. **Is feature selection stable across folds? Do the same features get chosen each time?**
    - *Why:* Unstable selection undermines interpretability.
    - *Tests:* Whether you measured selection stability (you did not report it).

42. **Decision Tree performs worst. Does that mean tree structure is wrong for this data, or the tree was mis-tuned?**
    - *Why:* Distinguishes model failure from configuration failure.
    - *Tests:* Understanding of hyperparameter effects.

---

## Rule Learning

43. **Define subgroup discovery and how it differs from classification.**
    - *Why:* Foundational vocabulary.
    - *Tests:* Descriptive vs. predictive induction.

44. **Why are rule-based methods "interpretable" in a way SHAP is not?**
    - *Why:* The central justification of the family.
    - *Tests:* Explicit-rule vs. attribution reasoning.

45. **What is the difference between association rules and distribution rules?**
    - *Why:* CarenR is built on distribution rules.
    - *Tests:* Understanding of preserving the full target distribution.

46. **Rules overlap 65% pairwise. Isn't your rule set redundant?**
    - *Why:* Challenges the coherence claim.
    - *Tests:* Understanding of overlapping-subgroup design.

47. **How do you set the minimum support threshold, and why 20%?**
    - *Why:* An experimental design choice, not algorithmic.
    - *Tests:* Support/robustness trade-off reasoning.

48. **Why are older learners (RIPPER 1995) your comparators rather than modern ones (RuleFit, CORELS, Bayesian Rule Lists)?**
    - *Why:* Your comparator set skews old.
    - *Tests:* Awareness of the current interpretable-ML landscape.

49. **How do you convert descriptive rules into a point forecast?**
    - *Why:* The discovery→prediction bridge.
    - *Tests:* Understanding of the aggregation mechanism.

---

## CarenR

50. **Walk me through the CarenR pipeline end to end.**
    - *Why:* Establishes command of your primary method.
    - *Tests:* Discretization → level-wise (Apriori) search → KS → support → aggregation.

51. **Which parts of Figure 3.3 are the CarenR algorithm and which are your thesis design choices?**
    - *Why:* You must own what you added vs. inherited.
    - *Tests:* Boundary between framework and contribution.

52. **Why equal-frequency quartile binning rather than equal-width or supervised bins?**
    - *Why:* Discretization drives rule conditions.
    - *Tests:* KS-reliability rationale for balanced bins.

53. **CarenR_Supervised was your worst variant. Why does supervised discretization fail?**
    - *Why:* A clean negative result about target leakage.
    - *Tests:* Understanding of leakage in feature engineering.

54. **Support-weighted median aggregation is ad hoc. Why not a learned combiner?**
    - *Why:* The prediction rule is heuristic.
    - *Tests:* Justification of the aggregation formula.

55. **Why does η² win in RDD but Sup_FS win in RVV?**
    - *Why:* The rank reversal is a key result.
    - *Tests:* Understanding of signal-strength vs. selection-conservatism.

56. **The fallback makes CarenR default to the median. Doesn't that guarantee it can't beat the baseline?**
    - *Why:* Probes the ceiling imposed by your own design.
    - *Tests:* Understanding that value can only come from fired rules.

57. **Is a model that abstains 44% of the time and harms 56% of the time in RVV actually useful?**
    - *Why:* Challenges the fallback-as-virtue framing.
    - *Tests:* Honest appraisal of net utility.

58. **Beam search is greedy. Could it miss the best subgroups?**
    - *Why:* Search completeness. *(Note: the premise is wrong — CAREN is level-wise/exhaustive over the support-frequent region. Correct it: it does NOT miss frequent subgroups the way a beam would.)*
    - *Tests:* Understanding of heuristic vs. exhaustive search.

59. **Prior CarenR work was in-sample only. What exactly is new in your use of it?**
    - *Why:* Pins down your incremental contribution.
    - *Tests:* Positioning against Moreira 2022 / Jorge 2006.

---

## RIPPER

60. **Why is RIPPER a fair comparator when it needs a discretized target?**
    - *Why:* The tertile discretization loses information.
    - *Tests:* Fairness of cross-method comparison.

61. **Explain FOIL gain and MDL pruning.**
    - *Why:* Core RIPPER mechanics.
    - *Tests:* Rule-growing and overfitting control.

62. **RIPPER's default rule covers most years. What does that failure mode tell you?**
    - *Why:* The default rule dominates on small data.
    - *Tests:* Understanding of class-imbalance and coverage.

63. **Cross-validated accuracy is ~28–42%. Is RIPPER contributing anything?**
    - *Why:* Barely above the 33% random baseline.
    - *Tests:* When a comparator is informative despite poor accuracy.

64. **Your "CV accuracy" — is that random k-fold? Doesn't that violate your own no-random-CV stance?**
    - *Why:* Potential internal inconsistency with Ch.2.
    - *Tests:* Consistency of methodology.

---

## M5Rules

65. **M5Rules collapsed to a global linear model. Why include it at all?**
    - *Why:* It never split — is it a real comparator?
    - *Tests:* Understanding of why splits fail at n≈80, leaf-min 4.

66. **Explain standard-deviation-reduction splitting and leaf smoothing.**
    - *Why:* Core M5 mechanics.
    - *Tests:* Model-tree construction knowledge.

67. **M5Rules gives a negative soil-water slope; CarenR gives a positive moderate-deficit range. Which is right?**
    - *Why:* An apparent contradiction between methods.
    - *Tests:* Linear-marginal vs. non-linear bin-specific structure.

68. **CV R² is negative for RVV. Why report coefficients from an overfit model at all?**
    - *Why:* Interpreting coefficients from a model worse than the mean.
    - *Tests:* Discipline about what is interpretable.

69. **Does M5Rules' collapse mean the relationship is truly linear, or that the method is starved of data?**
    - *Why:* Two very different conclusions.
    - *Tests:* Distinguishing model limitation from data structure.

---

## Evaluation

70. **Justify the weighted-MAE metric. Why weight by test-set size?**
    - *Why:* Weighting interacts with era composition.
    - *Tests:* Understanding of how the metric is constructed.

71. **Weighting gives early, most-confounded scenarios the most influence. Isn't that backwards?**
    - *Why:* The metric penalizes CarenR precisely where it's weakest.
    - *Tests:* Awareness of metric–design interaction.

72. **Your protocol text says "predict the next year," but tables predict all remaining years. Which did you do?**
    - *Why:* An internal inconsistency in the design description.
    - *Tests:* Precision about your own experiment.

73. **Multi-step trend extrapolation dominates early scenarios. Is your MAE measuring climate or trend?**
    - *Why:* The residual re-trending step carries most of the error.
    - *Tests:* Decomposition of error sources.

74. **Only 17 test years per region across ~3 production cycles. Can any conclusion be robust?**
    - *Why:* The evaluation window is short.
    - *Tests:* Honesty about statistical reach.

75. **Why walk-forward instead of blocked/nested CV designs?**
    - *Why:* Alternative temporally-valid schemes exist.
    - *Tests:* Knowledge of time-series validation options.

76. **How would a permutation or block-bootstrap test change your significance claims?**
    - *Why:* A more honest null for dependent data.
    - *Tests:* Resampling literacy.

77. **What baseline would truly be hard to beat — and did you use it?**
    - *Why:* Median is easy under MAE; ARIMAX or persistence-with-drift is harder.
    - *Tests:* Baseline rigor.

---

## Prediction

78. **State plainly: did any model beat the naive baseline? Yes or no.**
    - *Why:* Forces the null result into the open.
    - *Tests:* Directness under pressure.

79. **Why do the same rules that describe the past fail to predict the future?**
    - *Why:* The central asymmetry.
    - *Tests:* Discovery vs. prediction reasoning.

80. **In RVV, adding modern data makes CarenR worse. Explain the mechanism.**
    - *Why:* Counter-intuitive era-tuning.
    - *Tests:* Understanding of era-specific overfitting.

81. **If |r| ≈ 0.47 in-sample, why doesn't that translate to out-of-sample skill?**
    - *Why:* In-sample correlation ≠ predictive value.
    - *Tests:* Generalization gap reasoning.

82. **The era-balanced window wins 5/5, then loses 2019–2022. Doesn't that destroy the "promise"?**
    - *Why:* The promising window collapses immediately after.
    - *Tests:* Robustness of the conditional claim.

83. **Is a directional (above/below trend) forecast more achievable than a point forecast? Did you test it?**
    - *Why:* A weaker but useful target you did not evaluate.
    - *Tests:* Reframing the prediction task.

84. **How would a producer use a model that beats the median only under specific era conditions?**
    - *Why:* Practical deployability of a conditional result.
    - *Tests:* Bridging statistics to use.

---

## Discussion

85. **Is the two-decision decomposition genuinely novel, or a restatement of error decomposition?**
    - *Why:* Your best contribution challenged on originality.
    - *Tests:* Precise articulation of what is new.

86. **You say Decision 1 is the bottleneck. Prove it isn't Decision 2 (weak rules).**
    - *Why:* The diagnosis must be defended.
    - *Tests:* Evidence that the rule step is near the signal ceiling.

87. **Why do the two regions diverge so sharply — degree or kind?**
    - *Why:* Structural vs. gradational difference.
    - *Tests:* Understanding of confound magnitude.

88. **Cross-algorithm convergence — could it reflect shared confounding rather than truth?**
    - *Why:* Three methods on the same confounded features may agree wrongly.
    - *Tests:* Skepticism about your own robustness argument.

89. **Your |r| ≈ 0.47 matches Fraga et al. Does that mean the signal ceiling is intrinsic?**
    - *Why:* Positions your result against prior work.
    - *Tests:* Literature-grounded interpretation.

90. **If you re-ran everything on yield/ha, what would you predict changes?**
    - *Why:* Forces a falsifiable prediction about your own fix.
    - *Tests:* Scientific reasoning about the confound.

---

## Limitations

91. **What is the single most serious limitation, in your own words?**
    - *Why:* Self-critique maturity.
    - *Tests:* Prioritization of weaknesses.

92. **Two of your key variables (soil water, LAI) are model outputs. How does that error propagate?**
    - *Why:* VSIM/STICS uncertainty is unquantified.
    - *Tests:* Error-propagation awareness.

93. **You never validate rules on truly held-out data for Goal 1. Is "validated" the right word?**
    - *Why:* Goal 1 validation is all in-sample.
    - *Tests:* Precision of the "validated" claim.

94. **The abstract says 90–92% LOESS retention; Table 4.4 shows 15–16% for RVV. Which is correct?**
    - *Why:* A direct internal contradiction.
    - *Tests:* Command of your own numbers.

95. **Single-region annual aggregates preclude spatial analysis. Does that limit the whole conclusion?**
    - *Why:* Aggregation hides heterogeneity.
    - *Tests:* Understanding of resolution limits.

---

## Future Work

96. **You propose structural detrending as the fix. Why didn't you do it within the thesis?**
    - *Why:* The central prescription is left undone.
    - *Tests:* Scoping justification and feasibility awareness.

97. **What exactly would go into the structural covariate vector, and is the pre-1986 data available?**
    - *Why:* Feasibility of your own proposal.
    - *Tests:* Concrete grasp of the data sources (IVV/IVDP).

98. **Why would EMM detect interactions that CarenR's marginal KS rules miss?**
    - *Why:* You cite EMM as the natural generalization.
    - *Tests:* Understanding of model-based interestingness.

99. **How would you pre-register the era-balanced hypothesis for a confirmatory test?**
    - *Why:* Turns the post-hoc finding into science.
    - *Tests:* Knowledge of pre-registration and prospective design.

100. **If a new student continued this, what is the first experiment they should run?**
    - *Why:* Tests whether you know the highest-leverage next step.
    - *Tests:* Research prioritization.

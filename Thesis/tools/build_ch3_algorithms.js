// Expanded Section 3.4 — Algorithm deep-dive, to be presented as a standalone
// "Chapter 3 – Algorithm Details" supplement that replaces the thin 3.4 in the
// original chapter3_methods.docx.  We rebuild the full chapter with the new 3.4.
const {
  Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell,
  HeadingLevel, AlignmentType, BorderStyle, WidthType, ShadingType,
  PageNumber, Footer
} = require('docx');
const fs = require('fs');

const H1 = t => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 360, after: 180 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 28, bold: true })] });
const H2 = t => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 300, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, bold: true })] });
const H3 = t => new Paragraph({ heading: HeadingLevel.HEADING_3, spacing: { before: 240, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, italics: true })] });
const B = t => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24 })] });
const BM = runs => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: runs.map(r => new TextRun({ font: 'Times New Roman', size: 24, ...r })) });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 80, after: 80 } });
const CAP = t => new Paragraph({ spacing: { before: 80, after: 200 }, alignment: AlignmentType.CENTER,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 20, italics: true })] });
const BULLET = t => new Paragraph({ spacing: { before: 80, after: 80, line: 320 }, alignment: AlignmentType.JUSTIFIED,
  indent: { left: 720, hanging: 360 },
  children: [new TextRun({ text: '•  ' + t, font: 'Times New Roman', size: 24 })] });

function mkTable(headers, rows, colWidths) {
  const tw = colWidths.reduce((a,b)=>a+b,0);
  const br = { style: BorderStyle.SINGLE, size: 1, color: '888888' };
  const brs = { top: br, bottom: br, left: br, right: br };
  const cell = (text, w, bold=false, fill=null) => new TableCell({ borders: brs,
    width: { size: w, type: WidthType.DXA },
    shading: fill ? { fill, type: ShadingType.CLEAR } : undefined,
    margins: { top: 80, bottom: 80, left: 120, right: 120 },
    children: [new Paragraph({ children: [new TextRun({ text, font: 'Times New Roman', size: 20, bold })] })] });
  return new Table({ width: { size: tw, type: WidthType.DXA }, columnWidths: colWidths, rows: [
    new TableRow({ tableHeader: true, children: headers.map((h,i) => cell(h, colWidths[i], true, 'D9E2F3')) }),
    ...rows.map(r => new TableRow({ children: r.map((c,i) => cell(c, colWidths[i])) }))
  ]});
}

const children = [

  // ── Re-use all original 3.1–3.3 content ──────────────────────────────────
  H1('3. Data and Methodology'),
  SP(),
  B('This chapter describes the datasets, feature engineering pipeline, detrending methodology, rule-learning algorithms, walk-forward validation protocol, baseline models, and evaluation metrics used throughout the thesis. Section 3.4 provides a detailed technical description of all three rule-learning algorithms, including their internal mechanics, how they differ from one another, and why those differences matter for interpreting the results in Chapters 4 and 5.'),
  SP(),

  H2('3.1 Study Regions and Data'),
  SP(),
  B('This study examines two geographically and climatically distinct Portuguese wine regions: the Douro Demarcated Region (RDD) and the Vinho Verde Region (RVV). The RDD dataset covers 89 annual observations from 1934 to 2022. The RVV dataset covers 80 annual observations from 1942 to 2021. Both datasets consist of one observation per year, where each observation contains a set of meteorological and phenological covariates together with annual wine production in thousands of hectolitres (mhl) as the response variable. Table 3.1 summarises the key characteristics of both datasets.'),
  SP(),
  mkTable(
    ['Characteristic','RDD (Douro)','RVV (Vinho Verde)'],
    [
      ['Period','1934–2022','1942–2021'],
      ['Observations (n)','89 years','80 years'],
      ['Climate type','Continental / semi-arid','Temperate Atlantic'],
      ['Production trend','+80% increase (1930s–2010s)','−65% decline (1960s–2020s)'],
      ['Walk-forward scenarios','18','16'],
      ['Response variable','Annual wine production (mhl)','Annual wine production (mhl)'],
    ],
    [3000,3000,3000]
  ),
  CAP('Table 3.1. Dataset characteristics for the two study regions.'),
  SP(),

  H2('3.2 Feature Engineering'),
  SP(),
  B('Covariates were constructed from daily temperature, rainfall, and soil water balance records, supplemented by Leaf Area Index (LAI) estimates from a vine phenological model. All variables were aggregated over windows defined by phenological stage and calendar date, producing features that correspond to agronomically meaningful periods of vine development. The feature naming convention encodes the variable type, phenological stage, year offset (current or previous year), and window width; for example, swa_hv_y1_b20 denotes soil water availability (swa) in the 20 days before (b20) harvest (hv) in the current year (y1).'),
  SP(),
  B('Feature families included: mean and minimum temperature at each phenological window; total rainfall and number of wet days (days with rainfall > 1 mm); soil water availability (mm) at budburst, flowering, veraison, and harvest; accumulated heat degree-days above 35°C and cold days below 15°C; and LAI at budburst and flowering. Lagged versions of selected features (previous growing season, denoted y0) were included to capture carryover effects from the prior year. The final feature matrix contained 59 candidate predictors per region.'),
  SP(),

  H2('3.3 Detrending and the Two-Decision Decomposition'),
  SP(),
  B('All models operate on linearly detrended residuals r_t = y_t − (α + βt), where y_t is observed production and (α + βt) is the OLS linear trend estimated on the training set. Predictions are converted back to the original scale by adding the projected trend value: ŷ_t = r̂_t + (α + βt). All preprocessing steps are performed exclusively on the training fold within each scenario to prevent data leakage.'),
  SP(),
  BM([{ text: 'Decision 1 — Trend modelling: ', bold: true },
    { text: 'how accurately does the training-period trend extrapolate to test years? Measured by Naive_Median MAE.' }]),
  SP(),
  BM([{ text: 'Decision 2 — Climate signal extraction: ', bold: true },
    { text: 'can a rule-based model predict detrended residuals better than zero? Measured by the gap between a model\'s weighted MAE and the Naive_Median weighted MAE.' }]),
  SP(),

  // ── NEW EXPANDED 3.4 ──────────────────────────────────────────────────────
  H2('3.4 Rule-Learning Algorithms'),
  SP(),
  B('Three rule-learning algorithms were evaluated, representing three fundamentally different approaches to extracting interpretable patterns from the same data. Understanding how each algorithm works internally — and specifically what question it is designed to answer — is essential for correctly interpreting the results in Chapter 4 and the comparison in Section 4.6. Table 3.2 summarises the key characteristics of all three algorithms before each is described in detail.'),
  SP(),
  mkTable(
    ['Property','CarenR','RIPPER','M5Rules'],
    [
      ['Task type','Distributional subgroup discovery','Classification rule learning','Piecewise linear regression'],
      ['Target variable','Continuous (detrended residuals)','Discretised classes (Low/Med/High)','Continuous (detrended residuals)'],
      ['Core question answered','When does the shape of the production distribution shift significantly?','Which climate class does a year belong to?','What exact production level does a year have?'],
      ['Statistical criterion','Kolmogorov–Smirnov two-sample test (p ≤ 0.10)','Information gain / Gini (FOIL gain for rule growing)','Standard deviation reduction (MSE minimisation)'],
      ['Rule output','IF conditions THEN distribution shifts up/down','IF conditions THEN class = Low / Med / High','IF conditions THEN production = linear model(features)'],
      ['Feature input','Discretised equal-frequency bins','Continuous (RIPPER finds own thresholds)','Continuous (M5Rules finds own thresholds)'],
      ['Prediction output','Mean/median of matching subgroup','Class midpoint or median','Linear model evaluated at feature values'],
      ['Fallback (no rules fire)','Training median residual','Default rule (most common class)','Global linear model (no conditions)'],
      ['Interpretability','Very high — simple IF/THEN with distributional evidence','High — ordered IF/THEN classification','Medium — IF/THEN rules but with linear models at leaves'],
    ],
    [2400,2200,2200,2200]
  ),
  CAP('Table 3.2. Summary comparison of the three rule-learning algorithms.'),
  SP(),

  // ── CarenR ────────────────────────────────────────────────────────────────
  H3('3.4.1 CarenR — Distribution Rules via the CAREN Framework'),
  SP(),

  B('CarenR implements the CAREN (Contrast Association Rules for Extraordinary Nodes) framework, originally developed by Jorge et al. (2006) for discovering distribution rules in numeric data. It is the primary algorithm of this thesis and the only one of the three that is specifically designed for continuous response variables without requiring pre-discretisation of the target. This section describes its mechanics in detail.'),
  SP(),

  BM([{ text: 'Conceptual foundation. ', bold: true },
    { text: 'Most rule-learning algorithms ask: "what value does this observation have?" CarenR asks a different question: "is there a subgroup of observations — defined by a conjunction of input feature conditions — where the statistical distribution of the target variable is significantly different from the distribution across the full dataset?" A CarenR rule does not say "this year will produce X mhl." It says "in years satisfying these climate conditions, the production distribution is shifted upward (or downward) in a statistically significant way — the entire distributional shape is different, not just the mean." This distinction between point prediction and distributional shift is fundamental to understanding both the strengths and the limitations of CarenR.' }]),
  SP(),

  BM([{ text: 'Step 1 — Feature discretisation. ', bold: true },
    { text: 'CarenR requires discrete (ordinal) input features. Before rule induction, each continuous climate feature is independently discretised into four equal-frequency bins (quartiles) using the training set. Equal-frequency binning ensures that each bin contains approximately the same number of training observations, avoiding sparse bins at extreme values that would make the KS test unreliable. For example, the feature swa_hv_y1_b20 (soil water at harvest) might be discretised into bins [20–74 mm], [74–101 mm], [101–152 mm], [152–350 mm], each containing approximately 22 of the 89 RDD training observations. Crucially, bin boundaries are computed exclusively on the training fold in each walk-forward scenario, preventing any leakage of test-set information into the discretisation step.' }]),
  SP(),

  BM([{ text: 'Step 2 — Feature selection. ', bold: true },
    { text: 'With 59 candidate features, each discretised into 4 bins, the full itemset search space is combinatorially large. A feature selection step reduces the feature set before rule induction, retaining only the most informative features. This thesis evaluates several selection strategies (described in Table 3.3 below) as distinct CarenR variants. The primary strategies are η²-based selection (which measures the association between each discretised feature and the discretised response using the ANOVA effect size η²), Pearson correlation selection (|r| above median), and support-filtered Pearson selection (which additionally requires that each bin of a feature contains at least a minimum fraction of the training data). The feature selection step is also performed exclusively on training data within each fold.' }]),
  SP(),

  BM([{ text: 'Step 3 — Rule induction via subgroup search. ', bold: true },
    { text: 'CarenR searches for conjunctions of feature-bin conditions (itemsets) that define subgroups with distributional shift in the target variable. The search proceeds breadth-first: single-condition rules are evaluated first, then two-condition conjunctions are formed from the most promising single conditions, and so on up to a maximum rule length (typically 3–4 conditions). For each candidate subgroup, the observations in the training set satisfying all conditions are identified, and a two-sample Kolmogorov–Smirnov (KS) test is performed comparing the target distribution in the subgroup against the global training distribution.' }]),
  SP(),

  BM([{ text: 'Step 4 — The Kolmogorov–Smirnov test. ', bold: true },
    { text: 'The KS test is a non-parametric statistical test that compares two empirical cumulative distribution functions (ECDFs). Given a subgroup S of n_S observations and the global training set of n_T observations, the KS statistic D is the maximum absolute difference between the two ECDFs:' }]),
  SP(),
  new Paragraph({ spacing: { before: 80, after: 80 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: 'D = max|F_S(x) − F_T(x)|', font: 'Times New Roman', size: 24, italics: true })] }),
  SP(),
  B('where F_S(x) is the empirical CDF of the target in the subgroup and F_T(x) is the empirical CDF in the global set. A large D indicates that the two distributions are far apart — the subgroup has a systematically different pattern of production values. The p-value associated with D measures the probability of observing this large a difference by chance if the two samples were drawn from the same distribution. A rule is retained only if p ≤ 0.10 (the significance threshold used in this study). The KS test makes no assumption about the shape of either distribution, which is important given the small sample sizes (n = 80–89) and the non-normal distribution of production residuals.'),
  SP(),

  BM([{ text: 'Step 5 — Support filter. ', bold: true },
    { text: 'A statistically significant rule that covers only two or three observations (very low support) is not useful — it describes a pattern too rare to be actionable and is likely to be a chance finding. CarenR applies a minimum support filter: a rule is retained only if it covers at least 15% of training observations (approximately 12–13 years for an 89-year dataset). This filter ensures that reported rules represent patterns that recur meaningfully across the historical record, not isolated outlier years.' }]),
  SP(),

  BM([{ text: 'Step 6 — Rule output and prediction. ', bold: true },
    { text: 'Each retained rule describes a climate subgroup as a conjunction of feature-bin conditions, along with the mean and standard deviation of the target variable within the subgroup and the KS p-value. An example RDD rule output reads: "IF wet days post-flowering ∈ [0, 3.33] AND wet days post-maturity ∈ [0, 3.46] THEN production is +121 mhl above trend on average (support 28%, p = 0.0043)." At prediction time for a new test year, the feature values of the test year are checked against all retained rules. If one or more rules fire (all conditions are satisfied), their predicted residuals are aggregated — typically as a support-weighted average. If no rule fires, the model falls back to the training median residual, producing the same prediction as the Naive_Median baseline. This fallback mechanism is critical: it means CarenR never predicts worse than the naive baseline unless it fires a rule, which makes the walk-forward evaluation in Chapter 5 a direct test of whether the fired rules add value beyond the median.' }]),
  SP(),

  BM([{ text: 'What makes CarenR unique. ', bold: true },
    { text: 'Three properties distinguish CarenR from standard rule learners. First, the KS-test criterion detects distributional shift rather than class membership or squared error reduction. This allows CarenR to identify features whose relationship with production is probabilistic and distributional — for example, LAI at harvest in Vinho Verde (Section 4.6.3), which reliably shifts the production distribution without reliably predicting any single year\'s exact value. Second, rules are verified against a global null distribution rather than constructed greedily to separate pre-defined classes, making them statistically grounded in a way that classification rules are not. Third, the explicit fallback mechanism means that CarenR is conservative: it only makes a claim about a test year when at least one training-set rule matches, and defaults to the safe prediction otherwise.' }]),
  SP(),

  mkTable(
    ['Variant','Feature selector','Selection criterion','Notes'],
    [
      ['CarenR_Dist','None','All 59 features','Widest search; most risk of spurious rules'],
      ['CarenR_Dist_FS','Pearson |r|, top-N','Retain above-median |r| features','Most common baseline variant'],
      ['CarenR_Dist_Eta2','η² (ANOVA effect size)','Bin-wise ANOVA η² with response','Best for RDD — captures non-linear association'],
      ['CarenR_Dist_Sup_FS','Pearson + support ≥ 20%','Conservative: |r| + minimum bin coverage','Most stable across both regions'],
      ['CarenR_Dist_Thresh','Pearson hard threshold','|r| ≥ 0.20','Reduces feature set aggressively'],
      ['CarenR_Dist_Sup','Pearson + support filter','|r| + support filter (less strict than Sup_FS)','Intermediate conservatism'],
      ['CarenR_Dist_Lag','ACF-based lag features','Augment set with autocorrelated lags','Tests carryover temporal effects'],
      ['CarenR_Dist_Conf','Confidence-weighted prediction','Weight rules by 1 − p-value','Stronger rules get more prediction weight'],
      ['CarenR_Dist_Stack','Two-stage stacking','CarenR residuals as second-stage input','Tests whether stacking adds value'],
      ['CarenR_Supervised','Per-fold supervised bins','Discretise using response quantiles','Evaluates target leakage from discretisation'],
    ],
    [2400,2000,2400,2200]
  ),
  CAP('Table 3.3. CarenR variants evaluated in this thesis, differing in feature selection strategy or prediction mechanism.'),
  SP(),

  // ── RIPPER ────────────────────────────────────────────────────────────────
  H3('3.4.2 RIPPER — Repeated Incremental Pruning to Produce Error Reduction'),
  SP(),

  B('RIPPER, implemented as JRip in the Weka machine learning toolkit (Witten et al., 2011), is a propositional rule learning algorithm for classification. It was introduced by Cohen (1995) as an efficient, scalable alternative to earlier rule learners such as CN2, and remains one of the most widely used interpretable classification algorithms. In this thesis RIPPER serves as an interpretable classification benchmark, allowing the rules found by CarenR to be compared against a well-established rule-based classification baseline.'),
  SP(),

  BM([{ text: 'Target discretisation. ', bold: true },
    { text: 'RIPPER is a classifier: it requires a discrete target variable. The continuous production residuals are therefore discretised into three ordinal classes — Low, Medium, and High — using tertile boundaries estimated on the training set. Each class contains approximately one third of the training observations. The class boundaries are estimated within each training fold in the walk-forward evaluation to prevent leakage. This discretisation step introduces a loss of information relative to CarenR and M5Rules, which both operate on the continuous response.' }]),
  SP(),

  BM([{ text: 'Rule growing with FOIL gain. ', bold: true },
    { text: 'RIPPER constructs rules one class at a time, starting with the minority class and working towards the majority class. For each class, it grows a rule by greedily adding conditions that maximise the FOIL information gain — a measure that rewards conditions that increase the ratio of positive (target class) to negative (other class) examples covered by the rule. Starting from an empty condition set, RIPPER adds one condition at a time until no further gain is possible or a stopping criterion is met. The grown rule is typically a conjunction of 2–5 conditions that together define a subregion of feature space highly concentrated in the target class.' }]),
  SP(),

  BM([{ text: 'Rule pruning with MDL. ', bold: true },
    { text: 'Overfitting is controlled by a pruning step that removes conditions from the grown rule using the Minimum Description Length (MDL) principle: conditions whose removal reduces the total description length of the rule set plus the data are deleted. This balances rule complexity against coverage — a rule that is slightly less precise but covers more observations is preferred over a highly specific rule covering very few. After pruning, RIPPER checks whether replacing or revising a rule reduces the total error, iterating until convergence.' }]),
  SP(),

  BM([{ text: 'Ordered rule list and default rule. ', bold: true },
    { text: 'The final output of RIPPER is an ordered rule list: rules are applied in the order they were learned, and the first rule whose conditions are satisfied assigns the class for a given observation. Any observation not covered by any explicit rule is assigned the default class — the most frequent class in the training data. This ordered, default-inclusive structure ensures complete coverage: every observation receives a class label. In this thesis, the default rule covered the majority of observations (all years not matching explicit climate conditions), limiting RIPPER\'s ability to characterise positive (above-trend) production years in the Douro.' }]),
  SP(),

  BM([{ text: 'Why RIPPER struggles with small datasets. ', bold: true },
    { text: 'RIPPER was designed for large databases where each class is represented by thousands of examples. With n = 80–89 observations split into three classes of approximately 27–30 each, the per-class sample size is very small relative to the dimensionality of the feature space. FOIL gain estimates are unreliable on such small samples, and the MDL pruning criterion may be too aggressive or too permissive. The result is either overly specific rules (high precision, very low support) or a near-trivial rule list (one or two rules plus a default) that captures only the most obvious patterns. Cross-validated accuracy of 35–42% — only slightly above the 33% random baseline — reflects this limitation directly.' }]),
  SP(),

  // ── M5Rules ───────────────────────────────────────────────────────────────
  H3('3.4.3 M5Rules — Piecewise Linear Model Trees'),
  SP(),

  B('M5Rules, also implemented in Weka, is a piecewise linear regression rule learner derived from the M5 model tree algorithm (Quinlan, 1992; Wang and Witten, 1997). Instead of predicting a class label or a distributional shift, M5Rules predicts an exact numerical value for each observation by applying a linear regression model whose coefficients depend on which region of feature space the observation falls in. This makes M5Rules the only algorithm in this study that produces a direct point estimate of the production residual, making it directly comparable to the naive baseline and to CarenR\'s subgroup-mean prediction.'),
  SP(),

  BM([{ text: 'Model tree construction. ', bold: true },
    { text: 'M5Rules begins by constructing a regression model tree using a top-down recursive partitioning procedure. At each node of the tree, M5Rules selects the feature and split threshold that maximise the reduction in standard deviation of the target variable across the two resulting child nodes — analogous to the variance reduction criterion used by CART, but applied to a regression setting. This process continues recursively until each leaf contains a minimum number of observations (default: 4 in Weka). The result is a binary tree that partitions the feature space into disjoint regions, with a multivariate linear regression model estimated at each leaf using the observations that reach it.' }]),
  SP(),

  BM([{ text: 'Leaf model estimation and smoothing. ', bold: true },
    { text: 'Within each leaf, a linear regression model is fitted to the training observations in that leaf using ordinary least squares. M5Rules applies a smoothing procedure that interpolates between the leaf model and progressively coarser ancestor models as the observation moves up the tree, reducing the discontinuities at split boundaries that would otherwise produce implausible predictions near the edges of each leaf region. The smoothed prediction at leaf depth d is a weighted combination of the leaf model and each ancestor model\'s prediction.' }]),
  SP(),

  BM([{ text: 'Pruning and conversion to rules. ', bold: true },
    { text: 'After tree construction, M5Rules prunes branches whose removal does not increase cross-validation error beyond a tolerance threshold. The pruned tree is then converted into an ordered rule list: each path from the root to a leaf becomes one rule, with the conditions along the path as the antecedent and the leaf\'s linear model as the consequent. Rules are applied in order; the first matching rule is used, with the final rule having no conditions (applies to all observations) as a universal default. This conversion gives M5Rules the same human-readable rule format as RIPPER but with a linear model at each leaf rather than a class label.' }]),
  SP(),

  BM([{ text: 'Why M5Rules collapses to a global model on small datasets. ', bold: true },
    { text: 'M5Rules\' standard deviation reduction criterion requires that each split create two child nodes, each with sufficient observations for reliable linear regression. With a minimum leaf size of 4 and n = 80–89 training observations, the algorithm cannot split the data more than a few times before individual leaves become too small for stable regression. In practice, M5Rules found no beneficial split in either region on the full dataset, collapsing to a single global linear model: one rule with no conditions that applies to all observations. This global model fits the training data with R² ≈ 0.40–0.53 but generalises poorly, with cross-validated R² falling to 0.004 (RDD) and −0.07 (RVV). The negative CV R² for RVV confirms that the model is worse than simply predicting the global mean — it has overfit the training set despite using a single linear equation.' }]),
  SP(),

  BM([{ text: 'Comparison with CarenR and RIPPER. ', bold: true },
    { text: 'The three algorithms differ fundamentally in what they optimise and what they produce. CarenR searches for distributional anomalies using a non-parametric test — it will find a feature informative if it shifts the production distribution in a statistically reliable way, even if that shift is not captured by a linear coefficient or a class boundary. RIPPER searches for class-separating conditions using information gain — it will find a feature informative if it concentrates one production class, regardless of the distribution of other classes. M5Rules searches for variance-reducing splits using squared error — it will find a feature informative if it enables a more accurate linear model, penalising large deviations quadratically. A feature that shifts the mean of the production distribution (detected by CarenR) but does not cleanly separate the three classes (not detected by RIPPER) and adds noise rather than linearity to the regression (not detected by M5Rules) will appear in CarenR\'s output but not in the others. This is precisely the situation for LAI at harvest in Vinho Verde, as discussed in Section 4.6.3.' }]),
  SP(),

  // ── 3.5–3.8 reproduced from original ─────────────────────────────────────
  H2('3.5 Walk-Forward Validation Protocol'),
  SP(),
  B('Model performance for Goal 2 was assessed using a walk-forward expanding-window validation. The earliest 80% of each regional time series defines the minimum training window. At each subsequent step (scenario), the training window is extended by one year and the model is re-fitted from scratch on all available training data; the next year is then predicted and its prediction error recorded. This generates 18 evaluation scenarios for RDD and 16 for RVV. All preprocessing steps — linear trend estimation, feature discretisation, and feature selection — are performed exclusively on the training fold within each scenario to prevent data leakage.'),
  SP(),

  H2('3.6 Baseline Models'),
  SP(),
  BM([{ text: 'Naive_Median: ', bold: true }, { text: 'predicts the median of the training-set residuals for every test year — equivalent to predicting that every test year will be exactly on trend. Minimises MAE among all constant predictors.' }]),
  SP(),
  BM([{ text: 'Naive (last value): ', bold: true }, { text: 'predicts the most recently observed production value, corresponding to a random-walk assumption.' }]),
  SP(),
  BM([{ text: 'Naive_MA (moving average): ', bold: true }, { text: 'predicts the mean of the three most recent training observations in original units.' }]),
  SP(),
  B('A Decision Tree (CART) regressor was also included as a non-rule-based ML comparator.'),
  SP(),

  H2('3.7 Evaluation Metric'),
  SP(),
  B('The primary evaluation metric is scenario-size-weighted MAE on the original production scale (mhl):'),
  SP(),
  new Paragraph({ spacing: { before: 80, after: 80 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: 'Weighted MAE = Σ (n_s × MAE_s) / Σ n_s', font: 'Times New Roman', size: 24, italics: true })] }),
  SP(),
  B('where n_s is the number of test observations in scenario s. This weighting gives more influence to scenarios with larger test sets, which in the expanding-window design are the early scenarios with shorter training histories. Statistical significance of model differences was assessed using the Wilcoxon signed-rank test applied to per-scenario MAE pairs; the post-hoc era-balanced analysis used a binomial sign test (Section 5.4).'),
  SP(),

  H2('3.8 Software and Reproducibility'),
  SP(),
  B('The full pipeline was implemented in R (version 4.x). CarenR was run via the caren R package. RIPPER and M5Rules were run via RWeka, providing a JVM bridge to Weka 3.8. The pipeline is controlled by a master runner script (run_all_pipelines.R) that sequences data preparation, rule discovery, walk-forward validation, rule interpretation, and output generation. All detrending parameters and model hyperparameters are centralised in a configuration file (utils/config.R) to ensure reproducibility.'),
  SP(),
];

const doc = new Document({
  styles: { default: { document: { run: { font: 'Times New Roman', size: 24 } } },
    paragraphStyles: [
      { id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 28, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 360, after: 180 }, outlineLevel: 0 } },
      { id: 'Heading2', name: 'Heading 2', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 24, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 300, after: 120 }, outlineLevel: 1 } },
      { id: 'Heading3', name: 'Heading 3', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 24, italics: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 240, after: 120 }, outlineLevel: 2 } },
    ] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1440, right: 1440, bottom: 1440, left: 1800 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ children: [PageNumber.CURRENT], font: 'Times New Roman', size: 20 })] })] }) },
    children }]
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter3_methods_v2.docx', buf);
  console.log('Done: chapter3_methods_v2.docx');
});

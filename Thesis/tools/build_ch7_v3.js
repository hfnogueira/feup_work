// Chapter 7 v3 — contributions reframed as data science, domain finding repositioned as validation
const {
  Document, Packer, Paragraph, TextRun,
  HeadingLevel, AlignmentType, PageNumber, Footer
} = require('docx');
const fs = require('fs');

const H1 = t => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 360, after: 180 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 28, bold: true })] });
const H2 = t => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 300, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, bold: true })] });
const B = t => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24 })] });
const BM = runs => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: runs.map(r => new TextRun({ font: 'Times New Roman', size: 24, ...r })) });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 80, after: 80 } });
const bullet = t => new Paragraph({ spacing: { before: 80, after: 80, line: 320 }, alignment: AlignmentType.JUSTIFIED,
  indent: { left: 720, hanging: 360 },
  children: [new TextRun({ text: '•  ' + t, font: 'Times New Roman', size: 24 })] });

const children = [
  H1('7. Conclusions'),
  SP(),

  B('This thesis evaluated the CarenR distributional subgroup discovery framework on two contrasting wine production time series, pursuing two goals: pattern extraction from historical data and walk-forward prediction. The findings address the conditions under which rule-based machine learning succeeds and fails on non-stationary agricultural time series — a question with implications beyond the specific domain studied here.'),
  SP(),

  H2('7.1 Summary of Findings'),
  SP(),

  B('Goal 1 — Rule Extraction — is achieved. Applied to 89 years of Douro data and 80 years of Vinho Verde data, CarenR\'s KS-test-based subgroup induction discovers 48 and 20 statistically validated rules respectively, with feature selection consistently converging on a small set of dominant variables across all 11 variants evaluated. The harvest-period soil water variable appears in 54% of Douro rules; LAI at harvest appears in all 20 Vinho Verde rules. LOESS detrending validates that these variables carry genuine within-era signal rather than era-membership artefacts, and RIPPER and M5Rules independently confirm the same variable families through entirely different optimisation criteria. This cross-algorithm convergence is evidence that CarenR\'s distributional approach successfully identifies real structure in the data.'),
  SP(),

  B('Goal 2 — Walk-Forward Prediction — is not achieved in aggregate. No rule-based model consistently beats the Naive_Median baseline across the full evaluation sequence in either region (best CarenR: 189 vs 184 mhl in RDD, Wilcoxon p = 0.468; 172 vs 140 mhl in RVV, rules actively harmful when fired, p = 0.004). A post-hoc era-composition analysis suggests CarenR is competitive in RDD when training data is era-balanced (~29% post-1990 share; 5/5 scenarios won, binomial p = 0.031), but this result is exploratory and requires prospective validation. The two-decision decomposition framework explains the failure structurally: improving trend quality (Decision 1) removes the residual signal that rule induction requires (Decision 2), and these demands cannot be simultaneously satisfied with a time-only detrending covariate.'),
  SP(),

  BM([
    { text: 'The central finding: ' },
    { text: 'CarenR\'s distributional subgroup discovery successfully extracts statistically validated, cross-algorithm-confirmed patterns from a non-stationary time series spanning multiple structural eras. Those patterns do not reliably generalise to held-out prediction because the era structure that makes rules historically interpretable also contaminates the signal in ways that dissolve when predicting within a single era. The two-decision framework precisely characterises this failure mode and identifies structural detrending as the path to resolving it.', italics: true },
  ]),
  SP(),

  H2('7.2 Contributions'),
  SP(),

  B('This thesis makes the following contributions to data science methodology and its application to agricultural time series:'),
  SP(),

  bullet('Evaluation framework: the two-decision decomposition separates prediction error into trend quality (Decision 1) and residual signal extraction (Decision 2), providing a diagnostic tool that explains why rule-based prediction fails even when genuine signal exists. This framework is transferable to any agroclimatic forecasting problem where structural non-stationarity and climate signal co-exist.'),
  bullet('Systematic sensitivity analysis: a structured 11-variant evaluation of CarenR\'s feature selection strategies shows that the optimal configuration is problem-specific — η² outperforms in regimes with strong signal (Douro), while conservative support-filtered selection outperforms in regimes where fallback-to-median is the dominant prediction mechanism (Vinho Verde). This finding motivates structured variant evaluation rather than a priori configuration choice.'),
  bullet('Era-composition analysis: the walk-forward evaluation reveals a conditional regime in RDD where CarenR consistently outperforms the baseline once training data reaches ~29% modern-era representation (5/5 consecutive wins, post-hoc exploratory). This characterises not just whether rule-based prediction works, but under what data conditions it does — a more actionable finding than a binary success/failure conclusion.'),
  bullet('Cross-algorithm validation: RIPPER (classification rules) and M5Rules (piecewise linear rules) independently converge on the same feature variables as CarenR across both regions, despite entirely different optimisation criteria. This cross-algorithm agreement provides evidence that the identified features reflect genuine data structure rather than artefacts of any single induction method.'),
  bullet('Method validation in domain: the discovered rules are agronomically coherent — confirmed by LOESS detrending which shows feature correlations are preserved after era-trend removal. This validates that CarenR extracts genuine signal from real-world non-stationary data, not era-membership proxies. The domain provides 80–90 years of historical data, genuine structural non-stationarity, and expert-verifiable output — a demanding testbed for the method.'),
  SP(),

  H2('7.3 Limitations'),
  SP(),

  B('The primary methodological limitation is that detrending uses time as its sole covariate, which cannot cleanly separate structural production changes from inter-annual climate variation. Both regional series exhibit non-linear structural trajectories driven by non-climate factors (EU policy, industry restructuring) that a linear trend imperfectly captures. This limitation affects both rule quality (era confound) and prediction (baseline calibration). A second limitation is the small number of test scenarios (18 and 16) relative to the complexity of the model family evaluated; in particular, the post-hoc binomial finding (n = 5 scenarios) has insufficient statistical power for confirmatory conclusions. The datasets are single-region annual aggregates, precluding any spatial or sub-regional analysis.'),
  SP(),

  H2('7.4 Future Work'),
  SP(),

  bullet('Structural detrending: incorporating non-climate structural covariates (planted area, production quotas, number of certified producers) as explicit detrending variables would separate structural trends from climate residuals. This would reduce the era confound in rule induction and potentially resolve the Decision 1 / Decision 2 tension identified by the two-decision framework.'),
  bullet('Prospective validation: the post-hoc era-balanced finding (p = 0.031) should be formalised as a pre-registered hypothesis and tested on a new or extended dataset as additional modern-era years accumulate.'),
  bullet('Higher-resolution climate data: replacing regional aggregates with spatially explicit gridded data would allow rule conditions to reflect within-region heterogeneity, potentially sharpening the distributional signal available to CarenR.'),
  bullet('Exceptional Model Mining extension: the EMM framework (Duivesteijn et al., 2016) generalises distributional subgroup discovery to model-based interestingness criteria. Applying EMM to identify subgroups where the climate-production regression relationship is exceptional — rather than where the marginal distribution shifts — could detect interaction effects that KS-based rules miss.'),
  SP(),

  H2('7.5 Closing Statement'),
  SP(),

  B('Rule-based machine learning, and distributional subgroup discovery in particular, offers a transparent and auditable approach to pattern extraction from real-world time series. This thesis demonstrates that CarenR can identify statistically validated, cross-algorithm-confirmed patterns from 80–90 years of non-stationary data — a result that is meaningful for the data science method regardless of whether those patterns generalise to prediction. The limitations of the predictive application are precisely characterised by the two-decision framework and are addressable through better structural detrending. The methodological contributions — the evaluation framework, the sensitivity analysis, the era-composition diagnostic — are transferable to analogous problems wherever rule-based discovery is applied to long, structurally non-stationary time series.'),
  SP(),
];

const doc = new Document({
  styles: { default: { document: { run: { font: 'Times New Roman', size: 24 } } },
    paragraphStyles: [
      { id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 28, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 360, after: 180 }, outlineLevel: 0 } },
      { id: 'Heading2', name: 'Heading 2', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 24, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 300, after: 120 }, outlineLevel: 1 } },
    ] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1440, right: 1440, bottom: 1440, left: 1800 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ children: [PageNumber.CURRENT], font: 'Times New Roman', size: 20 })] })] }) },
    children }]
});
Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter7_v3.docx', buf);
  console.log('Done: chapter7_v3.docx');
});

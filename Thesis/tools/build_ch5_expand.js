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

// These sections slot into Chapter 5 after Section 5.7
const children = [
  H1('5. Goal 2 — Predictive Performance (Additions: Sections 5.8–5.10)'),
  SP(),
  B('This document contains Sections 5.8, 5.9, and 5.10, which expand the Chapter 5 analysis with a detailed feature selector comparison, an interpretation of the era-balanced winning scenarios, and a discussion of what the fallback-to-median behaviour means in practice.'),
  SP(),

  // ── 5.8 Feature Selector Comparison ──────────────────────────────────────
  H2('5.8 Feature Selector Comparison — Why Eta2 Wins in RDD and Sup_FS in RVV'),
  SP(),
  B('The 11 CarenR variants evaluated in this study differ primarily in their feature selection strategy — the step that reduces the 58-feature input space before CarenR searches for rules. This section analyses why different selection strategies produce different outcomes across the two regions, and what that tells us about the underlying agroclimatic signal.'),
  SP(),

  mkTable(
    ['Variant', 'Selection method', 'RDD rank\n(among CarenR)', 'RDD wtd MAE', 'RVV rank\n(among CarenR)', 'RVV wtd MAE'],
    [
      ['CarenR_Dist_Eta2',      'η² ANOVA effect size',         '1st', '189.2', '9th', '225.7'],
      ['CarenR_Dist_FS',        'Pearson |r|, top-N',           '2nd', '193.5', '7th', '222.4'],
      ['CarenR_Dist_Sup_FS',    'Pearson |r| + support ≥20%',   '3rd', '196.9', '1st', '171.6'],
      ['CarenR_Dist_Sup',       'Pearson + support filter',     '4th', '197.0', '4th', '185.2'],
      ['CarenR_Dist_Lag',       'ACF-based lag features',       '5th', '200.9', '8th', '225.0'],
      ['CarenR_Dist_Conf',      'Confidence-weighted rules',    '6th', '203.1', '3rd', '171.6'],
      ['CarenR_Dist_Sup_Thresh','Pearson + support + threshold','7th', '203.1', '2nd', '171.6'],
      ['CarenR_Dist_Thresh',    'Pearson hard threshold |r|≥0.20','8th', '203.1', '6th', '209.4'],
      ['CarenR_Dist',           'No selection (all features)',  '9th', '215.3', '10th','274.6'],
      ['CarenR_Dist_Stack',     'Two-stage stacking',           '10th','229.0', '5th', '196.2'],
      ['CarenR_Supervised',     'Per-fold supervised bins',     '11th','239.5', '11th','347.3'],
    ],
    [2000, 2200, 1200, 1100, 1200, 1100]
  ),
  CAP('Table 5.10. CarenR variants ranked within the CarenR family by weighted MAE for each region. Rank reversals between regions indicate that selector quality matters differently in each climate context.'),
  SP(),

  B('The rank reversals are striking: CarenR_Dist_Eta2 ranks 1st in RDD but 9th in RVV; CarenR_Dist_Sup_FS ranks 3rd in RDD and 1st in RVV. Understanding why requires examining what each selector does and why that matters differently in each region.'),
  SP(),

  BM([{ text: 'Why η² wins in RDD. ', bold: true },
    { text: 'The η² (eta-squared) selector measures the association between each discretised feature and the discretised response using an ANOVA effect size — it detects whether the production distribution differs meaningfully across the four quartile bins of each feature. In the Douro, where genuine climate signal exists and CarenR actually fires rules in several scenarios, the quality of the feature selection matters: η² identifies the specific variables (harvest soil water, post-flowering wet days) whose distributional relationship with production is strongest within the training period. This results in rules that fire on genuinely informative conditions and improve prediction in the era-balanced scenarios. In Vinho Verde, where CarenR falls back to the median in most scenarios (rules do not fire, or fire harmfully), the quality of the feature selection is largely irrelevant — the prediction is dominated by the fallback mechanism regardless of which 10–15 features were retained. The η² variant\'s aggressive selection produces slightly harmful rules in the few RVV scenarios where it does fire, explaining its poor RVV rank.' }]),
  SP(),

  BM([{ text: 'Why Sup_FS wins in RVV. ', bold: true },
    { text: 'CarenR_Dist_Sup_FS applies a support filter on top of Pearson correlation selection: it retains only features where each bin contains at least 20% of the training observations, ensuring that the feature\'s relationship with production is robust across the entire distribution rather than driven by a handful of extreme years. In Vinho Verde, where the dominant "below-trend" signal arises from a combination of multiple co-occurring conditions (soil water + heat stress + LAI), aggressive selection tends to produce overfit rules that fire on era-specific patterns rather than genuine climate signals. The conservative Sup_FS selection retains only the most broadly applicable features, which produces rules that either fire correctly on obvious patterns or — most importantly — recognise that no confident rule applies and fall back to the median. The fallback behaviour is the key: in 7 of 16 RVV scenarios, Sup_FS correctly predicts the median (zero gap vs Naive_Median), whereas a more aggressive selector would fire a spurious rule and accumulate error.' }]),
  SP(),

  B('The practical implication is clear: in regimes with strong and consistent climate signal (RDD), aggressive feature selection that identifies the most discriminating features improves prediction. In regimes with weaker or more era-contaminated signal (RVV), conservative selection that preserves the fallback mechanism is preferable. The optimal strategy is therefore problem-specific, which argues for the kind of structured sensitivity analysis — evaluating all 11 variants — that this thesis undertakes, rather than committing to a single variant a priori.'),
  SP(),

  // ── 5.9 Interpreting the Winning Scenarios ────────────────────────────────
  H2('5.9 What Happened in the Era-Balanced Winning Years?'),
  SP(),
  B('The five scenarios where CarenR_Dist_Eta2 beats the naive baseline in RDD correspond to test years 2014, 2015, 2016, 2017, and 2018. Understanding what made these years predictable from climate rules — and why CarenR succeeded where it fails in other years — provides important context for the post-hoc finding (p = 0.031).'),
  SP(),
  B('In each of these five years, CarenR fired rules based on the top RDD signals — dry post-flowering conditions and moderate soil water stress at harvest — and its predicted above-trend residual was closer to the actual production than the training median. The training sets for scenarios 10–14 (covering 1934 through 2013–2017) had reached approximately 29–32% post-1990 representation, meaning the discovered rules were calibrated on a training distribution that meaningfully included the modern (post-1990) production era rather than being dominated by the high-production pre-1976 era.'),
  SP(),
  B('Agronomically, 2014–2018 were years characterised by the Douro\'s increasingly hot, dry summers — a pattern that has intensified under recent climate trends. The dry post-flowering and moderate pre-harvest soil water conditions that define the top CarenR rules were clearly present in multiple years of this window, creating genuine rule-fireable conditions that translated to above-trend production. The fact that CarenR could detect and predict this correctly — rather than falling back to the median — is precisely what the distributional rule framework is designed to achieve: identifying years where the climate signal is strong enough to be actionable.'),
  SP(),
  B('The reason CarenR then fails again in scenarios 15–18 (test years 2019–2022) is equally informative. The 2019–2022 window was characterised by unusual year-to-year variability driven by the combined effects of climate extremes (severe drought years interspersed with unusually wet seasons). The rules learned from 1934–2018 training data were calibrated to the historical distributional structure, which was disrupted by the post-2018 volatility. This is a reminder that distributional rules are inherently backward-looking: they identify patterns from historical data, and their predictive value diminishes when the future departs from the historical distributional structure.'),
  SP(),

  // ── 5.10 The Meaning of Fallback-to-Median ───────────────────────────────
  H2('5.10 The Fallback Mechanism — When CarenR Says "I Don\'t Know"'),
  SP(),
  B('A distinctive and underappreciated feature of the CarenR prediction framework is its explicit fallback mechanism: when no rule fires for a given test year, the model predicts the training median residual — the same prediction as the Naive_Median baseline. This produces a zero gap between CarenR and Naive_Median in those scenarios.'),
  SP(),
  B('This behaviour is not a failure — it is the model correctly communicating uncertainty. A CarenR fallback is semantically equivalent to saying: "the climate conditions of this test year do not match any of the patterns I learned from historical data with sufficient confidence. My best prediction is therefore the historical average." This is a principled, conservative position that avoids compounding error by applying a rule that may not be appropriate.'),
  SP(),
  B('The contrast with RIPPER is instructive. RIPPER always assigns a class — it has a default rule that catches all unmatched observations, and it will always produce a prediction. This means RIPPER never explicitly signals uncertainty; every test year gets a confident class assignment regardless of how well it matches the training patterns. The result is that RIPPER\'s errors in difficult years are often larger than CarenR\'s, because CarenR defaults to the safe median while RIPPER applies its default (most-common-class) rule.'),
  SP(),

  mkTable(
    ['Scenario type', 'CarenR behaviour', 'Prediction', 'Gap vs baseline', 'Interpretation'],
    [
      ['Rules fire, correct direction', 'Applies matching rule(s)', 'Weighted rule mean', 'Negative (CarenR wins)', 'Genuine climate signal detected and used'],
      ['Rules fire, wrong direction', 'Applies matching rule(s)', 'Weighted rule mean', 'Positive (CarenR loses)', 'Era-specific rule misfires on test year'],
      ['No rules fire (fallback)', 'Returns training median', 'Training median ≈ 0', 'Zero (tie with baseline)', 'Model correctly recognises no applicable pattern'],
    ],
    [2000, 2000, 1600, 1600, 3000]
  ),
  CAP('Table 5.11. The three possible outcomes of CarenR prediction and their interpretation.'),
  SP(),

  B('In RDD, fallback scenarios are rare because CarenR tends to find applicable rules in most years — the Douro climate signal is strong enough that at least one condition-set matches most test years. In RVV, fallback is the dominant outcome in later scenarios (scenarios 12–16), where the training distribution has stabilised into the modern low-production era and CarenR correctly identifies that the historical rules — calibrated partly on high-production pre-1980 data — are no longer reliably applicable. The increase in fallback frequency in later RVV scenarios is itself a diagnostic finding: it shows that CarenR is becoming progressively more conservative as the era gap between training history and test years widens, which is the correct adaptive response to structural non-stationarity.'),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter5_expand.docx', buf);
  console.log('Done: chapter5_expand.docx');
});

// Chapter 5 v2 — p=0.031 protection tightened, post-hoc label explicit throughout
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

const children = [
  H1('5. Goal 2 — Predictive Performance'),
  SP(),

  B('This chapter reports the results of the walk-forward validation experiment for both regions. Seventeen models were evaluated across 18 scenarios for RDD and 16 for RVV. The primary ranking metric is scenario-size-weighted MAE on the original production scale (mhl). The 17-model set was designed as a structured sensitivity analysis: rather than selecting one CarenR configuration a priori, all major feature selection strategies were evaluated in parallel to determine which approach transfers best to out-of-sample prediction. This exhaustive comparison makes it possible to identify which design choices matter and which are irrelevant — a question that cannot be answered by evaluating a single variant. Section 5.1 presents the aggregate rankings; Section 5.2 examines the era-composition structure of the RDD results; Section 5.3 presents the regional contrast; Section 5.4 reports the statistical tests with explicit classification of confirmatory vs. exploratory results; and Section 5.5 covers the detrend sensitivity analysis.'),
  SP(),

  H2('5.1 Aggregate Walk-Forward Results'),
  SP(),

  H3('5.1.1 Douro (RDD)'),
  SP(),
  B('Table 5.1 presents the complete 17-model ranking for the Douro region, sorted by weighted MAE. Naive_Median ranks first with a weighted MAE of 184 mhl. The best CarenR variant, CarenR_Dist_Eta2, achieves 189 mhl — a gap of 5 mhl (3%) relative to the naive baseline. The remaining CarenR variants cluster between 193 and 215 mhl, all ahead of the single-value persistence baseline (Naive, 199 mhl). RIPPER (279 mhl) and Decision Tree (250 mhl) are the worst-performing models, consistent with the unsuitability of hard classification boundaries and tree splits for this small, high-noise dataset.'),
  SP(),

  mkTable(
    ['Rank','Model','Weighted MAE (mhl)','Mean MAE (mhl)','SD MAE'],
    [
      ['1','Naive_Median','184.0','172.8','28.4'],
      ['2','CarenR_Dist_Eta2','189.2','179.4','30.6'],
      ['3','CarenR_Dist_FS','193.5','189.0','22.4'],
      ['4','Naive_MA','194.6','170.3','51.1'],
      ['5','CarenR_Dist_Sup_FS','196.9','191.1','33.5'],
      ['6','CarenR_Dist_Sup','197.0','191.4','33.3'],
      ['7','Naive','199.4','186.3','32.0'],
      ['8','M5Rules','200.0','190.2','38.4'],
      ['9','CarenR_Dist_Lag','200.9','181.6','42.7'],
      ['10–12','CarenR_Dist_Conf / Sup_Thresh / Thresh (tied)','203.1','193.9','38.7'],
      ['13','CarenR_Dist','215.3','204.3','39.1'],
      ['14','CarenR_Dist_Stack','229.0','217.8','32.2'],
      ['15','CarenR_Supervised','239.5','230.1','36.6'],
      ['16','DT','249.7','242.4','43.7'],
      ['17','Ripper','279.4','275.9','84.6'],
    ],
    [600,3000,1800,1800,1000]
  ),
  CAP('Table 5.1. RDD walk-forward results, 17 models across 18 scenarios, sorted by weighted MAE. All values in mhl.'),
  SP(),

  H3('5.1.2 Vinho Verde (RVV)'),
  SP(),
  B('For Vinho Verde, Naive_Median again ranks first at 140 mhl. The best CarenR variant is CarenR_Dist_Sup_FS at 172 mhl — a gap of 31 mhl (22%) relative to the naive baseline. CarenR_Dist_Eta2, the best RDD model, drops from rank 2 in RDD to rank 12 in RVV (226 mhl), a rank reversal of 10 positions. RIPPER reaches 508 mhl and Decision Tree 479 mhl.'),
  SP(),

  mkTable(
    ['Rank','Model','Weighted MAE (mhl)','Rank in RDD','Rank shift vs RDD'],
    [
      ['1','Naive_Median','140.4','1','0'],
      ['2','Naive','143.8','7','+5'],
      ['3','Naive_MA','170.5','4','+1'],
      ['4','CarenR_Dist_Sup_FS','171.6','5','+1'],
      ['5','CarenR_Dist_Sup_Thresh','171.6','11','+6'],
      ['6','CarenR_Dist_Conf','171.6','10','+4'],
      ['7','CarenR_Dist_Sup','185.2','6','−1'],
      ['8','CarenR_Dist_Stack','196.2','14','+6'],
      ['9','CarenR_Dist_Thresh','209.4','12','+3'],
      ['10','CarenR_Dist_FS','222.4','3','−7'],
      ['11','CarenR_Dist_Lag','225.0','9','−2'],
      ['12','CarenR_Dist_Eta2','225.7','2','−10'],
      ['13','M5Rules','267.5','8','−5'],
      ['14','CarenR_Dist','274.6','13','−1'],
      ['15','CarenR_Supervised','347.3','15','0'],
      ['16','DT','479.3','16','0'],
      ['17','Ripper','508.2','17','0'],
    ],
    [600,2800,1800,1400,1600]
  ),
  CAP('Table 5.2. RVV walk-forward results, sorted by weighted MAE. Rank shift = RVV rank minus RDD rank (positive = better in RVV relative to RDD).'),
  SP(),

  H2('5.2 Era Composition and Per-Scenario Analysis (RDD)'),
  SP(),

  B('To investigate whether the aggregate weighted MAE obscures important structure, the per-scenario MAE for CarenR_Dist_Eta2 was examined alongside the proportion of post-1990 observations in each training fold. This analysis was conducted after observing the aggregate results — it is therefore exploratory, and all conclusions drawn from it should be read as hypothesis-generating rather than confirmatory.'),
  SP(),

  mkTable(
    ['Phase','Scenarios','Post-1990 share','Test window','CarenR vs Naive_Median','CarenR wins?'],
    [
      ['Early — era-confounded','1–9','20–28%','10–18 years','+5 to +28 mhl','No (7/9 losses)'],
      ['Balanced era','10–14','29–32%','5–9 years','−17 to −32 mhl','Yes (5/5 wins)'],
      ['Late — tiny test sets','15–18','33–35%','1–4 years','Noisy','Unreliable'],
    ],
    [1800,1100,1500,1300,1800,1500]
  ),
  CAP('Table 5.3. Per-scenario CarenR_Dist_Eta2 vs Naive_Median performance for RDD, grouped by era-composition phase. Phase boundaries were identified post-hoc.'),
  SP(),

  B('In scenarios 10–14, once the post-1990 share of training data reaches approximately 29%, CarenR_Dist_Eta2 beats the Naive_Median in every scenario by margins of 17–32 mhl. In early scenarios (post-1990 share 20–28%), CarenR loses in 7 of 9 scenarios. The aggregate weighted MAE still places CarenR behind the baseline (189 vs 184 mhl) because early scenarios have the largest test sets (18 years in Scenario 1 vs 5 years in Scenario 14) and therefore dominate the weighted aggregate.'),
  SP(),

  H2('5.3 Regional Contrast — RDD vs RVV'),
  SP(),

  B('The era-composition analysis was applied to RVV to test whether a similar improvement pattern appears. The result is unambiguous: across all 16 RVV scenarios, CarenR achieves zero wins against the Naive_Median. As more post-1990 data enters the training set, CarenR\'s performance in RVV worsens rather than improves — the opposite direction from RDD.'),
  SP(),

  mkTable(
    ['Outcome','Count','Interpretation'],
    [
      ['Identical to Naive_Median (fallback)','7 / 16','No rules fired — CarenR predicted training median'],
      ['Worse than Naive_Median','9 / 16','Rules fired but hurt prediction — harmful rules applied'],
      ['Better than Naive_Median','0 / 16','CarenR never beats the naive baseline in any scenario'],
    ],
    [2800,1200,5000]
  ),
  CAP('Table 5.4. RVV scenario outcomes for CarenR_Dist_Sup_FS vs Naive_Median.'),
  SP(),

  mkTable(
    ['Dimension','RDD — Douro','RVV — Vinho Verde'],
    [
      ['CarenR wins (any scenario)','5 / 18','0 / 16'],
      ['Effect of adding post-1990 data','Improves CarenR','Worsens CarenR'],
      ['Fallback to median (no rules)','Rare','7 / 16 (44%)'],
      ['Rules fire but hurt prediction','Some early scenarios','9 / 16 (56%)'],
      ['Best CarenR gap to Naive_Median','+3% (189 vs 184 mhl)','+22% (172 vs 140 mhl)'],
    ],
    [3200,2900,2900]
  ),
  CAP('Table 5.5. RDD vs RVV summary comparison.'),
  SP(),

  H2('5.4 Statistical Tests'),
  SP(),

  B('Three statistical tests were conducted. Tests are classified below as confirmatory (hypothesis specified before examining results) or exploratory (hypothesis identified after examining results). Uncorrected p-values are reported; no correction for multiple comparisons was applied.'),
  SP(),

  BM([{ text: 'Test 1 — Overall RDD (confirmatory): ', bold: true },
    { text: 'Wilcoxon signed-rank test, CarenR_Dist_Eta2 vs Naive_Median, 18 scenarios. Result: W = 68, p = 0.468. Not significant. CarenR does not significantly outperform or underperform the naive baseline in aggregate.' }]),
  SP(),

  BM([{ text: 'Test 2 — Era-balanced RDD scenarios 10–14 (exploratory / post-hoc): ', bold: true },
    { text: 'Binomial sign test on 5 scenarios where CarenR won in all 5. The scenarios were identified as era-balanced after observing that CarenR won in them — not pre-specified. Result: p = 0.031. Although this meets the α = 0.05 threshold, the post-hoc identification of the scenario window means this result must be treated as exploratory. It generates a testable hypothesis — that CarenR outperforms the naive baseline when post-1990 training share reaches ~29% — but does not confirm it. Independent prospective validation on new data is required before treating this as a robust finding. This result is referred to throughout this thesis as a post-hoc exploratory finding.' }]),
  SP(),

  BM([{ text: 'Test 3 — RVV rule-firing scenarios (confirmatory): ', bold: true },
    { text: 'Wilcoxon signed-rank test on the 9 RVV scenarios where CarenR fired rules (excluding 7 fallback scenarios). Result: W = 0, p = 0.004. Highly significant. When CarenR fires rules in Vinho Verde, those rules are significantly worse than the naive baseline. CarenR\'s rules actively harm predictive accuracy in RVV. This is a confirmatory negative result.' }]),
  SP(),

  mkTable(
    ['Test','Comparison','Statistic','p-value','Classification'],
    [
      ['Wilcoxon (2-sided)','CarenR_Dist_Eta2 vs Naive_Median, RDD, all 18 scenarios','W = 68','0.468','Confirmatory — not significant'],
      ['Binomial sign test','CarenR wins 5/5 era-balanced scenarios (10–14), RDD','p(k≥5, n=5)','0.031','POST-HOC / EXPLORATORY — requires prospective validation'],
      ['Wilcoxon (2-sided)','CarenR_Dist_Sup_FS vs Naive_Median, RVV, 9 rule-firing scenarios','W = 0','0.004','Confirmatory — highly significant negative result'],
    ],
    [1600,3500,1500,1000,1900]
  ),
  CAP('Table 5.6. Statistical tests with explicit classification. The p=0.031 result is exploratory and must not be interpreted as confirming CarenR\'s superiority in era-balanced conditions.'),
  SP(),

  H2('5.5 Detrend Sensitivity Analysis (LOESS)'),
  SP(),

  B('LOESS detrending (span = 0.40) reduces the Naive_Median weighted MAE from 140 to ~90 mhl for RVV (−36% improvement in Decision 1) but eliminates all CarenR rules (0 rules fire in all 16 scenarios). For RDD, LOESS worsens the baseline from 184 to 290 mhl (+58%) because LOESS extrapolates a slope that the Douro\'s plateau-shaped trajectory does not follow. The LOESS results confirm that the choice of detrend method is region-specific and that improving Decision 1 comes at the direct cost of suppressing the signal required for Decision 2.'),
  SP(),

  mkTable(
    ['Region','Detrend','Naive_Median wtd MAE','CarenR rules found','CarenR best wtd MAE'],
    [
      ['RDD','Linear (primary)','184 mhl','48','189 mhl (Eta2)'],
      ['RDD','LOESS (sensitivity)','290 mhl','N/A','—'],
      ['RVV','Linear (primary)','140 mhl','20','172 mhl (Sup_FS)'],
      ['RVV','LOESS (sensitivity)','~90 mhl','0 (suppressed)','Falls back to median'],
    ],
    [1600,1800,1800,2000,2000]
  ),
  CAP('Table 5.7. LOESS detrending sensitivity analysis.'),
  SP(),

  H2('5.6 Negative Results — Supervised Discretisation'),
  SP(),

  B('CarenR_Supervised — which performs discretisation of the response variable within each training fold — was the worst-performing CarenR variant in both regions (rank 15; RDD 240 mhl, RVV 347 mhl). It discovers 46–146 rules per scenario, substantially more than the unsupervised variants, but the rules overfit the training fold because the bin boundaries are optimised on the same data used to assess rule quality. This is a form of target leakage in the feature engineering step. The result is consistent across both regions, confirming that per-fold supervised discretisation degrades out-of-sample performance regardless of climate regime.'),
  SP(),

  H2('5.7 Summary of Goal 2 Findings'),
  SP(),

  B('No model beats Naive_Median in aggregate in either region. The best CarenR variant falls 3% behind in RDD (Wilcoxon p = 0.468) and 22% behind in RVV. The 22% gap in RVV is accompanied by a confirmatory finding that CarenR\'s rules actively harm prediction when they fire (p = 0.004). A post-hoc exploratory analysis (p = 0.031, n = 5 scenarios) suggests CarenR may be competitive in the Douro when training data is era-balanced — but this observation requires independent validation before it can be treated as a robust finding. Goal 2 is assessed as not achieved in aggregate, with a conditional observation in the RDD era-balanced window that motivates future prospective investigation.'),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter5_prediction_v2.docx', buf);
  console.log('Done: chapter5_prediction_v2.docx');
});

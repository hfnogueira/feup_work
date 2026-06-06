// Chapter 5 per-scenario tables supplement
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

function mkTable(headers, rows, colWidths, highlightRows=[]) {
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
    ...rows.map((r, ri) => {
      const fill = highlightRows.includes(ri) ? 'D5F5E3' : null; // green for wins
      return new TableRow({ children: r.map((c,i) => cell(c, colWidths[i], false, fill)) });
    })
  ]});
}

const children = [

  H1('5. Goal 2 — Predictive Performance (Per-Scenario Analysis Supplement)'),
  SP(),
  B('This document contains Sections 5.8 and 5.9 — the full per-scenario MAE tables — to be merged into Chapter 5 after Section 5.4.'),
  SP(),

  H2('5.8 Per-Scenario MAE — Douro (RDD)'),
  SP(),
  B('Table 5.8 presents the complete per-scenario MAE values for Naive_Median and CarenR_Dist_Eta2 across all 18 RDD scenarios. The gap column (positive = CarenR worse, negative = CarenR better) reveals the era-composition pattern described in Section 5.2. Scenarios 10–14, highlighted in green, are the five consecutive era-balanced scenarios where CarenR outperforms the naive baseline. The training period for each scenario is 1934 through the year indicated; the test year is the year immediately following.'),
  SP(),

  mkTable(
    ['Scen.', 'Train period', 'Test yr', 'Naive_Median\nMAE (mhl)', 'CarenR_Eta2\nMAE (mhl)', 'Gap\n(+ = CarenR worse)', 'Era phase'],
    [
      ['1',  '1934–2004', '2005', '183.5', '199.5', '+16.0', 'Early — era-confounded'],
      ['2',  '1934–2005', '2006', '199.7', '204.8', '+5.1',  'Early — era-confounded'],
      ['3',  '1934–2006', '2007', '223.3', '216.0', '−7.3',  'Early — marginal CarenR win'],
      ['4',  '1934–2007', '2008', '203.9', '203.7', '−0.2',  'Early — near-tie'],
      ['5',  '1934–2008', '2009', '184.3', '209.5', '+25.1', 'Early — era-confounded'],
      ['6',  '1934–2009', '2010', '174.4', '201.4', '+27.0', 'Early — era-confounded'],
      ['7',  '1934–2010', '2011', '186.5', '214.4', '+27.9', 'Early — era-confounded'],
      ['8',  '1934–2011', '2012', '174.9', '190.5', '+15.6', 'Early — era-confounded'],
      ['9',  '1934–2012', '2013', '164.0', '172.7', '+8.7',  'Early — era-confounded'],
      ['10', '1934–2013', '2014', '170.5', '140.5', '−30.0', 'Balanced era ★ CarenR wins'],
      ['11', '1934–2014', '2015', '165.4', '148.3', '−17.1', 'Balanced era ★ CarenR wins'],
      ['12', '1934–2015', '2016', '186.7', '158.9', '−27.9', 'Balanced era ★ CarenR wins'],
      ['13', '1934–2016', '2017', '170.1', '138.6', '−31.5', 'Balanced era ★ CarenR wins'],
      ['14', '1934–2017', '2018', '176.3', '145.0', '−31.3', 'Balanced era ★ CarenR wins'],
      ['15', '1934–2018', '2019', '157.6', '177.0', '+19.5', 'Late — small test sets'],
      ['16', '1934–2019', '2020', '158.1', '180.1', '+21.9', 'Late — small test sets'],
      ['17', '1934–2020', '2021', '86.7',  '120.2', '+33.4', 'Late — 1 yr test, noisy'],
      ['18', '1934–2021', '2022', '143.9', '208.7', '+64.8', 'Late — 1 yr test, noisy'],
    ],
    [500, 1400, 700, 1400, 1400, 1200, 2400],
    [9, 10, 11, 12, 13]  // rows 9-13 (0-indexed) = scenarios 10-14
  ),
  CAP('Table 5.8. Per-scenario MAE for Naive_Median and CarenR_Dist_Eta2 across all 18 RDD scenarios. Green rows = CarenR wins (scenarios 10–14). Gap = CarenR MAE − Naive_Median MAE.'),
  SP(),

  B('Several observations from Table 5.8 are worth highlighting beyond the era-composition pattern already discussed in Section 5.2.'),
  SP(),
  BM([{ text: 'Scenarios 3 and 4 (marginal CarenR wins, −7.3 and −0.2 mhl): ', bold: true },
    { text: 'These two early scenarios show CarenR marginally matching or beating the naive baseline, despite low post-1990 training share (~22%). They fall in 2007–2008, a period following a moderate drought year (2005, one of the driest in Iberia in the modern era) that may have created unusual year-to-year variation that CarenR\'s rules happened to capture. They do not represent reliable era-balanced wins — their margins are near-zero and not consistent across the broader early window.' }]),
  SP(),
  BM([{ text: 'Scenarios 17 and 18 (large positive gaps, +33 and +65 mhl): ', bold: true },
    { text: 'The two final scenarios each test only a single year (2021 and 2022 respectively). With a single test observation, the scenario MAE equals the absolute prediction error for that year alone. CarenR\'s large positive gap in these scenarios reflects that its rules misfired on these two specific years — likely because 2021 and 2022 were anomalous in ways not captured by the training distribution. This highlights a fundamental limitation of single-year test sets: they cannot separate systematic model failure from one-off prediction errors. The experiment log (Section 21.3) notes that these late scenarios are explicitly excluded from the era-balanced binomial analysis for this reason.' }]),
  SP(),
  BM([{ text: 'Scenario Naive_Median MAE range (86.7–223.3 mhl): ', bold: true },
    { text: 'The variation in Naive_Median MAE across scenarios reflects the inherent year-to-year production variability of the test year in each case. When the test year happens to be close to the trend (Scenario 17: 2021 was near-trend), Naive_Median performs very well. When the test year is a strong outlier (Scenario 3: 2007 was above-trend in Douro), Naive_Median has a high error regardless of what any model does. This variability confirms the importance of using weighted aggregate metrics rather than relying on any single scenario.' }]),
  SP(),

  H2('5.9 Per-Scenario MAE — Vinho Verde (RVV)'),
  SP(),
  B('Table 5.9 presents the equivalent per-scenario analysis for RVV, comparing Naive_Median against CarenR_Dist_Sup_FS (the best RVV CarenR variant). The "outcome" column indicates whether CarenR fell back to the median (identical prediction, zero gap) or fired rules that hurt prediction (positive gap).'),
  SP(),

  mkTable(
    ['Scen.', 'Train period', 'Test yr', 'Naive_Median\nMAE (mhl)', 'CarenR_Sup_FS\nMAE (mhl)', 'Gap\n(+ = CarenR worse)', 'Outcome'],
    [
      ['1',  '1942–2001', '2002', '143.0', '143.0', '+0.0',  'FALLBACK — no rules fired'],
      ['2',  '1942–2002', '2003', '137.3', '151.4', '+14.1', 'HARMFUL — rules fired, hurt prediction'],
      ['3',  '1942–2003', '2004', '115.7', '115.7', '+0.0',  'FALLBACK — no rules fired'],
      ['4',  '1942–2004', '2005', '106.0', '139.4', '+33.4', 'HARMFUL — rules fired, hurt prediction'],
      ['5',  '1942–2005', '2006', '111.4', '150.2', '+38.8', 'HARMFUL — rules fired, hurt prediction'],
      ['6',  '1942–2006', '2007', '121.5', '165.5', '+44.0', 'HARMFUL — rules fired, hurt prediction'],
      ['7',  '1942–2007', '2008', '129.5', '180.8', '+51.3', 'HARMFUL — rules fired, hurt prediction'],
      ['8',  '1942–2008', '2009', '130.5', '192.7', '+62.2', 'HARMFUL — rules fired, hurt prediction'],
      ['9',  '1942–2009', '2010', '151.8', '222.6', '+70.8', 'HARMFUL — rules fired, worst gap'],
      ['10', '1942–2010', '2011', '171.0', '244.8', '+73.8', 'HARMFUL — rules fired, worst gap'],
      ['11', '1942–2011', '2012', '175.2', '257.6', '+82.4', 'HARMFUL — rules fired, largest gap'],
      ['12', '1942–2012', '2013', '206.2', '206.2', '+0.0',  'FALLBACK — no rules fired'],
      ['13', '1942–2013', '2014', '184.0', '184.0', '+0.0',  'FALLBACK — no rules fired'],
      ['14', '1942–2014', '2015', '213.8', '213.8', '+0.0',  'FALLBACK — no rules fired'],
      ['15', '1942–2015', '2016', '235.9', '235.9', '+0.0',  'FALLBACK — no rules fired'],
      ['16', '1942–2016', '2017', '260.3', '260.3', '+0.0',  'FALLBACK — no rules fired'],
    ],
    [500, 1400, 700, 1400, 1400, 1200, 2400]
  ),
  CAP('Table 5.9. Per-scenario MAE for Naive_Median and CarenR_Dist_Sup_FS across all 16 RVV scenarios. Gap = CarenR MAE − Naive_Median MAE. All gaps are zero or positive — CarenR never beats the baseline.'),
  SP(),

  B('The RVV per-scenario table reveals important structure that the aggregate weighted MAE figure of 172 mhl obscures.'),
  SP(),
  BM([{ text: 'The degradation trajectory (Scenarios 4–11): ', bold: true },
    { text: 'The gap between CarenR and the naive baseline grows monotonically from +33 mhl (Scenario 4) to +82 mhl (Scenario 11). This is the clearest evidence of the era-composition deterioration described in Section 5.3: as the training set includes more post-1990 observations (with substantially lower production than the historical average), the rules CarenR discovers become increasingly calibrated to the modern low-production era. When those rules are applied to test years that are also in the modern era, they fire conditions that — by coincidence — match the training set\'s era characteristics rather than genuine inter-annual climate signals, and predict in the wrong direction.' }]),
  SP(),
  BM([{ text: 'The fallback pattern (Scenarios 1, 3, 12–16): ', bold: true },
    { text: 'Seven of 16 scenarios show a zero gap — CarenR fired no rules and fell back to the training median. This is not a failure in the same sense as the harmful-rules scenarios: it means CarenR correctly identified that no confident rule applied to the test year and made the safe prediction. The fallback scenarios are concentrated in early scenarios (1, 3) and late scenarios (12–16). The late fallback pattern is interesting: in scenarios 12–16, the training set is very era-balanced (all modern era), but CarenR apparently finds no applicable rules. This suggests that with a homogeneous modern-era training set, the wine production signal becomes too noisy for any subgroup to achieve both KS significance and ≥15% support — confirming that the era structure is what created the spurious rules in the first place.' }]),
  SP(),
  BM([{ text: 'Weighted MAE decomposition: ', bold: true },
    { text: 'The weighted aggregate of 172 mhl is dominated by the 9 harmful scenarios (weighted by test-set sizes of 20–80 years in early scenarios). The 7 fallback scenarios contribute 0 mhl to the gap. This means the 22% gap between CarenR and Naive_Median in RVV is entirely attributable to the harmful-rules scenarios, not to the fallback scenarios. The Wilcoxon test on harmful-rules scenarios (p = 0.004, Section 5.4) correctly identifies these as the source of the problem.' }]),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter5_per_scenario.docx', buf);
  console.log('Done: chapter5_per_scenario.docx');
});

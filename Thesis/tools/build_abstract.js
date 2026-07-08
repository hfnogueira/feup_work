const {
  Document, Packer, Paragraph, TextRun,
  HeadingLevel, AlignmentType, PageNumber, Footer
} = require('docx');
const fs = require('fs');

const H1 = t => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 360, after: 240 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 28, bold: true })] });
const B = t => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24 })] });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 80, after: 80 } });
const KW = t => new Paragraph({ spacing: { before: 200, after: 120 }, alignment: AlignmentType.JUSTIFIED,
  children: [
    new TextRun({ text: 'Keywords: ', font: 'Times New Roman', size: 24, bold: true }),
    new TextRun({ text: t, font: 'Times New Roman', size: 24, italics: true }),
  ] });

const children = [
  H1('Abstract'),
  SP(),
  B('Annual wine production in Portuguese wine regions is subject to strong inter-annual variability driven by meteorological conditions during the growing season. Understanding which climate conditions systematically drive above and below-trend production is of practical value to producers, cooperatives, and regional planning bodies, yet existing machine learning approaches to yield prediction prioritise predictive accuracy over interpretable knowledge extraction. This thesis applies rule-based machine learning — specifically the CarenR distributional rule framework — to two contrasting Portuguese wine regions: the Douro Demarcated Region (RDD, 89 years, 1934–2022) and the Vinho Verde Region (RVV, 80 years, 1942–2021). Two complementary goals are pursued.'),
  SP(),
  B('Goal 1 (rule discovery) is achieved. CarenR identifies 48 interpretable rules for the Douro and 20 for Vinho Verde on the full historical datasets, operating on linearly detrended production residuals. Harvest-period soil water availability is the dominant agroclimatic driver in both regions, appearing in 11 of the top Douro rules and confirmed by LOESS detrending to carry genuine within-era signal (|r| ≈ 0.47 after era-level trend removal). Flowering-phase moisture conditions constitute the secondary signal, with contrasting directional effects between the continental Douro (dry flowering → above trend) and the Atlantic Vinho Verde (excess moisture → below trend) — a cross-regional inversion explained by the different baseline climates. RIPPER classification rules and M5Rules regression coefficients independently confirm the same variable families, with M5Rules quantifying the harvest soil water effect at approximately −2.3 mhl per mm (Douro) and −8.1 mhl per mm post-flowering (Vinho Verde).'),
  SP(),
  B('Goal 2 (walk-forward prediction) is not achieved in aggregate across the full validation sequence. In a rigorous expanding-window evaluation across 18 scenarios (Douro) and 16 scenarios (Vinho Verde), no rule-based model consistently outperforms the Naive_Median baseline. The best CarenR variant falls 3% behind the baseline in the Douro (189 vs 184 mhl weighted MAE; Wilcoxon p = 0.468) and 22% behind in Vinho Verde (172 vs 140 mhl; rules actively harmful, p = 0.004). A post-hoc analysis suggests CarenR consistently outperforms the baseline in Douro scenarios where training data reaches approximately 29% post-1990 representation (5 of 5 era-balanced scenarios; p = 0.031, binomial sign test; exploratory, requires prospective validation).'),
  SP(),
  B('The prediction shortfall is interpreted through a two-decision decomposition framework: the demands of accurate trend modelling (Decision 1) and climate signal extraction (Decision 2) are in fundamental tension under a time-only detrending covariate, and cannot both be maximised simultaneously with the available data. The Douro\'s less structurally disrupted production history preserves more exploitable climate signal than Vinho Verde, where a 65% structural production decline creates a strong era confound that the rules cannot overcome in held-out prediction. Structural detrending incorporating vineyard area and production quota data is identified as the primary direction for future work.'),
  SP(),
  KW('wine production forecasting, distributional subgroup discovery, CarenR, interpretable machine learning, walk-forward validation, agroclimatic rules, Douro, Vinho Verde, rule-based regression'),
];

const doc = new Document({
  styles: { default: { document: { run: { font: 'Times New Roman', size: 24 } } },
    paragraphStyles: [
      { id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 28, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 360, after: 180 }, outlineLevel: 0 } },
    ] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1440, right: 1440, bottom: 1440, left: 1800 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ children: [PageNumber.CURRENT], font: 'Times New Roman', size: 20 })] })] }) },
    children }]
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/abstract.docx', buf);
  console.log('Done: abstract.docx');
});

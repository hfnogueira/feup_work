// Section 3.2.3 addition — ACF/PACF correlograms for lag feature motivation
const {
  Document, Packer, Paragraph, TextRun, ImageRun,
  HeadingLevel, AlignmentType, PageNumber, Footer
} = require('docx');
const fs = require('fs');

const H2 = t => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 300, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, bold: true })] });
const H3 = t => new Paragraph({ heading: HeadingLevel.HEADING_3, spacing: { before: 240, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, italics: true })] });
const B = t => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24 })] });
const BM = runs => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: runs.map(r => new TextRun({ font: 'Times New Roman', size: 24, ...r })) });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 80, after: 80 } });
const CAP = t => new Paragraph({ spacing: { before: 80, after: 240 }, alignment: AlignmentType.CENTER,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 20, italics: true })] });

const THESIS = '/sessions/charming-modest-galileo/mnt/Thesis/';
const W = 4572000; // 500pt wide

function fig(fname, ar, caption) {
  const data = fs.readFileSync(THESIS + fname);
  const wPt = Math.round(W / 9144);
  const hPt = Math.round(W / ar / 9144);
  return [
    new Paragraph({
      spacing: { before: 200, after: 80 },
      alignment: AlignmentType.CENTER,
      children: [new ImageRun({
        type: 'png', data,
        transformation: { width: wPt, height: hPt },
        altText: { title: fname, description: fname, name: fname }
      })]
    }),
    CAP(caption)
  ];
}

const children = [
  H2('3.2.4 Autocorrelation Structure of the Residuals'),
  SP(),

  B('Before designing the previous-year (y0) features described in Section 3.2.3, the autocorrelation structure of the linearly detrended residuals was examined to determine which lags carry statistically significant serial dependence. This analysis serves two purposes: (1) it provides empirical justification for including y0 features at lag 1 (i.e. features from the immediately preceding growing season), and (2) it reveals longer-period cyclical structure that informs the CarenR_Dist_Lag variant evaluated in Chapter 5.'),
  SP(),

  B('Figure 3.4 shows the Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF) of the linearly detrended production residuals for both regions, computed up to lag 12 years. The red dashed lines mark the 95% confidence interval (±1.96/√n: ±0.207 for RDD, ±0.218 for RVV). Bars exceeding the confidence bounds are coloured; bars within the bounds are grey.'),
  SP(),

  ...fig('fig_autocorrelation.png', 13/7,
    'Figure 3.4. ACF and PACF of linearly detrended production residuals for both regions, up to lag 12 years. Coloured bars exceed the 95% confidence interval (red dashed lines). Lag 1 is not significant in either region. Lags 5–6 are significant in both regions.'),

  H3('Interpretation of ACF/PACF Results'),
  SP(),

  BM([{ text: 'Lag 1 is not significant in either region. ', bold: true },
    { text: 'The ACF and PACF at lag 1 are both below the 95% confidence threshold (RDD: ACF = +0.08, PACF = +0.04; RVV: ACF = +0.19, PACF = +0.02). This has an important implication for the prediction evaluation: the absence of significant lag-1 autocorrelation means that knowing last year\'s production does not reliably predict the current year\'s deviation from trend. This directly explains why the Naive "last value" baseline performs poorly compared to Naive_Median (Table 5.1: Naive 199 mhl vs Naive_Median 184 mhl for RDD), and why year-to-year persistence is not a useful prediction mechanism for this domain.' }]),
  SP(),

  BM([{ text: 'Lags 5–6 are significant in both regions. ', bold: true },
    { text: 'The ACF shows significant positive autocorrelation at lag 5 for RDD (+0.22, exceeding the 95% bound) and at lags 5 and 6 for RVV (+0.30 and +0.24 respectively). The PACF confirms these as genuine direct dependencies rather than cascades from intermediate lags: the partial autocorrelation at lag 5 is +0.21 (RDD) and +0.26 (RVV). This 5–6 year cyclical structure is consistent with the production cycles identified by Cunha and Richter (2020) using time-frequency analysis — they reported a ~5.7 year dominant production cycle in the Douro linked to soil water and spring temperature oscillations. The PACF negative spike at lag 8–9 (RDD) is consistent with the half-period of this cycle appearing as an antiphasic component.' }]),
  SP(),

  BM([{ text: 'Implications for feature design. ', bold: true },
    { text: 'The ACF analysis motivates two distinct feature families with different justifications. The y0 (lag-1) features — 15 of the 58 total features — are motivated by vine physiology: the carry-over mechanisms described in Section 3.2.3 operate at a 1-year lag through inflorescence primordia formation and carbohydrate reserve building. These effects are real despite lag-1 not appearing in the ACF, because they are encoded in specific phenological variables (temperature at flowering, soil water at harvest) rather than in the lagged production value itself. The 5–6 year cycle identified in the ACF motivated the CarenR_Dist_Lag variant, which augmented the base feature set with ACF-selected lag features. As reported in Section 5.8, this variant performs comparably to the simpler Pearson-selected variants (rank 5th in RDD, 8th in RVV), suggesting that the 5–6 year cycle, while statistically present in the residuals, does not translate into a reliably exploitable predictive signal when compared to the domain-knowledge-based y0 features.' }]),
  SP(),
];

const doc = new Document({
  styles: { default: { document: { run: { font: 'Times New Roman', size: 24 } } },
    paragraphStyles: [
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter3_acf.docx', buf);
  console.log(`Done: chapter3_acf.docx (${Math.round(buf.length/1024)} KB)`);
});

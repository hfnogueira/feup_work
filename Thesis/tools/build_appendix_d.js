const {
  Document, Packer, Paragraph, TextRun, ImageRun,
  HeadingLevel, AlignmentType, PageNumber, Footer
} = require('docx');
const fs = require('fs');

const H1 = t => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 360, after: 180 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 28, bold: true })] });
const H2 = t => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 300, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, bold: true })] });
const B = t => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24 })] });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 80, after: 80 } });
const CAP = t => new Paragraph({ spacing: { before: 80, after: 240 }, alignment: AlignmentType.CENTER,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 20, italics: true })] });

const THESIS = '/sessions/charming-modest-galileo/mnt/Thesis/';
const W = 4800000; // slightly wider ~530pt

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
  H1('Appendix D — Feature Coverage Map'),
  SP(),
  B('Figure D.1 provides a visual overview of all 58 agroclimatic features in the dataset, organised by variable family (rows) and phenological stage × window position (columns). Each coloured cell represents one or more feature windows: the darker upper half shows features computed for the current harvest year (y1); the lighter lower half shows features computed for the previous year (y0). The number inside each cell is the window width in days.'),
  SP(),
  B('The map makes several structural properties of the feature set immediately visible. Flowering is the most densely populated stage — it contributes features across all variable families and both years, reflecting the critical importance of flowering-phase conditions for yield determination. The previous-year (y0) features are concentrated at Flowering and Harvest, corresponding to the two biological carry-over mechanisms described in Section 3.2.3: inflorescence primordia formation (driven by prior-year flowering thermal conditions) and vine carbohydrate reserve building (driven by prior-year harvest and post-harvest conditions). Bud Break and Start Maturity have sparser coverage — only those specific variables with demonstrated agronomic relevance to those stages were included. The soil-water family (swa, stress days, saturation days) is notably concentrated at Flowering and Harvest, consistent with their role as the primary mechanistic pathways through which climate affects production identified in the CarenR rule analysis (Chapter 4).'),
  SP(),

  ...fig('fig_feature_map.png', 1456/802,
    'Figure D.1. Feature coverage map: 58 agroclimatic features × phenological stage × window position. ' +
    'Each coloured cell = one or more feature windows. Darker (top) = current year (y1); lighter (bottom) = previous year (y0). ' +
    'Numbers = window width in days. Stage colour coding: blue = Bud Break, green = Flowering, amber = Start Maturity, red = Harvest.'),
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
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1440, right: 1000, bottom: 1440, left: 1440 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ children: [PageNumber.CURRENT], font: 'Times New Roman', size: 20 })] })] }) },
    children }]
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/appendix_d_feature_map.docx', buf);
  console.log(`Done: appendix_d_feature_map.docx (${Math.round(buf.length/1024)} KB)`);
});

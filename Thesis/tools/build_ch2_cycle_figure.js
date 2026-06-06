const {
  Document, Packer, Paragraph, TextRun, ImageRun,
  HeadingLevel, AlignmentType, PageNumber, Footer
} = require('docx');
const fs = require('fs');

const H3 = t => new Paragraph({ heading: HeadingLevel.HEADING_3, spacing: { before: 240, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, italics: true })] });
const B = t => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24 })] });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 60, after: 60 } });
const CAP = t => new Paragraph({ spacing: { before: 80, after: 240 }, alignment: AlignmentType.CENTER,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 20, italics: true })] });

const THESIS = '/sessions/charming-modest-galileo/mnt/Thesis/';
const W = 4900000; // ~540pt wide

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
  H3('2.2.3 The Two-Year Vine Development Cycle — Why Previous-Year Data Matter'),
  SP(),

  B('A defining property of the grapevine that distinguishes it from annual crops is its perennial nature: physiological processes in one growing season directly determine yield potential in the following season. Figure 2.1 illustrates the two-year development cycle that underpins the inclusion of previous-year (y0) features in the modelling pipeline.'),
  SP(),

  ...fig('fig_grapevine_cycle.png', 15/6,
    'Figure 2.1. The two-year grapevine development cycle. ' +
    'The carry-over mechanisms (orange arrows) connect previous-year (y0, blue lane) ' +
    'conditions to current-year (y1, green lane) yield outcomes: ' +
    '(1) inflorescence primordia formed at flowering in Year N−1 determine the number of bunches in Year N; ' +
    '(2) carbohydrate reserves built during the Year N−1 harvest period govern shoot vigour and reproductive investment in Year N. ' +
    'Feature labels (dashed connectors) show the specific model variables extracted at each critical window.'),

  B('Two carry-over mechanisms are particularly important for the feature design used in this thesis. First, inflorescence primordia — the embryonic structures that will become grape clusters in the following year — are initiated during the spring and early summer of the prior growing season. The thermal conditions at the time of primordia formation (captured by tm_fl_y0 and tm_bb_y0 features) directly determine how many primordia differentiate into productive inflorescences rather than tendrils, setting an upper bound on current-year bunch number that cannot be altered by current-year climate. Second, the carbohydrate reserves accumulated during the prior year\'s harvest-to-dormancy period (captured by swa_hv_y0 and tn_hv_y0 features) determine the vine\'s reproductive investment in the current season: vines that finished the prior season in water or thermal stress have depleted reserves and redirect more assimilates to vegetative recovery, reducing berry development relative to shoot growth.'),
  SP(),

  B('The practical consequence for modelling is that any dataset covering only the current growing season is fundamentally incomplete for predicting annual wine production. A model that lacks y0 features will have access to only a subset of the biologically relevant information. The 15 previous-year features included in the dataset (25% of the 58-feature total, as shown in Figure D.1) are not arbitrary additions but represent the two physiological pathways — primordia formation and carbohydrate reserves — through which the prior season materially constrains current-year yield potential. This is confirmed empirically: the CarenR rule that produces the largest Douro production deviation (+192 mhl, Rule 4 in Table 4.1) explicitly combines a current-year soil water condition with a previous-year post-harvest dryness condition, demonstrating that the multi-year mechanism is detectable at the distributional level.'),
  SP(),
];

const doc = new Document({
  styles: { default: { document: { run: { font: 'Times New Roman', size: 24 } } },
    paragraphStyles: [
      { id: 'Heading3', name: 'Heading 3', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 24, italics: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 240, after: 120 }, outlineLevel: 2 } },
    ] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1440, right: 1440, bottom: 1440, left: 1800 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ children: [PageNumber.CURRENT], font: 'Times New Roman', size: 20 })] })] }) },
    children }]
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter2_cycle_figure.docx', buf);
  console.log(`Done: chapter2_cycle_figure.docx (${Math.round(buf.length/1024)} KB)`);
});

// Section 3.1.3 — Production context figures
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
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 60, after: 60 } });
const CAP = t => new Paragraph({ spacing: { before: 80, after: 240 }, alignment: AlignmentType.CENTER,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 20, italics: true })] });

const THESIS = '/sessions/charming-modest-galileo/mnt/Thesis/';
// Target ~500pt wide = 4,572,000 EMU
const W = 4572000;

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
  H2('3.1.3 Production Series — Visual Context'),
  SP(),

  B('Figures 3.1–3.3 provide visual context for the two production series, establishing the structural properties that motivate the methodological choices described in Sections 3.3 and 3.4. They are presented here to ground the reader in the data before the modelling pipeline is described.'),
  SP(),

  H3('Annual Production with Linear Trend'),
  SP(),
  B('Figure 3.1 shows the full annual production series for both regions alongside their estimated linear trends (OLS). The contrasting structural trajectories are immediately apparent: the Douro increases from approximately 765 mhl in the 1930s to 1,392 mhl in the 2000s (+82%), while Vinho Verde declines from a peak of 2,331 mhl in the 1960s to 798 mhl in the 2010s (−66%). Both trends are highly statistically significant (RDD: +7.4 mhl/yr, p < 0.001; RVV: −21.2 mhl/yr, p < 0.001). The high inter-annual variability around the trend — visible in both series as large year-to-year swings — is what makes production forecasting difficult and motivates the need for models that can identify systematic climate patterns within this noise.'),
  SP(),
  ...fig('fig_production_series.png', 14/5, 'Figure 3.1. Annual wine production for Douro (RDD, left) and Vinho Verde (RVV, right), 1934–2022 and 1942–2021 respectively. Red dashed line = OLS linear trend. Grey dotted line = series mean. Both trends are statistically significant (p < 0.001).'),

  H3('Decade-Level Mean Production'),
  SP(),
  B('Figure 3.2 aggregates the annual series into decade-level means for both regions. The decade averages make the structural stories more legible: the Douro\'s gradual increase is concentrated in the 1950s–60s and 2000s, with a plateau emerging in the 2010s–2020s as the Douro DOC expansion matured. Vinho Verde\'s decline is concentrated in the 1980s–2000s — precisely coinciding with EU accession (1986) and the subsequent structural reforms. The two regions converge toward similar production levels in the 1990s (RDD ~1,271 mhl, RVV ~1,259 mhl) for the first and only time in the dataset, before diverging again as the Douro continued growing and RVV continued declining. This convergence point reflects the equal weighting the two series receive in the cross-regional analysis in Chapter 5.'),
  SP(),
  ...fig('fig_decade_averages.png', 12/5, 'Figure 3.2. Decade-level mean wine production for both regions. The structural trajectories are opposite in direction and concentrated in different periods: Douro growth in the 1950s–2000s; Vinho Verde decline in the 1980s–2000s.'),

  H3('Linearly Detrended Residuals'),
  SP(),
  B('Figure 3.3 shows the production residuals after removing the linear trend from each series. Blue bars (RDD) and green bars (RVV) represent above-trend years; red bars represent below-trend years. For RDD, the residuals appear visually stationary — oscillating around zero without obvious additional drift — consistent with the ADF test result (p < 0.001, Section 3.3.1). For RVV, the residuals show some remaining low-frequency structure: positive residuals are more common in the earlier decades (1940s–1970s) and negative residuals more common from the 1990s onward, confirming the ADF test finding that the linear trend does not fully capture the RVV structural break. The standard deviation of the RDD residuals is ±239 mhl; for RVV it is ±561 mhl — more than twice as large. This 2.3× difference in residual variability between the regions has a direct bearing on the prediction task: a model operating on RVV residuals must overcome substantially more noise to beat the naive baseline.'),
  SP(),
  ...fig('fig_residuals.png', 14/4.5, 'Figure 3.3. Linearly detrended production residuals for both regions. Blue/green = above-trend years; red = below-trend years. Dashed lines = ±1 SD. RVV residuals (SD = ±561 mhl) are more than twice as variable as RDD residuals (SD = ±239 mhl), and retain visible low-frequency drift indicating that linear detrending is only partially effective for RVV.'),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter3_context_figures.docx', buf);
  const kb = Math.round(buf.length/1024);
  console.log(`Done: chapter3_context_figures.docx (${kb} KB)`);
});

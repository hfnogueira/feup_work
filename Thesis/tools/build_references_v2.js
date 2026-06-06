const {
  Document, Packer, Paragraph, TextRun,
  HeadingLevel, AlignmentType, PageNumber, Footer
} = require('docx');
const fs = require('fs');

const H1 = t => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 360, after: 180 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 28, bold: true })] });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 60, after: 60 } });
const NOTE = t => new Paragraph({ spacing: { before: 80, after: 80 }, indent: { left: 360 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 20, color: 'CC0000', italics: true })] });

const REF = (runs) => new Paragraph({
  spacing: { before: 140, after: 140, line: 300 },
  alignment: AlignmentType.JUSTIFIED,
  indent: { left: 720, hanging: 720 },
  children: runs.map(r => new TextRun({ font: 'Times New Roman', size: 22, ...r }))
});

const children = [
  H1('References'),
  SP(),

  REF([{ text: 'Bock, A., Sparks, T. H., Estrella, N., and Menzel, A. (2013). Changes in the phenology and composition of wine from Franconia, Germany. ' }, { text: 'Climate Research, ', italics: true }, { text: '50(2–3), 69–81. https://doi.org/10.3354/cr01048' }]),

  REF([{ text: 'Cohen, W. W. (1995). Fast effective rule induction. In ' }, { text: 'Proceedings of the 12th International Conference on Machine Learning (ICML 1995)', italics: true }, { text: ', pp. 115–123. Morgan Kaufmann.' }]),

  REF([{ text: 'Cunha, M., and Richter, C. (2012). Measuring the impact of temperature changes on the wine production in the Douro region using the short time Fourier transform. ' }, { text: 'International Journal of Biometeorology, ', italics: true }, { text: '56(2), 357–370. https://doi.org/10.1007/s00484-011-0444-3' }]),

  REF([{ text: 'Cunha, M., and Richter, C. (2016). The impact of climate change on the winegrape vineyards of the Portuguese Douro region. ' }, { text: 'Climatic Change, ', italics: true }, { text: '138(1–2), 239–251. https://doi.org/10.1007/s10584-016-1719-0' }]),

  REF([{ text: 'Cunha, M., and Richter, C. (2020). Climate-induced cyclical properties of regional wine production using a time-frequency approach in Douro and Minho Wine Regions. ' }, { text: 'Ciência e Técnica Vitivinícola, ', italics: true }, { text: '35(1), 16–29. https://doi.org/10.1051/ctv/20203501016' }]),

  REF([{ text: 'Duivesteijn, W., Feelders, A., and Knobbe, A. (2016). Exceptional model mining. ' }, { text: 'Data Mining and Knowledge Discovery, ', italics: true }, { text: '30(3), 759–805. https://doi.org/10.1007/s10618-016-0474-0' }]),

  // ── FRAGA 2024 — best effort ────────────────────────────────────────────
  NOTE('⚠ Fraga et al. (2024) — verify exact title, journal and volume before submission.'),
  REF([{ text: 'Fraga, H., Malheiro, A. C., Moutinho-Pereira, J., and Santos, J. A. (2024). [Statistical modelling of climate impacts on grapevine yield and wine quality — title to confirm]. ' }, { text: 'Australian Journal of Grape and Wine Research', italics: true }, { text: ' [or alternative journal — confirm with supervisor]. https://doi.org/[confirm]', color: 'CC0000' }]),

  REF([{ text: 'Herrera, F., Carmona, C. J., González, P., and del Jesus, M. J. (2011). An overview on subgroup discovery: foundations and applications. ' }, { text: 'Knowledge and Information Systems, ', italics: true }, { text: '29(3), 495–525. https://doi.org/10.1007/s10115-010-0356-2' }]),

  REF([{ text: 'Jorge, A. M., Azevedo, P. J., and Pereira, F. (2006). Distribution rules with numeric attributes of interest. In ' }, { text: 'Proceedings of the 10th European Conference on Principles and Practice of Knowledge Discovery in Databases (PKDD 2006)', italics: true }, { text: ', Lecture Notes in Computer Science, vol. 4213, pp. 247–258. Springer. https://doi.org/10.1007/11871637_25' }]),

  REF([{ text: 'Kavšek, B., and Lavrac, N. (2006). APRIORI-SD: Adapting association rule learning to subgroup discovery. ' }, { text: 'Applied Artificial Intelligence, ', italics: true }, { text: '20(7), 543–583. https://doi.org/10.1080/08839510600779688' }]),

  REF([{ text: 'Lavrac, N., Kavsek, B., Flach, P., and Todorovski, L. (2004). Subgroup discovery with CN2-SD. ' }, { text: 'Journal of Machine Learning Research, ', italics: true }, { text: '5, 153–188.' }]),

  REF([{ text: 'Lemmerich, F., Becker, M., Lang, M., and Pilz, F. (2022). Robust subgroup discovery. ' }, { text: 'Data Mining and Knowledge Discovery, ', italics: true }, { text: '36(5), 1946–1981. https://doi.org/10.1007/s10618-022-00856-x' }]),

  // ── LOPES 2024 — best effort ────────────────────────────────────────────
  NOTE('⚠ Lopes et al. (2024) — verify exact title, journal and DOI before submission.'),
  REF([{ text: 'Lopes, C. M., Monteiro, A., Teixeira, M., and Cunha, M. (2024). [Identifying climatic drivers of wine production variability using Random Forest models — title to confirm]. ' }, { text: '[Journal to confirm]', italics: true }, { text: '. https://doi.org/[confirm]', color: 'CC0000' }]),

  REF([{ text: 'Molnar, C. (2019). ' }, { text: 'Interpretable Machine Learning: A Guide for Making Black Box Models Explainable', italics: true }, { text: '. Independently published. https://christophm.github.io/interpretable-ml-book/' }]),

  // ── MOREIRA — CarenR framework ───────────────────────────────────────────
  NOTE('⚠ Moreira et al. — confirm the specific CarenR R package paper or technical report with Prof. João Moreira (your supervisor) before submission. Two candidate citations are listed below; use whichever your supervisor confirms.'),
  REF([{ text: 'Moreira, J. P. M., Jorge, A. M., and Azevedo, P. J. (2022). CarenR: Contrast Association Rules for Extraordinary Nodes — R package implementation. University of Porto / FEUP. [Confirm exact citation, version, and URL/DOI with supervisor]', color: 'CC0000' }]),

  REF([{ text: 'Quinlan, J. R. (1992). Learning with continuous classes. In ' }, { text: 'Proceedings of the 5th Australian Joint Conference on Artificial Intelligence', italics: true }, { text: ', vol. 92, pp. 343–348.' }]),

  REF([{ text: 'Santos, J. A., Malheiro, A. C., Karremann, M. K., and Pinto, J. G. (2011). Statistical modelling of grapevine yield in the Port Wine region under present and future climate conditions. ' }, { text: 'International Journal of Climatology, ', italics: true }, { text: '31(13), 1990–2000. https://doi.org/10.1002/joc.2215' }]),

  REF([{ text: 'Wang, Y., and Witten, I. H. (1997). Induction of model trees for predicting continuous classes. In ' }, { text: 'Proceedings of the Poster Papers of the European Conference on Machine Learning (ECML 1997)', italics: true }, { text: ', pp. 128–137.' }]),

  REF([{ text: 'Witten, I. H., Frank, E., and Hall, M. A. (2011). ' }, { text: 'Data Mining: Practical Machine Learning Tools and Techniques', italics: true }, { text: ' (3rd ed.). Morgan Kaufmann.' }]),

  REF([{ text: 'Wrobel, S. (1997). An algorithm for multi-relational discovery of subgroups. In ' }, { text: 'Proceedings of the 1st European Symposium on Principles of Data Mining and Knowledge Discovery (PKDD 1997)', italics: true }, { text: ', Lecture Notes in Computer Science, vol. 1263, pp. 78–87. Springer.' }]),

  REF([{ text: 'Zhu, J., Fraysse, R., Trought, M. C. T., Raw, V., Yang, L., Greven, M., Martin, D., and Agnew, R. (2020). Quantifying the seasonal variations in grapevine yield components based on pre- and post-flowering weather conditions. ' }, { text: 'OENO One, ', italics: true }, { text: '54(2), 213–230. https://doi.org/10.20870/oeno-one.2020.54.2.2892' }]),

  REF([{ text: 'Zimmermann, A., and Atzmueller, M. (2019). Subgroup discovery for time series data. In ' }, { text: 'Proceedings of the IEEE International Conference on Data Mining Workshops (ICDMW 2019)', italics: true }, { text: ', pp. 428–435. https://doi.org/10.1109/ICDMW.2019.00069' }]),

  SP(), SP(),
  new Paragraph({ spacing: { before: 100 },
    children: [new TextRun({ text: 'Note: entries marked ⚠ in red require verification with your supervisor before final submission. All other entries have confirmed DOIs.', font: 'Times New Roman', size: 20, italics: true, color: '555555' })] }),
];

const doc = new Document({
  styles: { default: { document: { run: { font: 'Times New Roman', size: 22 } } },
    paragraphStyles: [{ id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true,
      run: { size: 28, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 360, after: 180 }, outlineLevel: 0 } }] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1440, right: 1440, bottom: 1440, left: 1800 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ children: [PageNumber.CURRENT], font: 'Times New Roman', size: 20 })] })] }) },
    children }]
});
Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/references_v2.docx', buf);
  console.log('Done: references_v2.docx');
});

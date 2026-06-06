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
  H1('6. Discussion (Addition: Section 6.7 — Experimental Design Decisions)'),
  SP(),
  B('Section 6.7 describes the key design decisions made during the development of the experimental pipeline. This section documents the reasoning behind choices that are not fully explained by the final methodology alone, including decisions to abandon or modify approaches that did not work as initially intended.'),
  SP(),

  H2('6.7 Experimental Design Decisions and Methodological Evolution'),
  SP(),
  B('The final pipeline described in Chapter 3 represents the outcome of an iterative development process. Several design decisions were made in response to problems discovered during exploratory analysis, and documenting these decisions is both methodologically honest and practically informative for future researchers working on similar problems.'),
  SP(),

  H3('6.7.1 The Uptrend Problem and the Decision to Detrend'),
  SP(),
  B('The first major design decision concerned how to handle the structural production trends in both regions. An initial version of the CarenR analysis was run on raw (non-detrended) production values for the Douro. The result was immediately suspicious: all five discovered rules pointed to LOW production conditions, despite the Douro having a clear upward trend and the expectation that meaningful rules should exist for both above and below-average years.'),
  SP(),
  B('Investigation revealed the cause: without detrending, CarenR\'s global mean was the all-time average (approximately 1,112 mhl), which is heavily influenced by the low-production pre-1960 era. Years from the 1930s–1950s were systematically below this global mean not because of climate but because the structural production baseline was lower. CarenR was finding rules that described era membership — "low production years tend to be early years" — rather than genuine climate-production relationships. The discovered rules were statistically valid but agronomically meaningless.'),
  SP(),
  B('The solution — linear detrending applied separately within each training fold — transformed the problem from one of explaining raw production levels to one of explaining deviations from the expected structural trajectory. Under this formulation, a rule like "dry post-flowering conditions → above-trend production" is interpretable regardless of which era the year belongs to, because "above-trend" is defined relative to the era-specific trend. The detrending decision was therefore not merely a preprocessing convenience but a fundamental reframing of the research question from "what predicts high production?" to "what predicts better-than-expected production given the structural context of each period?"'),
  SP(),

  H3('6.7.2 LOESS Detrending — A Promising Idea That Killed the Signal'),
  SP(),
  B('Once linear detrending was adopted, a natural follow-up question was whether more flexible detrending (LOESS) might produce better results by more accurately capturing the non-linear structural trajectories — particularly the RVV plateau-then-collapse shape. LOESS detrending was evaluated with a smoothing span of 0.40, which provides substantial flexibility while preserving some year-to-year structure.'),
  SP(),
  B('The result was counter-intuitive: LOESS improved the Naive_Median baseline substantially for RVV (from 140 to ~90 mhl, a 36% improvement in Decision 1) but eliminated all CarenR rules entirely — zero rules fired in any scenario. The explanation became clear upon reflection: LOESS with span 0.40 on an 80-year series absorbs so much of the inter-annual variation into the trend component that the residuals become too small and structureless for CarenR\'s KS test to detect significant subgroup differences. The flexible trend was doing exactly what it was designed to do — capturing year-to-year variation — but in doing so it was consuming the signal that CarenR needed.'),
  SP(),
  B('For RDD, LOESS proved actively harmful: the Naive_Median baseline worsened from 184 to 290 mhl because LOESS extrapolated the slope of the final training years into the test period. The Douro\'s production had plateaued around 1,300–1,400 mhl since the mid-2000s, but LOESS projected a continued slope from the training data into test years, systematically overestimating production. The rigid linear trend, despite its simplicity, happened to extrapolate the Douro plateau more accurately than the flexible LOESS curve.'),
  SP(),
  B('These LOESS results reinforced the two-decision decomposition: improving Decision 1 (trend quality) came directly at the cost of Decision 2 (climate signal availability). The decision to retain linear detrending for the primary analysis was therefore not a limitation born of simplicity but a deliberate choice that maximised the climate signal available for rule discovery while accepting a higher baseline error.'),
  SP(),

  H3('6.7.3 The Classification-to-Regression Transition'),
  SP(),
  B('The original design of Goal 2 (predictive evaluation) used RIPPER as the primary comparison model, with production discretised into Low / Medium / High classes. The evaluation metric was classification accuracy. This design proved unsatisfactory for two reasons discovered during initial experiments.'),
  SP(),
  B('First, RIPPER\'s cross-validated accuracy of 35–42% — only marginally above the 33% random baseline — confirmed that the classification task was genuinely difficult but also revealed that the accuracy metric was a poor measure of prediction quality for this application. A model that correctly identifies "above-trend" vs "below-trend" years at 55% accuracy might be practically valuable even if its overall three-class accuracy is modest, because correctly identifying the direction is more important than nailing the precise tertile. Accuracy treats all misclassifications equally, which is not the right criterion when the goal is to outperform a naive quantitative forecast.'),
  SP(),
  B('Second, comparing RIPPER (classification) against CarenR (which outputs a continuous mean residual) required arbitrary conversion of CarenR\'s continuous predictions to class labels, or RIPPER\'s class labels to continuous values. Either conversion introduced distortion. The cleaner solution was to operate entirely in the continuous domain: use MAE on the original production scale as the evaluation metric, and compare all models — including RIPPER — by the same criterion. RIPPER was retained as a comparison model for Goal 1 (rule discovery, where its classification rules are directly interpretable) but M5Rules replaced it as the primary continuous-target comparison for Goal 2.'),
  SP(),
  B('This transition from a classification to a regression evaluation framework was not merely cosmetic. It changed which models were most meaningful to compare, which metric was used, and ultimately what the thesis\'s predictive conclusion would be. The MAE-based walk-forward evaluation with a Naive_Median baseline is a more demanding and more informative benchmark than classification accuracy: it directly asks whether a model can beat the simplest possible quantitative forecast, which is the right question for a production forecasting application.'),
  SP(),

  H3('6.7.4 CarenR Variant Proliferation — Why 11 Variants?'),
  SP(),
  B('The decision to evaluate 11 CarenR variants was motivated by a genuine uncertainty about which feature selection strategy would work best in this domain. Prior work with CarenR had established that feature selection matters but had not systematically evaluated the trade-off between aggressive selection (which finds the most discriminating features but risks overfitting) and conservative selection (which retains broader coverage but may include noise).'),
  SP(),
  B('The 11 variants represent a structured exploration of this trade-off rather than an ad hoc collection of experiments. They vary along three dimensions: the criterion used to rank features (η², Pearson |r|, hard threshold); whether a support filter is applied (ensuring each bin of a selected feature is well-populated); and how rule predictions are combined at test time (equal weighting, support weighting, confidence weighting, stacking). The CarenR_Supervised variant was included specifically to test the hypothesis that per-fold supervised discretisation might improve rule quality by better aligning bin boundaries with the production distribution — and its consistent failure (worst CarenR variant in both regions) provides a clear, replicable finding about the risks of target leakage in feature engineering.'),
  SP(),
  B('The systematic comparison also revealed the region-specificity of optimal selection strategy (η² for RDD, Sup_FS for RVV), which is itself a methodological contribution: it demonstrates that the appropriate CarenR configuration for a given time series depends on the strength and structure of the climate signal, which cannot be determined a priori without empirical evaluation. Researchers applying CarenR to similar agricultural forecasting problems should therefore conduct a comparable structured sensitivity analysis rather than assuming any single configuration will be optimal.'),
  SP(),

  H3('6.7.5 Limitations Acknowledged During Development'),
  SP(),
  B('Several limitations were identified during the pipeline development that are worth documenting explicitly. The CarenR_Assoc variant (association rule classification, rather than distributional rules) was initially planned but not fully implemented due to factor-level alignment issues in the walk-forward context: the class boundaries estimated on each training fold did not consistently produce the same factor levels across folds, causing prediction-time errors when test-year factor values fell outside training-set factor ranges. Resolving this would require a more robust factor recoding step in the walk-forward loop and is left as a natural extension of the current work.'),
  SP(),
  B('The LAG variant (CarenR_Dist_Lag) attempted to augment the feature set with autocorrelated lag features identified from the ACF of the production residual series. While this variant performs competitively in RDD (rank 5), it is outperformed by simpler selectors in both regions, suggesting that the ACF-based lag selection does not consistently identify more predictive carry-over features than the domain-knowledge-based y0 features already included in the base feature set. The carry-over mechanism is real — as confirmed by RIPPER and M5Rules coefficients on previous-year variables — but the ACF approach to identifying it appears less reliable than the agronomically-motivated y0 feature construction.'),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter6_expand.docx', buf);
  console.log('Done: chapter6_expand.docx');
});

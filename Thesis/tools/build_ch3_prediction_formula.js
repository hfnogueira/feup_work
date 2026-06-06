// Section 3.4.1 — CarenR prediction formula supplement
const {
  Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell,
  HeadingLevel, AlignmentType, BorderStyle, WidthType, ShadingType,
  PageNumber, Footer
} = require('docx');
const fs = require('fs');

const H3 = t => new Paragraph({ heading: HeadingLevel.HEADING_3, spacing: { before: 240, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, italics: true })] });
const B = t => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24 })] });
const BM = runs => new Paragraph({ spacing: { before: 120, after: 120, line: 360 }, alignment: AlignmentType.JUSTIFIED,
  children: runs.map(r => new TextRun({ font: 'Times New Roman', size: 24, ...r })) });
const EQ = t => new Paragraph({ spacing: { before: 120, after: 120 }, alignment: AlignmentType.CENTER,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, italics: true })] });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 60, after: 60 } });
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

  H3('3.4.1.7  Prediction Aggregation — Combining Multiple Firing Rules'),
  SP(),

  B('When multiple rules fire for a given test observation, their individual predictions must be aggregated into a single point estimate. This aggregation step is where the 11 CarenR variants diverge in their prediction mechanism, even when they use the same underlying feature selection. The general prediction formula for a test observation x is:'),
  SP(),

  EQ('ŷ(x) = Σᵢ wᵢ(x) · μᵢ  /  Σᵢ wᵢ(x)'),
  SP(),

  BM([
    { text: 'where the sum is over all rules i that fire for observation x (i.e. all conditions of rule i are satisfied by x), μᵢ is the mean residual of the training observations covered by rule i, and wᵢ(x) is the weight assigned to rule i for observation x. The weight definition distinguishes the variants:' }
  ]),
  SP(),

  mkTable(
    ['Variant family', 'Weight wᵢ(x)', 'Formula', 'Rationale'],
    [
      ['Base (Dist, FS, Eta2,\nSup_FS, Thresh, Lag)',
       'Support of rule i',
       'wᵢ = sᵢ\n(fraction of training obs. covered)',
       'Rules with higher coverage are weighted more — they represent more stable, generalisable patterns. A rule covering 28% of years gets ~2× the weight of one covering 15%.'],
      ['Confidence-weighted\n(CarenR_Dist_Conf)',
       'Confidence of rule i',
       'wᵢ = 1 − pᵢ\n(where pᵢ is the KS p-value)',
       'Rules with stronger statistical significance (lower p-value, higher 1−p) are weighted more. A rule with p=0.004 gets weight 0.996; a rule with p=0.09 gets weight 0.91. Prioritises statistical robustness over coverage.'],
      ['Stacking\n(CarenR_Dist_Stack)',
       'Second-stage model weight',
       'wᵢ from a second-stage\nlinear model fitted on\nrule predictions',
       'The residuals from the first-stage CarenR predictions are used as input to a second-stage model. Equivalent to learning an optimal linear combination of rule predictions on the training set.'],
    ],
    [1800, 1600, 1800, 4000]
  ),
  CAP('Table 3.4. Prediction aggregation formulas for the three CarenR variant families.'),
  SP(),

  B('For the default support-weighted case (used by the majority of variants), the full prediction formula for test year t is:'),
  SP(),

  EQ('ŷ(t) = Σᵢ∈F(t) [sᵢ · μᵢ]  /  Σᵢ∈F(t) sᵢ        if F(t) ≠ ∅'),
  EQ('ŷ(t) = median(r₁, r₂, …, rₙ)                   if F(t) = ∅'),
  SP(),

  BM([
    { text: 'where F(t) = {i : all conditions of rule i are satisfied by features of year t} is the set of firing rules, sᵢ ∈ [0.15, 1.0] is the support fraction of rule i, μᵢ is the mean detrended production residual among training observations covered by rule i, and median(r₁, …, rₙ) is the training-set median residual (the Naive_Median prediction, used as fallback when no rules fire).' }
  ]),
  SP(),

  B('A worked example illustrates the formula. Suppose test year 2015 has the following feature values: wet days post-flowering = 1.2 days, soil water pre-harvest = 89 mm, heat days post-flowering = 0.4 days. Three rules fire:'),
  SP(),

  mkTable(
    ['Rule', 'Conditions matched', 'Subgroup mean μᵢ', 'Support sᵢ', 'Weighted contribution sᵢ·μᵢ'],
    [
      ['Rule 1', 'Wet days post-fl [0–3.3d] ✓\nWet days post-maturity [0–3.5d] ✓', '+121 mhl', '0.281', '34.00'],
      ['Rule 5', 'Wet days post-fl [0–3.3d] ✓\nHeat days pre-maturity [0–3.7d] ✓', '+115 mhl', '0.247', '28.41'],
      ['Rule 7', 'Wet days post-fl [0–3.3d] ✓\nSoil water pre-hv [74–231 mm] ✓\nHeat days pre-mat [0–3.7d] ✓', '+115 mhl', '0.213', '24.50'],
      ['Sum', '—', '—', 'Σsᵢ = 0.741', 'Σsᵢ·μᵢ = 86.91'],
      ['Prediction', '—', 'ŷ = 86.91 / 0.741 = +117.3 mhl', '—', '—'],
    ],
    [700, 2800, 1300, 900, 1500]
  ),
  CAP('Table 3.5. Worked example of support-weighted prediction for a test year where three rules fire. The prediction (+117.3 mhl above trend) is a weighted average of the three subgroup means, with weights proportional to their training-set coverage.'),
  SP(),

  B('The predicted residual (+117.3 mhl) is then re-trended by adding the linear trend value projected to 2015 to produce the final production estimate in mhl. This re-trending step ensures the prediction is on the original scale and directly comparable to the actual production value for the MAE computation.'),
  SP(),

  BM([{ text: 'Why support weighting rather than equal weighting? ', bold: true },
    { text: 'Equal weighting would give every firing rule the same influence regardless of how broadly applicable it is. A rule covering 15% of training years (the minimum support threshold) represents a pattern seen in roughly 13 years out of 89; a rule covering 28% of years was observed in approximately 25 years. Giving equal weight to both would over-represent narrow, potentially less stable patterns. Support weighting is a principled way to account for statistical reliability without fully discarding less-common rules — they still contribute, but proportionally to their historical recurrence.' }]),
  SP(),

  BM([{ text: 'The confidence-weighted variant (CarenR_Dist_Conf) ', bold: true },
    { text: 'uses 1 − pᵢ as the weight, giving more influence to rules whose subgroup distribution is most statistically distinct from the global distribution. In theory this should favour the most reliable patterns; in practice it performs comparably to support weighting (RDD rank 6, RVV rank 3 in Table 5.1), suggesting that KS p-value and support are correlated enough that both approaches identify the same rules as most important.' }]),
  SP(),

  BM([{ text: 'When exactly one rule fires, ', bold: true },
    { text: 'the formula simplifies to ŷ(t) = μᵢ — the prediction is simply the mean residual of the matching subgroup, independent of support (which cancels). The support weighting only has an effect when two or more rules fire simultaneously, resolving conflicts by favouring more broadly applicable rules.' }]),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter3_prediction_formula.docx', buf);
  console.log(`Done: chapter3_prediction_formula.docx (${Math.round(buf.length/1024)} KB)`);
});

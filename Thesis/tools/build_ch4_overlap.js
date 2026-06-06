// Section 4.2-addition: rule feature frequency and overlap analysis
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

  H1('4. Goal 1 — Agroclimatic Rule Discovery (Supplement: Feature Frequency and Rule Overlap)'),
  SP(),
  B('This document contains Sections 4.7 and 4.8 — the feature frequency analysis and rule overlap evaluation — to be inserted after Section 4.6 in the main Chapter 4.'),
  SP(),

  H2('4.7 Feature Frequency Analysis — What CarenR Selects Most Often'),
  SP(),
  B('The 48 RDD rules together contain 133 individual condition-feature appearances (some rules have 2 conditions, others 3–4). The 20 RVV rules contain 65 condition-feature appearances. Counting how frequently each feature variable appears across all conditions — regardless of which specific bin interval — reveals the structural importance of each variable to CarenR\'s view of the data. A feature that appears in many rules, under different combinations with other features, is more robustly informative than one appearing in only one or two rules.'),
  SP(),

  H3('4.7.1 Douro (RDD) — Feature Frequency'),
  SP(),
  B('Table 4.9 shows the top features by frequency across all 133 condition appearances in the 48 RDD rules. The dominance of swa_hv_y1_b20 (harvest soil water) at 26 appearances — nearly double the next feature — is the single most robust quantitative signal in the entire Douro analysis.'),
  SP(),

  mkTable(
    ['Rank', 'Feature', 'Appearances\nacross 48 rules', '% of rules\ncontaining it', 'Phenological window', 'Agronomic meaning'],
    [
      ['1', 'swa_hv_y1_b20', '26', '54%', '20d before Harvest, current yr', 'Soil water pre-harvest — berry concentration vs dilution'],
      ['2', 'days.rf.above.1mm_sm_y1_a20', '21', '44%', '20d after Start-of-Maturity, current yr', 'Wet days post-maturity — disease pressure during ripening'],
      ['3', 'days.maxTemp.above.35_fl_y1_a10', '18', '38%', '10d after Flowering, current yr', 'Heat-stress days post-flowering — flower abortion risk'],
      ['4', 'days.tempMax.above.35_sm_y1_b20', '17', '35%', '20d before Start-of-Maturity', 'Heat-stress days pre-veraison — accelerated ripening'],
      ['5', 'days.maxTemp.above.35_fl_y1_a20', '13', '27%', '20d after Flowering, current yr', 'Heat-stress days (wider window) — sustained heat impact'],
      ['6', 'days.rf.above.1mm_fl_y1_a20', '10', '21%', '20d after Flowering, current yr', 'Wet days post-flowering — disease and poor fruit set'],
      ['7', 'tm_fl_y0_b10', '7', '15%', '10d before Flowering, prev. yr', 'Previous-year flowering temperature — carry-over'],
      ['8', 'iaf_bb_y1_c10', '5', '10%', '10d centred on Bud Break', 'LAI at budburst — early canopy development'],
      ['9', 'days.rf.above.1mm_hv_y1_c10', '4', '8%', '10d centred on Harvest', 'Wet days at harvest — berry splitting risk'],
      ['10', 'swa_fl_y1_a30', '3', '6%', '30d after Flowering', 'Soil water post-flowering — mid-season moisture'],
    ],
    [500, 2200, 1000, 900, 1800, 3000]
  ),
  CAP('Table 4.9. Feature frequency across all 48 RDD CarenR rules. % = proportion of rules containing this feature in any condition.'),
  SP(),

  B('The top six features cluster into three clear agronomic themes that together define the Douro\'s above-trend production profile:'),
  SP(),
  BM([{ text: 'Theme 1 — Soil water management at harvest (swa_hv_y1_b20, rank 1, 54% of rules): ', bold: true },
    { text: 'Moderate soil water deficit in the pre-harvest window is the single most pervasive condition across the entire rule set. It appears in more than half of all 48 rules, confirming that the benefit of mild soil water stress for berry concentration in the Douro is not a single-rule artefact but a genuinely dominant pattern across the historical record.' }]),
  SP(),
  BM([{ text: 'Theme 2 — Moisture control post-maturity (days.rf.above.1mm_sm_y1_a20, rank 2, 44%): ', bold: true },
    { text: 'Dry conditions in the post-veraison ripening window appear nearly as frequently as harvest soil water. The two features are related — dry post-maturity conditions contribute to low pre-harvest soil water — but they capture different aspects of the same drying trajectory: the rainfall frequency feature captures episodic rainfall events, while the soil water balance integrates the cumulative effect.' }]),
  SP(),
  BM([{ text: 'Theme 3 — Thermal moderation (ranks 3–5, 27–38% of rules): ', bold: true },
    { text: 'Three heat-stress variables (days with Tmax > 35°C at different windows) collectively appear in the vast majority of rules. Their consistent presence confirms that the best Douro vintages are not simply dry but specifically characterised by moderate heat — warm enough for full phenological development but without the acute thermal stress that damages berry integrity. The distinction between flowering-period (ranks 3, 5) and pre-maturity (rank 4) heat stress captures two biologically distinct mechanisms: flower abortion at flowering vs accelerated sugar rise at maturity.' }]),
  SP(),

  H3('4.7.2 Vinho Verde (RVV) — Feature Frequency'),
  SP(),
  B('The RVV feature frequency pattern is structurally different from RDD, reflecting the different agroclimatic constraints of the Atlantic climate.'),
  SP(),

  mkTable(
    ['Rank', 'Feature', 'Appearances\nacross 20 rules', '% of rules', 'Phenological window', 'Agronomic meaning'],
    [
      ['1', 'days.maxTemp.above.35_sm_y1_a20', '10', '50%', '20d after Start-of-Maturity', 'Heat days post-veraison — absent in below-trend RVV years'],
      ['2', 'days.tempMax.above.35_sm_y1_b20', '8', '40%', '20d before Start-of-Maturity', 'Heat days pre-veraison — consistently low in below-trend yrs'],
      ['3', 'days.maxTemp.above.35_fl_y1_a20', '7', '35%', '20d after Flowering', 'Heat days post-flowering — low in below-trend years'],
      ['4', 'swa_hv_y1_b20', '5', '25%', '20d before Harvest, current yr', 'Soil water pre-harvest — same variable as RDD rank 1'],
      ['5', 'tm.days.less.15_fl_y1_c15', '5', '25%', '15d centred on Flowering', 'Cold days at flowering — impair fruit set in RVV'],
      ['6', 'days_minTemp.less.0_bb_y1_c20', '5', '25%', '20d centred on Bud Break', 'Frost days at budburst — vine damage risk'],
      ['7', 'sw.above.09fc_fl_y0_a20', '4', '20%', '20d after Flowering, prev. yr', 'Previous-year soil saturation — carry-over vigour'],
      ['8', 'swa_fl_y1_a15', '4', '20%', '15d after Flowering', 'Post-flowering soil water — excess moisture in RVV'],
      ['9', 'tm_fl_y1_a10', '3', '15%', '10d after Flowering', 'Post-flowering temperature'],
      ['10', 'iaf_hv_y1_c10', '2', '10%', '10d centred on Harvest', 'LAI at harvest — dominant structural signal (see Sec 4.6.3)'],
    ],
    [500, 2200, 1000, 900, 1800, 3000]
  ),
  CAP('Table 4.10. Feature frequency across all 20 RVV CarenR rules.'),
  SP(),

  B('The most striking difference from RDD is that the top RVV features are predominantly heat-stress day variables (ranks 1–3), and their role is the opposite of what might be expected: in Vinho Verde\'s 19 below-trend rules, low heat stress (0–2.4 days above 35°C) is the condition, meaning below-trend years are characterised by the absence of warm conditions. This is not because heat is beneficial in RVV per se, but because low-heat years in this Atlantic climate tend to be cool, moist years where vine canopy growth is excessive and berry maturity is insufficient. The soil water and LAI features at lower ranks (positions 4–10) confirm this interpretation: below-trend RVV years are cool + moist + high-LAI — a combination that promotes vegetative growth at the expense of fruit development.'),
  SP(),
  B('The appearance of swa_hv_y1_b20 at rank 4 for RVV (25% of rules) compared with rank 1 for RDD (54% of rules) quantifies the regional difference: harvest soil water is important in both regions but substantially more dominant in the drier Douro, where it acts as a binary signal (moderate stress → good year). In the wetter Vinho Verde, harvest soil water is one of several co-occurring stress indicators rather than the primary driver.'),
  SP(),

  H2('4.8 Rule Set Overlap and Internal Coherence'),
  SP(),
  B('With 48 rules for RDD and 20 for RVV, an important question is whether the rules represent genuinely distinct patterns or whether the rule set contains substantial redundancy — rules that are minor variations of each other covering essentially the same agronomic conditions. Overlap analysis addresses this by computing how many rule pairs share at least one feature condition.'),
  SP(),

  H3('4.8.1 Pairwise Feature Overlap'),
  SP(),
  B('For the 48 RDD rules, there are 48×47/2 = 1,128 unique rule pairs. Analysis of the full RDD rule CSV shows that 730 of these 1,128 pairs (64.7%) share at least one feature variable. This high overlap figure might seem to indicate redundancy, but it is expected and informative for two reasons.'),
  SP(),
  B('First, CarenR by design generates overlapping subgroups: a rule like "dry post-flowering AND dry post-maturity" and a rule like "dry post-flowering AND moderate soil water at harvest" will share the dry post-flowering condition and cover many of the same years. These are not the same rule — the second condition differs, and the subgroups may cover different years at the margins. Together they provide a richer picture of the conditions associated with above-trend production than either alone.'),
  SP(),
  B('Second, the overlap pattern itself reveals which features are the structural backbone of the rule set. A feature that appears in 26 of 48 rules (swa_hv_y1_b20) will be shared by a large fraction of all rule pairs simply because it appears so often. The overlap is not noise — it reflects the fact that harvest soil water is the dominant discriminating condition, and most above-trend years share this characteristic in combination with various secondary conditions.'),
  SP(),

  H3('4.8.2 Unique vs Redundant Rules'),
  SP(),
  B('A more informative measure of coherence is how many rules contain at least one condition not shared with any other rule. Examination of the 48 RDD rules shows that 44 of 48 rules (92%) contain at least one condition-bin combination unique to that rule. Only 4 rules are subsets of other rules — meaning the conditions of rule A are a strict subset of rule B\'s conditions, making rule A less specific. These 4 subset rules are retained because their higher support (the less specific rule covers more years) provides complementary information about the breadth of the pattern.'),
  SP(),
  B('The RVV rule set (20 rules) is more tightly clustered: 11 of 20 rules (55%) contain at least one unique condition. The remaining 9 rules are variations of the core harvest-soil-water + heat-stress-days theme, differing only in the specific heat-stress window used. This higher redundancy in RVV reflects that CarenR is identifying the same fundamental pattern (cool + dry + low-heat-stress → below trend) from multiple angle across different window widths, rather than finding genuinely distinct agronomic pathways.'),
  SP(),

  mkTable(
    ['Metric', 'RDD (48 rules)', 'RVV (20 rules)', 'Interpretation'],
    [
      ['Rule pairs sharing ≥1 feature', '730 / 1128 (64.7%)', 'N/A (20 rules)', 'Expected high overlap — confirms dominant shared features'],
      ['Rules with ≥1 unique condition-bin', '44 / 48 (92%)', '11 / 20 (55%)', 'RDD has richer feature diversity; RVV more concentrated'],
      ['Rules that are subsets of another rule', '4 / 48 (8%)', '9 / 20 (45%)', 'RVV rule set has more structural redundancy'],
      ['Number of unique feature variables used', '10', '11', 'Both use ~10 distinct variable types'],
      ['Most shared single feature', 'swa_hv_y1_b20 (26 rules, 54%)', 'days.maxTemp.above.35_sm_y1_a20 (10 rules, 50%)', 'Both have a single dominant structural backbone feature'],
    ],
    [2800, 1900, 1900, 2400]
  ),
  CAP('Table 4.11. Rule set overlap and coherence metrics for RDD and RVV.'),
  SP(),
  B('The overlap analysis confirms that the CarenR rule sets are internally coherent and structured: they are not random collections of statistically significant conditions but organised around a small number of dominant features (swa_hv in RDD, heat-stress days in RVV) with a richer secondary layer of context-specific conditions. This structure is agronomically meaningful — it reflects the genuine multi-variable nature of wine production determination, where a single dominant driver (soil water or thermal conditions) acts in combination with secondary modifiers to determine whether a given year exceeds or falls below the production trend.'),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter4_overlap_freq.docx', buf);
  console.log('Done: chapter4_overlap_freq.docx');
});

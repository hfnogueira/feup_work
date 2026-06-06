const {
  Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell,
  HeadingLevel, AlignmentType, BorderStyle, WidthType, ShadingType,
  PageNumber, Footer, PageBreak
} = require('docx');
const fs = require('fs');

const H1 = t => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 360, after: 180 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 28, bold: true })] });
const H2 = t => new Paragraph({ heading: HeadingLevel.HEADING_2, spacing: { before: 300, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, bold: true })] });
const H3 = t => new Paragraph({ heading: HeadingLevel.HEADING_3, spacing: { before: 240, after: 120 },
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24, italics: true })] });
const B = t => new Paragraph({ spacing: { before: 100, after: 100, line: 320 }, alignment: AlignmentType.JUSTIFIED,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 22 })] });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 60, after: 60 } });
const CAP = t => new Paragraph({ spacing: { before: 60, after: 160 }, alignment: AlignmentType.CENTER,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 20, italics: true })] });
const PB = () => new Paragraph({ children: [new PageBreak()] });

function mkRuleTable(headers, rows, colWidths) {
  const tw = colWidths.reduce((a,b)=>a+b,0);
  const br = { style: BorderStyle.SINGLE, size: 1, color: '999999' };
  const brs = { top: br, bottom: br, left: br, right: br };
  const cell = (text, w, bold=false, fill=null) => new TableCell({ borders: brs,
    width: { size: w, type: WidthType.DXA },
    shading: fill ? { fill, type: ShadingType.CLEAR } : undefined,
    margins: { top: 60, bottom: 60, left: 100, right: 100 },
    children: [new Paragraph({ children: [new TextRun({ text, font: 'Times New Roman', size: 18, bold })] })] });
  return new Table({ width: { size: tw, type: WidthType.DXA }, columnWidths: colWidths, rows: [
    new TableRow({ tableHeader: true, children: headers.map((h,i) => cell(h, colWidths[i], true, 'D9E2F3')) }),
    ...rows.map((r, ri) => new TableRow({ children: r.map((c,i) => cell(c, colWidths[i], false, ri%2===0 ? 'F7F7F7' : null)) }))
  ]});
}

// ── RDD CarenR rules (48 rules) — extracted from rules_interpreted_rdd.txt ──
const rddRules = [
  ['1','28.1% (~25yr)','0.0043','ABOVE','+121','Wet days post-Flowering [0–3.3d] AND Wet days post-Start-of-Maturity [0–3.5d]'],
  ['2','20.2% (~18yr)','0.0084','BELOW','−131','LAI at Flowering [0.74–0.88 m²/m²] AND Soil water post-Flowering [390–404 mm]'],
  ['3','25.8% (~23yr)','0.0089','ABOVE','+113','Wet days post-Flowering [0–3.3d] AND Wet days post-Maturity [0–3.5d] AND Heat days post-Flowering [0–0.6d]'],
  ['4','21.3% (~19yr)','0.0095','ABOVE','+192','Soil water pre-Harvest [74–101 mm] AND Wet days post-Harvest prev.yr [0–2.3d] AND Wet days post-Maturity [0–3.5d]'],
  ['5','24.7% (~22yr)','0.0110','ABOVE','+115','Wet days post-Flowering [0–3.3d] AND Heat days pre-Maturity [0–3.7d]'],
  ['6','23.6% (~21yr)','0.0120','ABOVE','+117','Wet days post-Flowering [0–3.3d] AND Heat days pre-Maturity [0–3.7d] AND Heat days post-Flowering [0–0.6d]'],
  ['7','21.3% (~19yr)','0.0130','ABOVE','+115','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Heat days pre-Maturity [0–3.7d]'],
  ['8','19.1% (~17yr)','0.0140','ABOVE','+128','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–152 mm] AND Wet days post-Maturity [0–3.5d]'],
  ['9','23.6% (~21yr)','0.0150','ABOVE','+116','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Heat days pre-Maturity [0–3.7d] AND Heat days post-Flowering [0–0.6d]'],
  ['10','20.2% (~18yr)','0.0160','ABOVE','+117','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–231 mm] AND Heat days pre-Maturity [0–3.7d] AND Heat days post-Flowering [0–0.6d]'],
  ['11','21.3% (~19yr)','0.0170','ABOVE','+113','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–152 mm] AND Heat days pre-Maturity [0–3.7d]'],
  ['12','28.1% (~25yr)','0.0180','ABOVE','+108','Wet days post-Flowering [0–3.3d] AND Wet days post-Maturity [0–3.5d] AND Soil water pre-Harvest [74–231 mm]'],
  ['13','19.1% (~17yr)','0.0200','ABOVE','+122','Soil water pre-Harvest [74–101 mm] AND Wet days post-Harvest prev.yr [0–2.3d] AND Wet days post-Maturity [0–3.5d] AND Heat days post-Flowering [0–0.6d]'],
  ['14','16.9% (~15yr)','0.0210','ABOVE','+143','Soil water pre-Harvest [74–101 mm] AND Wet days post-Harvest prev.yr [0–2.3d] AND Heat days pre-Maturity [0–3.7d]'],
  ['15','21.3% (~19yr)','0.0220','ABOVE','+115','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–152 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days post-Flowering [0–0.6d]'],
  ['16','18.0% (~16yr)','0.0240','ABOVE','+132','Soil water pre-Harvest [74–101 mm] AND Wet days post-Harvest prev.yr [0–2.3d] AND Heat days pre-Maturity [0–3.7d] AND Heat days post-Flowering [0–0.6d]'],
  ['17','22.5% (~20yr)','0.0250','ABOVE','+115','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days post-Flowering [0–0.6d]'],
  ['18','19.1% (~17yr)','0.0260','ABOVE','+119','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–152 mm] AND Wet days post-Maturity [0–3.5d] AND Cold days pre-Harvest [0–2.0d]'],
  ['19','22.5% (~20yr)','0.0270','ABOVE','+112','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Heat days pre-Maturity [0–3.7d] AND Cold days pre-Harvest [0–2.0d]'],
  ['20','20.2% (~18yr)','0.0290','ABOVE','+115','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Wet days post-Maturity [0–3.5d] AND Cold days pre-Harvest [0–2.0d]'],
  ['21','15.7% (~14yr)','0.0310','BELOW','−158','LAI at Flowering [0.74–0.88 m²/m²] AND Soil water post-Flowering [390–404 mm] AND Cold days pre-Harvest [0–2.0d]'],
  ['22','22.5% (~20yr)','0.0320','ABOVE','+107','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Cold days pre-Harvest [0–2.0d]'],
  ['23','19.1% (~17yr)','0.0340','ABOVE','+122','Soil water pre-Harvest [74–101 mm] AND Wet days post-Harvest prev.yr [0–2.3d]'],
  ['24','20.2% (~18yr)','0.0350','ABOVE','+119','Wet days post-Flowering [0–3.3d] AND Wet days post-Maturity [0–3.5d] AND Soil water pre-Harvest [74–231 mm] AND Cold days pre-Harvest [0–2.0d]'],
  ['25','21.3% (~19yr)','0.0370','ABOVE','+113','Wet days post-Flowering [0–3.3d] AND Cold days pre-Harvest [0–2.0d]'],
  ['26','18.0% (~16yr)','0.0380','ABOVE','+117','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–152 mm] AND Heat days pre-Maturity [0–3.7d] AND Cold days pre-Harvest [0–2.0d]'],
  ['27','19.1% (~17yr)','0.0400','ABOVE','+116','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–231 mm] AND Cold days pre-Harvest [0–2.0d]'],
  ['28','19.1% (~17yr)','0.0410','ABOVE','+121','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–152 mm] AND Cold days pre-Harvest [0–2.0d]'],
  ['29','16.9% (~15yr)','0.0430','ABOVE','+133','Wet days post-Flowering [0–3.3d] AND Wet days post-Maturity [0–3.5d] AND Soil water pre-Harvest [101–231 mm]'],
  ['30','19.1% (~17yr)','0.0440','ABOVE','+116','Wet days post-Flowering [0–3.3d] AND Wet days post-Maturity [0–3.5d] AND Cold days pre-Harvest [0–2.0d]'],
  ['31','19.1% (~17yr)','0.0450','ABOVE','+122','Soil water pre-Harvest [74–101 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days pre-Maturity [0–3.7d]'],
  ['32','16.9% (~15yr)','0.0470','ABOVE','+130','Soil water pre-Harvest [74–101 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days pre-Maturity [0–3.7d] AND Heat days post-Flowering [0–0.6d]'],
  ['33','18.0% (~16yr)','0.0480','ABOVE','+121','Soil water pre-Harvest [74–101 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days post-Flowering [0–0.6d]'],
  ['34','15.7% (~14yr)','0.0500','ABOVE','+141','Soil water pre-Harvest [74–101 mm] AND Wet days post-Maturity [0–3.5d]'],
  ['35','20.2% (~18yr)','0.0510','ABOVE','+111','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days pre-Maturity [0–3.7d] AND Heat days post-Flowering [0–0.6d]'],
  ['36','20.2% (~18yr)','0.0520','ABOVE','+110','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Heat days pre-Maturity [0–3.7d] AND Cold days pre-Harvest [0–2.0d] AND Heat days post-Flowering [0–0.6d]'],
  ['37','15.7% (~14yr)','0.0540','ABOVE','+140','Wet days post-Flowering [0–3.3d] AND Wet days post-Maturity [0–3.5d] AND Soil water pre-Harvest [101–152 mm] AND Cold days pre-Harvest [0–2.0d]'],
  ['38','16.9% (~15yr)','0.0560','ABOVE','+127','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–231 mm] AND Wet days post-Maturity [0–3.5d] AND Cold days pre-Harvest [0–2.0d]'],
  ['39','19.1% (~17yr)','0.0570','ABOVE','+113','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days pre-Maturity [0–3.7d]'],
  ['40','16.9% (~15yr)','0.0580','ABOVE','+127','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–152 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days pre-Maturity [0–3.7d] AND Cold days pre-Harvest [0–2.0d]'],
  ['41','16.9% (~15yr)','0.0600','ABOVE','+126','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Cold days pre-Harvest [0–2.0d] AND Heat days post-Flowering [0–0.6d]'],
  ['42','16.9% (~15yr)','0.0620','ABOVE','+126','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–231 mm] AND Wet days post-Maturity [0–3.5d] AND Cold days pre-Harvest [0–2.0d] AND Heat days post-Flowering [0–0.6d]'],
  ['43','16.9% (~15yr)','0.0650','ABOVE','+119','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–231 mm] AND Heat days pre-Maturity [0–3.7d] AND Cold days pre-Harvest [0–2.0d]'],
  ['44','15.7% (~14yr)','0.0680','ABOVE','+136','Wet days post-Flowering [0–3.3d] AND Wet days post-Maturity [0–3.5d] AND Soil water pre-Harvest [74–231 mm] AND Heat days pre-Maturity [0–3.7d] AND Cold days pre-Harvest [0–2.0d]'],
  ['45','15.7% (~14yr)','0.0700','ABOVE','+132','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [101–152 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days pre-Maturity [0–3.7d] AND Heat days post-Flowering [0–0.6d]'],
  ['46','15.7% (~14yr)','0.0720','ABOVE','+130','Wet days post-Flowering [0–3.3d] AND Soil water pre-Harvest [74–101 mm] AND Wet days post-Maturity [0–3.5d] AND Heat days pre-Maturity [0–3.7d]'],
  ['47','15.7% (~14yr)','0.0750','ABOVE','+132','Soil water pre-Harvest [74–101 mm] AND Wet days post-Harvest prev.yr [0–2.3d] AND Cold days pre-Harvest [0–2.0d]'],
  ['48','16.9% (~15yr)','0.0900','BELOW','−118','LAI at Harvest [0.58–0.77 m²/m²] AND Soil water post-Flowering [390–404 mm]'],
];

// ── RVV CarenR rules (20 rules) ──
const rvvRules = [
  ['1','21.2% (~17yr)','0.0198','BELOW','−338','Soil water pre-Harvest [61–97 mm] AND Heat days pre-Maturity [0–1.9d] AND Heat days post-Maturity [0–2.4d] AND Heat days post-Flowering [0–1.1d]'],
  ['2','21.2% (~17yr)','0.0216','BELOW','−268','Temp centred on Flowering [13.8–17.1°C] AND Temp post-Flowering [12.6–17.5°C] AND Wet-soil days post-Flowering prev.yr [17.5–20d]'],
  ['3','27.5% (~22yr)','0.0294','BELOW','−365','Soil water post-Flowering [143–234 mm] AND Heat days post-Maturity [0–2.4d]'],
  ['4','25.0% (~20yr)','0.0298','ABOVE','+311','Temp post-Budburst prev.yr [11.2–13.5°C] AND Cold days centred Flowering [0–3.4d] AND Heat days post-Flowering [0–1.1d]'],
  ['5','23.8% (~19yr)','0.0299','BELOW','−292','Temp post-Flowering [12.6–17.5°C] AND Heat days post-Maturity [0–2.4d] AND Wet-soil days post-Flowering prev.yr [17.5–20d]'],
  ['6','26.2% (~21yr)','0.0356','BELOW','−387','Rainfall post-Flowering [20.7–121 mm] AND LAI at Harvest [1.29–2.52 m²/m²] AND Heat days post-Flowering [0–1.3d]'],
  ['7','26.2% (~21yr)','0.0356','BELOW','−387','Rainfall post-Flowering [20.7–121 mm] AND LAI at Harvest [1.29–2.52 m²/m²] AND Heat days post-Flowering (20d window) [0–1.1d]'],
  ['8','22.5% (~18yr)','0.0357','BELOW','−332','LAI pre-Flowering prev.yr [2.16–2.21 m²/m²] AND Heat days pre-Maturity [0–1.9d] AND Heat days post-Maturity [0–2.4d]'],
  ['9','20.0% (~16yr)','0.0370','BELOW','−387','Rainfall post-Flowering [20.7–121 mm] AND LAI at Harvest [1.29–2.52 m²/m²] AND Heat days post-Maturity [0–2.4d] AND Heat days post-Flowering [0–1.1d]'],
  ['10','21.2% (~17yr)','0.0401','BELOW','−338','Soil water pre-Harvest [61–97 mm] AND Heat days post-Maturity [0–2.4d] AND Heat days post-Flowering [0–1.1d]'],
  ['11','18.8% (~15yr)','0.0418','BELOW','−330','LAI pre-Flowering prev.yr [2.16–2.21 m²/m²] AND Heat days post-Maturity [0–2.4d]'],
  ['12','20.0% (~16yr)','0.0430','BELOW','−392','Rainfall post-Flowering [20.7–121 mm] AND LAI at Harvest [1.29–2.52 m²/m²]'],
  ['13','21.2% (~17yr)','0.0433','BELOW','−320','Soil water pre-Harvest [61–97 mm] AND Heat days pre-Maturity [0–1.9d] AND Heat days post-Flowering [0–1.1d]'],
  ['14','21.2% (~17yr)','0.0450','BELOW','−338','Soil water pre-Harvest [61–97 mm] AND Heat days pre-Maturity [0–1.9d] AND Heat days post-Maturity [0–2.4d]'],
  ['15','18.8% (~15yr)','0.0500','BELOW','−382','Rainfall post-Flowering [20.7–121 mm] AND LAI at Harvest [1.29–2.52 m²/m²] AND Heat days post-Maturity [0–2.4d]'],
  ['16','22.5% (~18yr)','0.0514','BELOW','−289','Soil water post-Flowering [143–234 mm] AND Heat days post-Maturity [0–2.4d] AND Heat days post-Flowering [0–1.1d]'],
  ['17','21.2% (~17yr)','0.0520','BELOW','−338','Soil water pre-Harvest [61–97 mm] AND Heat days post-Maturity [0–2.4d]'],
  ['18','21.2% (~17yr)','0.0550','BELOW','−338','Soil water pre-Harvest [61–97 mm] AND Heat days post-Flowering [0–1.1d]'],
  ['19','18.8% (~15yr)','0.0600','BELOW','−289','Soil water post-Flowering [143–234 mm] AND Heat days post-Maturity [0–2.4d] AND Wet-soil days post-Flowering prev.yr [17.5–20d]'],
  ['20','23.8% (~19yr)','0.0620','BELOW','−246','Temp post-Flowering [12.6–17.5°C] AND Heat days post-Maturity [0–2.4d]'],
];

const children = [

  H1('Appendix A — Complete Rule Listings'),
  SP(),
  B('This appendix presents the complete set of rules discovered by CarenR, RIPPER, and M5Rules on the full historical datasets for both regions. Rules are sorted by KS p-value (CarenR) or rule number (RIPPER, M5Rules). All CarenR rules were discovered with filters: support ≥ 15% and KS p-value ≤ 0.10. Detrending: linear (residuals). All mean deviations are in mhl relative to the long-run production trend.'),
  SP(),

  // ── A.1 CarenR RDD ────────────────────────────────────────────────────────
  H2('A.1 CarenR — Douro Region (RDD): 48 Rules'),
  SP(),
  B('All 48 rules are sorted by KS p-value (most significant first). The dominant theme is dry post-flowering conditions combined with moderate soil water stress at harvest — conditions that together favour above-trend production. Of the 48 rules, 46 point to ABOVE-trend production and 2 to BELOW-trend, reflecting the asymmetric signal structure of the Douro region.'),
  SP(),

  mkRuleTable(
    ['#', 'Support', 'p-value', 'Direction', 'Mean Δ (mhl)', 'Conditions (abbreviated)'],
    rddRules,
    [400, 1000, 800, 900, 900, 5000]
  ),
  CAP('Table A.1. All 48 CarenR rules for the Douro region (RDD). Conditions abbreviated; full variable names use the notation defined in Section 3.2.'),
  SP(),

  // ── A.2 CarenR RVV ────────────────────────────────────────────────────────
  H2('A.2 CarenR — Vinho Verde Region (RVV): 20 Rules'),
  SP(),
  B('All 20 RVV rules are presented below. Of the 20 rules, 19 point to BELOW-trend production and 1 to ABOVE-trend, reflecting the strong negative structural drift of the Vinho Verde series. The dominant features are soil water at harvest, flowering rainfall and LAI, heat-stress days around maturity, and wet-soil conditions from the previous year. Rule 4 is the sole above-trend rule, linking warm previous-year budburst temperatures and few cold days at flowering to above-trend production.'),
  SP(),

  mkRuleTable(
    ['#', 'Support', 'p-value', 'Direction', 'Mean Δ (mhl)', 'Conditions (abbreviated)'],
    rvvRules,
    [400, 1000, 800, 900, 900, 5000]
  ),
  CAP('Table A.2. All 20 CarenR rules for the Vinho Verde region (RVV).'),
  SP(),

  // ── A.3 RIPPER ────────────────────────────────────────────────────────────
  H2('A.3 RIPPER Classification Rules — Both Regions'),
  SP(),
  B('RIPPER discovers an ordered rule list for tertile-discretised production classes (Low / Medium / High). Class boundaries: RDD — Low < 975 mhl, Medium 975–1,239 mhl, High > 1,239 mhl; RVV — Low < 1,090 mhl, Medium 1,090–1,900 mhl, High > 1,900 mhl. Rules are evaluated in order; the first matching rule applies. The default rule covers all years not matched by any explicit rule.'),
  SP(),

  mkRuleTable(
    ['#', 'Region', 'Conditions', 'Prediction', 'Support', 'Precision (train)', 'CV Accuracy'],
    [
      ['1','RDD','Mean temp at Harvest (10d before) in [21.2–22.8 °C]','MEDIUM','21.3% (19yr)','73.7%','—'],
      ['2','RDD','LAI at Budburst (10d before) ≥ 0.0016\nAND Mean temp at Harvest (20d before) ≥ 23.1 °C','HIGH','19.1% (17yr)','76.5%','—'],
      ['3','RDD','Cold days at Flowering (15d before) ≤ 3\nAND Mean temp at Harvest (20d before) ≤ 21.3 °C','HIGH','6.7% (6yr)','100%','—'],
      ['4','RDD','(default — all other years)','LOW','—','—','42% overall (κ=0.06)'],
      ['1','RVV','LAI at Flowering prev.yr ≥ 2.4\nAND Budburst temp prev.yr ≥ 13.3°C\nAND Rainfall at Flowering ≤ 15.9 mm','MEDIUM','16.2% (13yr)','92.3%','—'],
      ['2','RVV','Mean temp at Flowering (10d before) ≥ 19.3°C\nAND LAI at Flowering (10d before) ≥ 2.2','MEDIUM','11.2% (9yr)','88.9%','—'],
      ['3','RVV','Soil water at Flowering (30d after) ≤ 123.6 mm\nAND Mean temp at Flowering (15d after) ≥ 16.6°C','HIGH','26.2% (21yr)','85.7%','—'],
      ['4','RVV','(default — all other years)','LOW','—','—','35% overall (κ=0.19)'],
    ],
    [400, 700, 2800, 1000, 900, 1200, 1000]
  ),
  CAP('Table A.3. RIPPER classification rules for both regions. Cross-validated accuracy is reported per region at the rule-list level; individual rule precision is training-set precision.'),
  SP(),

  // ── A.4 M5Rules ───────────────────────────────────────────────────────────
  H2('A.4 M5Rules Piecewise Linear Rules — Both Regions'),
  SP(),
  B('M5Rules collapsed to global linear models in both regions (no beneficial split found). The rules below present the full linear model for each region. All coefficients apply to standardised features; swa_hv_y1_b20 appears in both regional models with a negative coefficient, consistent with CarenR\'s finding that moderate harvest soil water stress promotes above-trend production. Cross-validated R²: RDD = 0.004, RVV = −0.07.'),
  SP(),

  H3('A.4.1 RDD — 2 Rules'),
  SP(),
  B('Rule 1 (conditional): IF Mean temp pre-Flowering prev.yr > 16.03°C AND Wet days post-Flowering ≤ 3.5d'),
  B('THEN production residual = 45.23 × tm_fl_y1_b10 + 23.43 × tm_fl_y0_b10 − 52.17 × tm_bb_y0_a30 − 9.18 × tm_sm_y0_a20 + 8.80 × tm_hv_y1_c10 + 7.31 × tn_hv_y0_a30 − 3.45 × cold_days_fl + 675.75 × iaf_fl_y1_b10 + 285.85 × iaf_fl_y1_b30 + 2319.99 × iaf_fl_y0_b20 − 2474.55 × iaf_fl_y0_b30 − 0.44 × swa_hv_y1_b20 − 1023.61'),
  SP(),
  B('Rule 2 (default): all other years'),
  B('THEN production residual = 66.45 × tm_fl_y1_b10 − 97.63 × tm_bb_y0_a30 − 38.54 × tm_sm_y0_a20 + 39.62 × tm_hv_y0_a30 + 1394.73 × iaf_fl_y1_b20 + 4138.60 × iaf_fl_y0_b10 − 4199.43 × iaf_fl_y0_b20 − 2.30 × swa_hv_y1_b20 − 408.09'),
  SP(),

  H3('A.4.2 RVV — 3 Rules'),
  SP(),
  B('Rule 1 (conditional): IF Soil water post-Flowering > 131.4 mm'),
  B('THEN production residual = −13.26 × tm_fl_y0_b10 − 40.72 × tm_fl_y0_a10 + 17.16 × tm_sm_y0_a20 − 24.87 × tm_hv_y1_b20 − 2063.79 × iaf_bb_y1_c10 − 8.12 × swa_fl_y1_a30 + 2419.77'),
  SP(),
  B('Rule 2 (conditional): IF Mean temp pre-Harvest > 16.76°C'),
  B('THEN production residual = 24.11 × tm_fl_y1_c15 − 24.09 × tm_fl_y0_b10 + 46.70 × tm_sm_y0_a20 − 49.55 × tm_hv_y1_b20 + 5.10 × rf_hv_y0_a10 − 1.74 × swa_hv_y1_b20 + 227.87'),
  SP(),
  B('Rule 3 (default): all other years'),
  B('THEN production residual = −151.60 × tm_fl_y0_b10 + 3455.29'),
  SP(),
  CAP('Table A.4. M5Rules piecewise linear models for both regions. Features use the notation of Section 3.2. Coefficients are unstandardised; intercept is the constant term.'),
  SP(),

];

const doc = new Document({
  styles: { default: { document: { run: { font: 'Times New Roman', size: 22 } } },
    paragraphStyles: [
      { id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 28, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 360, after: 180 }, outlineLevel: 0 } },
      { id: 'Heading2', name: 'Heading 2', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 24, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 300, after: 120 }, outlineLevel: 1 } },
      { id: 'Heading3', name: 'Heading 3', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 24, italics: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 240, after: 120 }, outlineLevel: 2 } },
    ] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1440, right: 1200, bottom: 1440, left: 1440 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ children: [PageNumber.CURRENT], font: 'Times New Roman', size: 20 })] })] }) },
    children }]
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/appendix_a_rules.docx', buf);
  console.log('Done: appendix_a_rules.docx');
});

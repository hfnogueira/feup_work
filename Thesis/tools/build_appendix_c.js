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
const B = t => new Paragraph({ spacing: { before: 120, after: 120, line: 340 }, alignment: AlignmentType.JUSTIFIED,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 24 })] });
const SP = () => new Paragraph({ children: [new TextRun('')], spacing: { before: 60, after: 60 } });
const CAP = t => new Paragraph({ spacing: { before: 80, after: 200 }, alignment: AlignmentType.CENTER,
  children: [new TextRun({ text: t, font: 'Times New Roman', size: 20, italics: true })] });

function mkTable(headers, rows, colWidths) {
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
    ...rows.map((r, ri) => new TableRow({
      children: r.map((c,i) => cell(c, colWidths[i], false, ri%2===0 ? 'F7F7F7' : null))
    }))
  ]});
}

// All 58 features with full descriptions
const features = [
  // ── Temperature — current year ──
  ['tm_fl_y1_c15',  'Mean temperature', 'Flowering', 'y1', 'centred ±15d', '°C', 'Average', 'Thermal conditions during the critical flowering window — governs pollen viability, fruit set, and inflorescence development'],
  ['tm_fl_y1_b10',  'Mean temperature', 'Flowering', 'y1', '10d before', '°C', 'Average', 'Pre-flowering thermal conditions — vine heat accumulation entering the flowering phase'],
  ['tm_fl_y1_a10',  'Mean temperature', 'Flowering', 'y1', '10d after', '°C', 'Average', 'Post-flowering temperature — governs initial berry cell division and early fruit development'],
  ['tm_bb_y0_a15',  'Mean temperature', 'Bud Break', 'y0', '15d after prev. yr', '°C', 'Average', 'Previous-year early spring thermal conditions — influences inflorescence primordia initiation for current year'],
  ['tm_bb_y0_a30',  'Mean temperature', 'Bud Break', 'y0', '30d after prev. yr', '°C', 'Average', 'Wider post-budburst window in previous year — vine carbohydrate allocation and early season vigour'],
  ['tm_sm_y1_c20',  'Mean temperature', 'Start Maturity', 'y1', 'centred ±20d', '°C', 'Average', 'Temperature at veraison onset — governs timing of berry colour change and ripening acceleration'],
  ['tm_sm_y0_a20',  'Mean temperature', 'Start Maturity', 'y0', '20d after prev. yr', '°C', 'Average', 'Previous-year post-veraison heat — vine thermal stress at end of prior season; affects reserve accumulation'],
  ['tm_sm_y0_b20',  'Mean temperature', 'Start Maturity', 'y0', '20d before prev. yr', '°C', 'Average', 'Previous-year pre-veraison temperature — mid-ripening thermal conditions in prior season'],
  ['tm_hv_y1_c10',  'Mean temperature', 'Harvest', 'y1', 'centred ±10d', '°C', 'Average', 'Temperature at harvest — final ripening conditions; warmer harvest accelerates sugar accumulation'],
  ['tm_hv_y1_b20',  'Mean temperature', 'Harvest', 'y1', '20d before', '°C', 'Average', 'Pre-harvest temperature — late-season thermal driver of final berry maturation'],
  ['tm_fl_y0_b10',  'Mean temperature', 'Flowering', 'y0', '10d before prev. yr', '°C', 'Average', 'Previous-year pre-flowering temperature — key driver of inflorescence primordia differentiation'],
  ['tm_fl_y0_a10',  'Mean temperature', 'Flowering', 'y0', '10d after prev. yr', '°C', 'Average', 'Previous-year post-flowering temperature — affects fruit set and bunch architecture for current year'],
  // ── Min temperature ──
  ['tn_hv_y0_a30',  'Min temperature', 'Harvest', 'y0', '30d after prev. yr', '°C', 'Average', 'Previous-year post-harvest night temperatures — warm nights maintain vine metabolic activity; affect carbohydrate reserve building'],
  // ── Cold days ──
  ['tm.days.less.15_fl_y1_c15', 'Cold days (Tmed<15°C)', 'Flowering', 'y1', 'centred ±15d', 'days', 'Count', 'Number of cold days during flowering — impairs pollen viability and fertilisation; directly reduces berry set'],
  ['tm.days.less.15_fl_y1_b15', 'Cold days (Tmed<15°C)', 'Flowering', 'y1', '15d before', 'days', 'Count', 'Pre-flowering cold days — cool conditions entering the flowering phase'],
  // ── Frost days ──
  ['days_minTemp.less.0_bb_y1_c20', 'Frost days (Tmin<0°C)', 'Bud Break', 'y1', 'centred ±20d', 'days', 'Count', 'Frost days at budburst — potential spring frost damage to emerging shoots; severe frosts can destroy entire crop'],
  ['days.minTemp.less.0_bb_y1_a30', 'Frost days (Tmin<0°C)', 'Bud Break', 'y1', '30d after', 'days', 'Count', 'Post-budburst frost risk window — young shoots most vulnerable'],
  ['days.minTemp.less.0_fl_y1_a10', 'Frost days (Tmin<0°C)', 'Flowering', 'y1', '10d after', 'days', 'Count', 'Late frost at flowering — unusual but damaging frost events during fruit set'],
  // ── Heat stress days ──
  ['days.maxTemp.above.35_fl_y1_a10', 'Heat days (Tmax>35°C)', 'Flowering', 'y1', '10d after', 'days', 'Count', 'Extreme heat at flowering — causes flower abortion, pollen tube failure, and reduces berry number'],
  ['days.maxTemp.above.35_fl_y1_a20', 'Heat days (Tmax>35°C)', 'Flowering', 'y1', '20d after', 'days', 'Count', 'Extended post-flowering heat exposure — sustained heat impact on early berry development'],
  ['days.tempMax.above.35_sm_y1_b20', 'Heat days (Tmax>35°C)', 'Start Maturity', 'y1', '20d before', 'days', 'Count', 'Heat stress approaching veraison — accelerates sugar rise; can impair acid retention and berry integrity'],
  ['days.maxTemp.above.35_sm_y1_a20', 'Heat days (Tmax>35°C)', 'Start Maturity', 'y1', '20d after', 'days', 'Count', 'Post-veraison heat stress — berry thermal stress during the key ripening phase'],
  // ── Rainfall ──
  ['rf_fl_y1_a10',  'Rainfall total', 'Flowering', 'y1', '10d after', 'mm', 'Sum', 'Rainfall immediately post-flowering — disease risk (Botrytis, downy mildew); excess rain dilutes berry development'],
  ['rf_hv_y1_b10',  'Rainfall total', 'Harvest', 'y1', '10d before', 'mm', 'Sum', 'Pre-harvest rainfall — late rain can cause berry splitting, dilution of sugars, and Botrytis infection'],
  ['rf_hv_y0_a10',  'Rainfall total', 'Harvest', 'y0', '10d after prev. yr', 'mm', 'Sum', 'Post-harvest rainfall in previous year — begins recharging soil water reserves for the following season'],
  // ── Wet days ──
  ['days.rf.above.1mm_fl_y1_c15', 'Wet days (Rain>1mm)', 'Flowering', 'y1', 'centred ±15d', 'days', 'Count', 'Frequency of rain days at flowering — high frequency promotes disease and physically interferes with pollination'],
  ['days.rf.above.1mm_fl_y1_a20', 'Wet days (Rain>1mm)', 'Flowering', 'y1', '20d after', 'days', 'Count', 'Post-flowering rain event count — key disease pressure indicator in the Douro; DRY = above trend'],
  ['days.rf.above.1mm_sm_y1_b20', 'Wet days (Rain>1mm)', 'Start Maturity', 'y1', '20d before', 'days', 'Count', 'Rain days approaching veraison — moisture during mid-season ripening phase'],
  ['days.rf.above.1mm_sm_y1_a20', 'Wet days (Rain>1mm)', 'Start Maturity', 'y1', '20d after', 'days', 'Count', 'Post-veraison rain days — disease and dilution risk during the ripening phase; 2nd most frequent RDD rule feature'],
  ['days.rf.above.1mm_hv_y1_c10', 'Wet days (Rain>1mm)', 'Harvest', 'y1', 'centred ±10d', 'days', 'Count', 'Rain days at harvest — wet harvest conditions cause berry splitting, Botrytis; reduce quality and quantity'],
  ['days.rf.above.1mm_hv_y0_a10', 'Wet days (Rain>1mm)', 'Harvest', 'y0', '10d after prev. yr', 'days', 'Count', 'Post-harvest rain events in previous year — soil recharge initiation; carry-over for next season'],
  // ── LAI (current year) ──
  ['iaf_bb_y1_c10', 'Leaf Area Index', 'Bud Break', 'y1', 'centred ±10d', 'm²/m²', 'Average', 'LAI at budburst — canopy size at growth initiation; proxy for vine vigour entering the season'],
  ['iaf_fl_y1_b10', 'Leaf Area Index', 'Flowering', 'y1', '10d before', 'm²/m²', 'Average', 'Pre-flowering canopy development — vegetative vigour entering the critical fruit-set window'],
  ['iaf_fl_y1_b20', 'Leaf Area Index', 'Flowering', 'y1', '20d before', 'm²/m²', 'Average', 'Broader pre-flowering LAI window — canopy development trajectory before flowering'],
  ['iaf_fl_y1_b30', 'Leaf Area Index', 'Flowering', 'y1', '30d before', 'm²/m²', 'Average', 'Early pre-flowering canopy — wider window capturing canopy build-up phase'],
  ['iaf_fl_y1_a10', 'Leaf Area Index', 'Flowering', 'y1', '10d after', 'm²/m²', 'Average', 'Post-flowering canopy — rapid leaf area expansion immediately after flowering; vigour proxy'],
  ['iaf_hv_y1_c10', 'Leaf Area Index', 'Harvest', 'y1', 'centred ±10d', 'm²/m²', 'Average', 'LAI at harvest — canopy size at the time of picking; DOMINANT RVV feature (20/20 rules); high LAI → below trend'],
  // ── LAI (previous year) ──
  ['iaf_fl_y0_b10', 'Leaf Area Index', 'Flowering', 'y0', '10d before prev. yr', 'm²/m²', 'Average', 'Previous-year pre-flowering LAI — vine canopy state before prior-year fruit set; carry-over vigour signal'],
  ['iaf_fl_y0_b20', 'Leaf Area Index', 'Flowering', 'y0', '20d before prev. yr', 'm²/m²', 'Average', 'Wider previous-year pre-flowering LAI window'],
  ['iaf_fl_y0_b30', 'Leaf Area Index', 'Flowering', 'y0', '30d before prev. yr', 'm²/m²', 'Average', 'Widest previous-year pre-flowering LAI window — M5Rules LAI differential uses b10/b20/b30 together'],
  ['iaf_fl_y0_a10', 'Leaf Area Index', 'Flowering', 'y0', '10d after prev. yr', 'm²/m²', 'Average', 'Post-flowering LAI in previous year — canopy expansion after prior-year fruit set'],
  // ── Soil water availability ──
  ['swa_fl_y1_a15', 'Soil water availability', 'Flowering', 'y1', '15d after', 'mm', 'Average', 'Soil water balance post-flowering — moisture availability during early berry development; excess → vigour/disease in RVV'],
  ['swa_fl_y1_a30', 'Soil water availability', 'Flowering', 'y1', '30d after', 'mm', 'Average', 'Wider post-flowering soil water window — cumulative moisture through mid-season; appears in RVV rules 3, 16, 19'],
  ['swa_hv_y1_b20', 'Soil water availability', 'Harvest', 'y1', '20d before', 'mm', 'Average', 'Pre-harvest soil water — DOMINANT feature in both regions; RDD: moderate deficit → above trend; RVV: deficit → below trend'],
  ['swa_hv_y0_a15', 'Soil water availability', 'Harvest', 'y0', '15d after prev. yr', 'mm', 'Average', 'Post-harvest soil water in previous year — soil recharge initiation; carry-over moisture for next season'],
  ['swa_hv_y0_a20', 'Soil water availability', 'Harvest', 'y0', '20d after prev. yr', 'mm', 'Average', 'Wider post-harvest soil water in previous year — confirms carry-over recharge pattern'],
  // ── Soil water stress days ──
  ['sw.less.15wp_fl_y1_a10', 'Stress days (Sm<1.5×Wp)', 'Flowering', 'y1', '10d after', 'days', 'Count', 'Severe stress days post-flowering — days when soil water falls below vine wilting threshold'],
  ['sw.less.15wp_fl_y1_a20', 'Stress days (Sm<1.5×Wp)', 'Flowering', 'y1', '20d after', 'days', 'Count', 'Extended post-flowering stress exposure'],
  ['sw.less.15wp_sm_y1_a10', 'Stress days (Sm<1.5×Wp)', 'Start Maturity', 'y1', '10d after', 'days', 'Count', 'Severe stress days at veraison onset — critical window for sugar–acid balance'],
  ['sw.less.15wp_sm_y1_a20', 'Stress days (Sm<1.5×Wp)', 'Start Maturity', 'y1', '20d after', 'days', 'Count', 'Extended stress at maturity — prolonged wilting-level stress during ripening'],
  ['sw.less.15wp_hv_y1_b20', 'Stress days (Sm<1.5×Wp)', 'Harvest', 'y1', '20d before', 'days', 'Count', 'Pre-harvest severe stress — intense berry concentration but risk of vine damage if prolonged'],
  ['sw.less.15wp_hv_y1_a20', 'Stress days (Sm<1.5×Wp)', 'Harvest', 'y1', '20d after', 'days', 'Count', 'Post-harvest severe stress — vine recovery period after harvest'],
  ['sw.less.15wp_fl_y0_a10', 'Stress days (Sm<1.5×Wp)', 'Flowering', 'y0', '10d after prev. yr', 'days', 'Count', 'Previous-year post-flowering stress — carry-over water deficit from prior season'],
  ['sw.less.15wp_fl_y0_a20', 'Stress days (Sm<1.5×Wp)', 'Flowering', 'y0', '20d after prev. yr', 'days', 'Count', 'Extended previous-year post-flowering stress'],
  // ── Soil saturation days ──
  ['sw.above.09fc_fl_y1_a10', 'Saturation days (Sm>0.9×Fc)', 'Flowering', 'y1', '10d after', 'days', 'Count', 'Near-saturation days post-flowering — promote disease and vegetative vigour; relevant to RVV below-trend rules'],
  ['sw.above.09fc_fl_y1_a20', 'Saturation days (Sm>0.9×Fc)', 'Flowering', 'y1', '20d after', 'days', 'Count', 'Extended near-saturation post-flowering'],
  ['sw.above.09fc_fl_y0_a10', 'Saturation days (Sm>0.9×Fc)', 'Flowering', 'y0', '10d after prev. yr', 'days', 'Count', 'Previous-year post-flowering soil saturation — carry-over vigour effect identified in RVV Rule 2'],
  ['sw.above.09fc_fl_y0_a20', 'Saturation days (Sm>0.9×Fc)', 'Flowering', 'y0', '20d after prev. yr', 'days', 'Count', 'Wider window for previous-year post-flowering soil saturation — strongest RVV carry-over signal'],
];

const children = [
  H1('Appendix C — Complete Feature Reference'),
  SP(),
  B('This appendix provides a complete reference for all 58 agroclimatic features used in the modelling datasets (dataPrep_cont_dataset_rdd.csv and dataPrep_cont_dataset_rvv.csv). Features are grouped by variable family. The naming convention is described in Section 3.2.1. For each feature, the full name, phenological anchor, year offset, window specification, unit, calculation type, and agronomic interpretation are provided.'),
  SP(),
  B('Key notation: y1 = current harvest year; y0 = previous growing season. Positions: c = centred (±n/2 days around stage DOY), b = n days before stage DOY, a = n days after stage DOY. All aggregations are computed over the specified window relative to the phenological DOY for each individual year, using the daily meteorological records from dataPrep_meteo_*.csv.'),
  SP(),

  H2('C.1 Temperature Features (13 features)'),
  SP(),
  mkTable(
    ['Feature name', 'Variable', 'Stage', 'Year', 'Window', 'Unit', 'Calc', 'Agronomic interpretation'],
    features.slice(0, 13),
    [1600, 1200, 900, 500, 1000, 500, 600, 3700]
  ),
  CAP('Table C.1. Mean and minimum temperature features (13 total).'),
  SP(),

  H2('C.2 Cold and Frost Day Features (5 features)'),
  SP(),
  mkTable(
    ['Feature name', 'Variable', 'Stage', 'Year', 'Window', 'Unit', 'Calc', 'Agronomic interpretation'],
    features.slice(13, 18),
    [1600, 1400, 900, 500, 1000, 500, 600, 3500]
  ),
  CAP('Table C.2. Cold day (Tmed < 15°C) and frost day (Tmin < 0°C) features.'),
  SP(),

  H2('C.3 Heat Stress Day Features (4 features)'),
  SP(),
  mkTable(
    ['Feature name', 'Variable', 'Stage', 'Year', 'Window', 'Unit', 'Calc', 'Agronomic interpretation'],
    features.slice(18, 22),
    [1800, 1400, 900, 500, 1000, 500, 600, 3300]
  ),
  CAP('Table C.3. Heat stress day features (Tmax > 35°C).'),
  SP(),

  H2('C.4 Rainfall Features (8 features)'),
  SP(),
  mkTable(
    ['Feature name', 'Variable', 'Stage', 'Year', 'Window', 'Unit', 'Calc', 'Agronomic interpretation'],
    features.slice(22, 30),
    [1800, 1200, 900, 500, 1000, 500, 600, 3500]
  ),
  CAP('Table C.4. Rainfall total and wet day count features.'),
  SP(),

  H2('C.5 Leaf Area Index (LAI) Features (10 features)'),
  SP(),
  mkTable(
    ['Feature name', 'Variable', 'Stage', 'Year', 'Window', 'Unit', 'Calc', 'Agronomic interpretation'],
    features.slice(30, 40),
    [1600, 1200, 900, 500, 1000, 500, 600, 3700]
  ),
  CAP('Table C.5. Leaf Area Index features. iaf_hv_y1_c10 is the dominant RVV feature (20/20 rules).'),
  SP(),

  H2('C.6 Soil Water Features (18 features)'),
  SP(),
  mkTable(
    ['Feature name', 'Variable', 'Stage', 'Year', 'Window', 'Unit', 'Calc', 'Agronomic interpretation'],
    features.slice(40),
    [1600, 1400, 900, 500, 1000, 500, 600, 3500]
  ),
  CAP('Table C.6. Soil water availability, stress day (Sm < 1.5×Wp), and saturation day (Sm > 0.9×Fc) features. swa_hv_y1_b20 is the dominant feature in both regions.'),
  SP(),
];

const doc = new Document({
  styles: { default: { document: { run: { font: 'Times New Roman', size: 22 } } },
    paragraphStyles: [
      { id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 28, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 360, after: 180 }, outlineLevel: 0 } },
      { id: 'Heading2', name: 'Heading 2', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 24, bold: true, font: 'Times New Roman' }, paragraph: { spacing: { before: 300, after: 120 }, outlineLevel: 1 } },
    ] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1440, right: 1200, bottom: 1440, left: 1440 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ children: [PageNumber.CURRENT], font: 'Times New Roman', size: 20 })] })] }) },
    children }]
});
Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/appendix_c_features.docx', buf);
  console.log('Done: appendix_c_features.docx');
});

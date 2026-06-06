// Standalone "Data Pipeline" chapter section — to be merged into Ch3 or used as Section 3.1 expansion
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
const MONO = t => new Paragraph({ spacing: { before: 60, after: 60 },
  indent: { left: 720 },
  children: [new TextRun({ text: t, font: 'Courier New', size: 20 })] });

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

  H1('3. Data and Methodology'),
  SP(),
  B('This chapter describes the full data pipeline — from raw meteorological archives to the curated annual feature datasets used for rule discovery and walk-forward prediction — together with the detrending methodology, algorithm descriptions, and evaluation protocol. Section 3.1 tells the complete data story: the sources, transformations, and decisions that shaped the final datasets. This pipeline was developed over an extended period of exploratory data analysis and represents a significant methodological contribution in its own right, as no equivalent phenology-anchored climate feature dataset existed for these two regions across the full 80–90 year time horizons.'),
  SP(),

  H2('3.1 Data Sources'),
  SP(),
  B('The datasets for both regions originate from two independent source streams that were prepared, cleaned, and ultimately joined at the annual level.'),
  SP(),

  H3('3.1.1 Meteorological Data'),
  SP(),
  B('Daily meteorological records for the Douro region were sourced from the BCM base climate files maintained by the University of Porto\'s viticulture research group (Professor Mário Cunha). The raw files (BCM_base_RDD.xls, BCM_base_RVV.xls) contain one row per calendar day from 1933-01-01 to 2022-12-31 for RDD (32,842 daily records) and from 1941-01-01 to 2022-11-13 for RVV (29,902 daily records). Each daily record contains the following primary meteorological variables:'),
  SP(),
  mkTable(
    ['Variable', 'Description', 'Unit'],
    [
      ['Tmax', 'Daily maximum temperature', '°C'],
      ['Tmin', 'Daily minimum temperature', '°C'],
      ['Tmed', 'Daily mean temperature', '°C'],
      ['Rain', 'Daily total rainfall', 'mm'],
      ['Sm', 'Soil water availability (model-derived)', 'mm'],
      ['Iaf', 'Leaf Area Index (phenological model)', 'm²/m²'],
      ['Eto', 'Reference evapotranspiration', 'mm'],
    ],
    [1500, 4000, 1500]
  ),
  CAP('Table 3.1. Primary daily meteorological variables in the raw source files.'),
  SP(),
  B('The soil water availability (Sm) and Leaf Area Index (Iaf) values are not direct measurements but model-derived estimates produced by the STICS vine growth model calibrated to each region\'s soil and vine characteristics by Professor Cunha\'s group over decades of field work. These modelled variables are critical for the analysis: Sm represents the balance between precipitation, evapotranspiration, and soil water-holding capacity integrated daily, while Iaf represents vine canopy development as a continuous daily estimate anchored to the vine\'s phenological state.'),
  SP(),
  B('In Stage 1 of the pipeline (script: 1_data_preparation_stage_1.R), the raw daily records were cleaned and five binary/threshold indicator variables were derived from the primary variables:'),
  SP(),
  mkTable(
    ['Derived indicator', 'Condition', 'Agronomic meaning'],
    [
      ['R>0.1', 'Rain > 0.1 mm', 'Wet day indicator (trace rainfall)'],
      ['Tmed<15', 'Tmed < 15°C', 'Cold day — impairs flowering and fruit set'],
      ['Tmax>35', 'Tmax > 35°C', 'Heat-stress day — causes flower abortion, accelerates ripening'],
      ['Tmin<0', 'Tmin < 0°C', 'Frost day — potential bud and shoot damage'],
      ['Sm<1.5Wp', 'Sm < 1.5 × wilting point', 'Severe soil water stress — below vine survival threshold'],
      ['Sm>0.9Fc', 'Sm > 0.9 × field capacity', 'Near-saturation — promotes disease, excessive vigour'],
    ],
    [1600, 2400, 3000]
  ),
  CAP('Table 3.2. Binary threshold indicators derived from raw daily meteorological variables in Stage 1.'),
  SP(),
  B('The wilting point (Wp) and field capacity (Fc) thresholds used in the Sm indicators were set based on soil profile parameters calibrated for each region. The resulting Stage 1 output is a clean daily dataset with 17 columns per region (dataPrep_meteo_rdd.csv, dataPrep_meteo_rvv.csv) spanning the full time horizon.'),
  SP(),

  H3('3.1.2 Phenology and Production Data'),
  SP(),
  B('Annual wine production records and vine phenological dates were sourced from the Pheno_Prd source files maintained by the same research group (Pheno_Prd_1933_2022_recente.xlsx for RDD; equivalent for RVV). Each annual record contains:'),
  SP(),
  mkTable(
    ['Variable', 'Description'],
    [
      ['Wine_mhl', 'Annual regional wine production (thousands of hectolitres)'],
      ['DOY_BB', 'Day of Year of Bud Break — start of vegetative growth'],
      ['DOY_Fl', 'Day of Year of Flowering — critical fruit-set window'],
      ['DOY_sM', 'Day of Year of Start of Maturity (veraison) — berry ripening begins'],
      ['DOY_Fs', 'Day of Year of Fruit Set — end of berry cell division'],
      ['DOY_Hv', 'Day of Year of Harvest — end of growing season'],
    ],
    [1500, 5500]
  ),
  CAP('Table 3.3. Annual phenology and production variables.'),
  SP(),
  B('The phenological dates (DOY_BB through DOY_Hv) are derived from the STICS phenological sub-model and represent the average calendar date of each key event across the regional vine population for each year. These dates vary by 2–4 weeks across years for each stage, reflecting the sensitivity of vine phenology to temperature — warmer springs advance flowering and harvest, cooler springs delay them. The variability in these anchor dates is what makes phenology-based windowing more informative than fixed calendar windows: a feature computed as "the 20 days before Harvest" adapts to the actual harvest date of each year rather than always covering, say, the first three weeks of September.'),
  SP(),
  B('The production series for RDD spans 1933–2022 (90 observations), but the feature construction starts at 1934 to allow for previous-year (y0) features — leaving 89 usable observations. For RVV, the series spans 1942–2021 with 80 usable observations. The production values show the structural trajectories described in Chapter 2: RDD production ranging from 518 to 1,961 mhl with a mean of 1,116 mhl and an upward trend; RVV production ranging from 494 to 3,443 mhl with a mean of 1,621 mhl and a sharp downward trend since the 1960s.'),
  SP(),

  H2('3.2 Feature Engineering — From Daily Records to Annual Features'),
  SP(),
  B('The core methodological contribution of the data pipeline is the transformation from 32,842 (RDD) and 29,902 (RVV) daily records into 59 annual agroclimatic features per region. This transformation is performed by script 2_ features creation.R and is anchored entirely on the phenological dates for each year. The logic is straightforward in principle but required extensive domain expertise and iterative refinement to implement correctly: for each year y1, each combination of (meteorological variable × phenological anchor × window position × window width × year offset) defines one feature.'),
  SP(),

  H3('3.2.1 Feature Naming Convention'),
  SP(),
  B('All 59 features follow a systematic naming convention that encodes all relevant information about how the feature was computed:'),
  SP(),
  MONO('{variable}_{stage}_{year}_{position}{width}'),
  SP(),
  B('where:'),
  SP(),
  mkTable(
    ['Component', 'Options', 'Meaning'],
    [
      ['variable', 'tm, tn, swa, iaf, rf, days.rf, sw.less.15wp, sw.above.09fc, days.maxTemp.above.35, tm.days.less.15, days.minTemp.less.0', 'The meteorological quantity being aggregated'],
      ['stage', 'bb (Bud Break), fl (Flowering), sM (Start Maturity), hv (Harvest)', 'The phenological anchor point for the window'],
      ['year', 'y1 (current harvest year), y0 (previous year)', 'Which growing season the feature refers to'],
      ['position', 'c (centred), b (before), a (after)', 'Whether the window is centred on, before, or after the stage date'],
      ['width', '5, 10, 15, 20, 30 (days)', 'Half-width (centred) or full width (before/after) of the aggregation window'],
    ],
    [1200, 2800, 3000]
  ),
  CAP('Table 3.4. Feature naming convention components.'),
  SP(),
  B('For example: swa_hv_y1_b20 = soil water availability (swa), in the 20 days BEFORE (b20) Harvest (hv), in the current year (y1). tm_fl_y0_b10 = mean temperature (tm), in the 10 days BEFORE (b10) Flowering (fl), in the PREVIOUS year (y0). iaf_fl_y1_a10 = Leaf Area Index (iaf), in the 10 days AFTER (a10) Flowering (fl), in the current year (y1).'),
  SP(),
  B('The choice of which (stage × position × width × year) combinations to include was guided by agronomic knowledge of the critical development windows established in the literature review (Chapter 2) and by exploratory correlation analysis against the response variable. Not all possible combinations were included: the final 59 features represent those with either strong a priori agronomic justification or demonstrated empirical correlation with production in at least one of the two regions.'),
  SP(),

  H3('3.2.2 Feature Groups and Counts'),
  SP(),
  B('Table 3.5 summarises the 59 features by variable family and their counts. The distribution reflects the relative importance of different variable types in the viticulture literature: temperature variables (tm, tn) are most numerous because temperature governs nearly every phenological process; soil water and LAI variables are next because they are the primary within-season stress indicators; rainfall and heat/cold day counts complete the picture.'),
  SP(),
  mkTable(
    ['Variable family', 'Symbol', 'Count', 'What it measures'],
    [
      ['Mean daily temperature', 'tm', '12', 'Average thermal conditions at key phenological windows'],
      ['Leaf Area Index', 'iaf', '10', 'Vine canopy development — proxy for vegetative vigour'],
      ['Soil water deficit days', 'sw.less.15wp', '8', 'Days when soil water < 1.5× wilting point — severe stress'],
      ['Rainfall event count', 'days.rf.above.1mm', '6', 'Number of days with rainfall > 1 mm — disease pressure proxy'],
      ['Soil water availability', 'swa', '5', 'Absolute soil water balance at harvest and flowering windows'],
      ['Soil saturation days', 'sw.above.09fc', '4', 'Days when soil water > 90% field capacity — excess moisture'],
      ['Heat stress days (Tmax > 35°C)', 'days.maxTemp.above.35 / days.tempMax.above.35', '4', 'Extreme heat events — flower abortion and thermal damage'],
      ['Rainfall sum', 'rf', '3', 'Total precipitation at harvest and flowering windows'],
      ['Cold days (Tmed < 15°C)', 'tm.days.less.15', '2', 'Cool days impairing pollination and fruit set'],
      ['Frost days (Tmin < 0°C)', 'days.minTemp.less.0', '2', 'Frost events — bud and shoot damage risk'],
      ['Minimum temperature', 'tn', '1', 'Night temperature at harvest — berry quality proxy'],
      ['Previous-year soil water', 'y0 variants of swa/sw', '2', 'Carry-over water stress from prior season'],
    ],
    [2400, 1200, 700, 4000]
  ),
  CAP('Table 3.5. Feature groups, counts, and agronomic meanings. Total: 59 features per region.'),
  SP(),

  H3('3.2.3 Current-Year vs Previous-Year Features'),
  SP(),
  B('A key design decision in the feature engineering was the inclusion of previous-year (y0) features alongside current-year (y1) features. As discussed in Section 2.2, vine physiology has a two-year temporal structure: inflorescence primordia that determine current-year bunch number are formed in the prior growing season, and vine carbohydrate reserves that determine shoot vigour are accumulated during the prior year\'s harvest-to-dormancy period. Failing to include prior-year features would miss this carry-over mechanism entirely.'),
  SP(),
  B('In the final feature matrix, 15 of the 59 features (25%) use y0 (previous-year) anchors. The most agronomically motivated y0 features are:'),
  SP(),
  BM([{ text: 'tm_fl_y0_b10 ', bold: true }, { text: '(mean temperature in the 10 days before previous-year flowering): captures the thermal conditions governing inflorescence primordia formation in the prior season — the mechanism that determines current-year bunch potential.' }]),
  SP(),
  BM([{ text: 'tm_bb_y0_a30 ', bold: true }, { text: '(mean temperature in the 30 days after previous-year bud break): captures the early-season thermal conditions that initiate the prior year\'s vegetative growth phase, influencing vine carbohydrate allocation.' }]),
  SP(),
  BM([{ text: 'tm_sm_y0_a20 ', bold: true }, { text: '(mean temperature in the 20 days after previous-year start of maturity): captures the heat load during the prior year\'s ripening phase, which governs both berry quality in the prior year and the vine\'s thermal stress exposure at the end of the growing season.' }]),
  SP(),
  BM([{ text: 'sw.above.09fc_fl_y0_a20 ', bold: true }, { text: '(soil saturation days in the 20 days after previous-year flowering): captures prior-year excess soil moisture during flowering — the carry-over mechanism identified in RVV CarenR Rule 2 (see Section 4.1.2).' }]),
  SP(),

  H3('3.2.4 Feature Discretisation for CarenR'),
  SP(),
  B('The continuous feature matrix (dataPrep_cont_dataset_*.csv, 59 columns × 89/80 rows) is used directly by M5Rules and RIPPER, which perform their own internal threshold finding. For CarenR, a separate discretised version of the dataset (dataPrep_dis_rule_dataset_*.csv) is produced by script 2_ features creation.R using equal-frequency binning into four quartile intervals. Each feature is independently divided into four bins, each containing approximately 25% of the training observations, with the boundaries represented as interval notation (e.g. [18.3|22.4] for the bin from 18.3°C to 22.4°C).'),
  SP(),
  B('The equal-frequency choice, rather than equal-width or domain-knowledge-based breakpoints, ensures that each bin is populated by a comparable number of observations. This is important for the KS test used by CarenR: a bin with only two or three observations would produce an unreliable distributional comparison regardless of its meteorological significance. In the walk-forward scenarios, the bin boundaries are re-estimated within each training fold, ensuring that the discretisation reflects the distribution of climate conditions actually observed in the training period — not the full historical record that would be unknowable at prediction time.'),
  SP(),

  H2('3.3 Data Pipeline Summary'),
  SP(),
  B('Figure 3.1 summarises the complete data pipeline from raw sources to the final modelling datasets. The pipeline was implemented as a sequence of seven R scripts coordinated by a master runner (run_all_pipelines.R), with all intermediate datasets written to disk at each stage to allow inspection and validation.'),
  SP(),
  mkTable(
    ['Stage', 'Script', 'Input', 'Output', 'Key transformation'],
    [
      ['0 — Raw data', '(external)', 'BCM_base*.xls\nPheno_Prd*.xlsx', 'Raw Excel files', 'Daily climate + annual phenology/production from field stations and STICS model'],
      ['1 — Daily prep', '1_data_preparation_stage_1.R', 'Raw Excel files', 'dataPrep_meteo_*.csv\ndataPrep_production_*.csv', 'Column standardisation, binary threshold indicators (Tmax>35, Sm<1.5Wp etc.), date decomposition. 32,842 rows (RDD), 29,902 rows (RVV).'],
      ['2 — Feature engineering', '2_ features creation.R', 'dataPrep_meteo_*.csv\ndataPrep_production_*.csv', 'dataPrep_cont_dataset_*.csv\ndataPrep_dis_rule_dataset_*.csv', 'Phenological window aggregation → 59 annual features per region. Equal-frequency discretisation (4 bins) for CarenR. 89 rows (RDD), 80 rows (RVV).'],
      ['3 — Rule discovery', '3_carenR__application.R\n4_ripper_pipeline.R\n5_m5rules_pipeline.R', 'dataPrep_*_dataset_*.csv', 'RulesTemp_*_linear.csv\nripper_rules_*.txt\nm5rules_rules_*.txt', 'Full-dataset rule induction for all three algorithms. CarenR: 48 rules (RDD), 20 rules (RVV).'],
      ['4 — Walk-forward validation', '6_scenario_validation.R', 'dataPrep_cont_dataset_*.csv', 'scenario_metrics_*.csv\nscenario_predictions_*.csv\nscenario_summary_*.csv', '17-model × 18/16 scenario expanding-window evaluation. All preprocessing within fold.'],
      ['5 — Rule interpretation', '9_rule_interpreter.R', 'RulesTemp_*.csv\nripper_rules_*.txt\nm5rules_rules_*.txt', 'rules_interpreted_*.txt/.csv\nripper_interpreted_*.txt\nm5rules_interpreted_*.txt', 'Translation of raw rule outputs to plain-English descriptions with agronomic pathway annotations.'],
    ],
    [1000, 1600, 1600, 2000, 3000]
  ),
  CAP('Table 3.6. Complete data pipeline from raw sources to final outputs. Each stage writes intermediate datasets to disk for auditability.'),
  SP(),
  B('The entire pipeline from Stage 1 through Stage 5 is reproducible by running run_all_pipelines.R, which sequences all scripts in order with consistent configuration parameters centralised in utils/config.R. Key configuration choices — detrend method (linear), CarenR support threshold (15%), significance threshold (p ≤ 0.10), minimum training fraction (80%), and feature selection strategy per variant — are documented in that file and were fixed before the final analysis run.'),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter3_data_pipeline.docx', buf);
  console.log('Done: chapter3_data_pipeline.docx');
});

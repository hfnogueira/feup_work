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
  H1('Appendix B — Exploratory Data Analysis'),
  SP(),
  B('This appendix presents the exploratory data analysis of the two regional datasets. The analyses were conducted prior to model development and informed several key methodological decisions: the choice of linear detrending, the inclusion of previous-year (y0) features, the identification of likely era-confounded variables, and the feature selection strategy for CarenR. All statistics are computed from the final curated datasets (dataPrep_cont_dataset_rdd.csv and dataPrep_cont_dataset_rvv.csv) unless otherwise noted.'),
  SP(),

  H2('B.1 Wine Production — Descriptive Statistics and Trends'),
  SP(),

  B('Table B.1 presents the key descriptive statistics for wine production in both regions over the full study period.'),
  SP(),

  mkTable(
    ['Statistic', 'RDD (Douro)\n1934–2022', 'RVV (Vinho Verde)\n1942–2021'],
    [
      ['Observations (n)', '89 years', '80 years'],
      ['Mean production', '1,116 mhl', '1,621 mhl'],
      ['Standard deviation', '307 mhl', '751 mhl'],
      ['Coefficient of variation (CV)', '27.6%', '46.4%'],
      ['Minimum', '518 mhl (1952)', '494 mhl (1997)'],
      ['Maximum', '1,961 mhl (1990)', '3,443 mhl (1962)'],
      ['Linear trend slope', '+7.4 mhl/year (increasing)', '−21.2 mhl/year (declining)'],
      ['Total change over period', '+~660 mhl (~+80%)', '−~1,500 mhl (~−65%)'],
    ],
    [3000, 3000, 3000]
  ),
  CAP('Table B.1. Descriptive statistics for annual wine production in both regions.'),
  SP(),

  B('The contrast between the two regions is immediately apparent. Vinho Verde has a substantially higher mean production (1,621 vs 1,116 mhl) and nearly three times the standard deviation (751 vs 307 mhl), yielding a CV of 46.4% vs 27.6%. This higher variability in RVV is partly structural — the larger absolute production values amplify variation — but also reflects the stronger era-driven structural changes in the RVV series. The coefficient of variation is a key number for understanding the prediction task: with CV = 27.6% for RDD and 46.4% for RVV, any model must reduce the residual error substantially below the naive prediction to be useful, which is a high bar with a climate signal explaining only ~22% of variance.'),
  SP(),

  H3('B.1.1 Decade-Level Production Averages'),
  SP(),
  B('Table B.2 presents decade-level mean production for both regions, revealing the structural trajectory of each series.'),
  SP(),

  mkTable(
    ['Decade', 'RDD mean (mhl)', 'RDD n', 'RVV mean (mhl)', 'RVV n'],
    [
      ['1930s', '765', '7',  '—',   '—'],
      ['1940s', '767', '10', '2,026', '9'],
      ['1950s', '926', '10', '1,941', '10'],
      ['1960s', '1,184', '10', '2,331', '10'],
      ['1970s', '1,090', '10', '2,229', '10'],
      ['1980s', '1,148', '10', '1,638', '10'],
      ['1990s', '1,271', '10', '1,259', '10'],
      ['2000s', '1,392', '10', '906',   '10'],
      ['2010s', '1,308', '10', '798',   '10'],
      ['2020s', '1,287', '3',  '871',   '2'],
    ],
    [1400, 1800, 1100, 1800, 1100]
  ),
  CAP('Table B.2. Decade-level mean wine production. RDD shows gradual increase; RVV shows sharp decline from the 1980s onward.'),
  SP(),

  B('The decade table makes the structural stories of both regions clearly visible. RDD production roughly doubled between the 1930s–40s (765–767 mhl) and the 2000s–2010s (1,300–1,400 mhl), with the most rapid growth occurring in the 1950s–60s and again in the 1990s–2000s. The slowdown in the 2010s is consistent with the maturation of the Douro DOC expansion and the administrative constraints of the Port benefício quota system. For RVV, the peak decade was the 1960s (2,331 mhl), with a sharp decline beginning in the 1980s (1,638 mhl) continuing steeply into the 2000s (906 mhl) — exactly coinciding with Portugal\'s EU accession (1986) and the structural reforms that followed. By the 2010s, RVV production had fallen to approximately one-third of its 1960s peak.'),
  SP(),
  B('These structural trajectories directly explain the era confound described throughout the thesis. In both regions, the linear trend does not fully capture the non-linear shape of the production history — particularly the RVV plateau in the 1970s (2,229 mhl, barely below the 1960s peak) followed by a sharp break in the 1980s. This structural break is the primary reason LOESS detrending performs better than linear detrending for RVV (Section 5.5): the flexible LOESS curve can fit the plateau-then-collapse shape, while the linear trend averages over it, leaving systematic residuals in the 1970s.'),
  SP(),

  H2('B.2 Phenological Timing — Trends and Variability'),
  SP(),
  B('The phenological dates used as anchors for feature engineering are themselves variable across years and show systematic advancement over the study period, consistent with the warming trends documented in the broader viticulture literature. Table B.3 summarises the mean, variability, and long-term trend for each key phenological stage.'),
  SP(),

  mkTable(
    ['Stage', 'RDD mean\n(DOY / date)', 'RDD SD\n(days)', 'RDD trend\n(days/decade)', 'RVV mean\n(DOY / date)', 'RVV SD\n(days)', 'RVV trend\n(days/decade)'],
    [
      ['Bud Break (BB)',     'DOY 88 / 27 Mar', '8.4', '−0.5', 'DOY 90 / 30 Mar', '9.2', '−1.0'],
      ['Flowering (Fl)',     'DOY 135 / 14 May', '9.8', '−0.7', 'DOY 154 / 2 Jun', '12.0', '−1.1'],
      ['Start Maturity (sM)','DOY 179 / 27 Jun', '8.6', '−0.9', 'DOY 227 / 14 Aug', '13.6', '−1.7'],
      ['Harvest (Hv)',       'DOY 257 / 13 Sep', '13.3', '−1.9', 'DOY 270 / 26 Sep', '17.0', '−2.6'],
    ],
    [1800, 1600, 900, 1400, 1600, 900, 1400]
  ),
  CAP('Table B.3. Phenological timing statistics. Negative trend = earlier occurrence over time. All trends in days per decade.'),
  SP(),

  B('Several findings stand out. First, harvest timing is advancing most rapidly in both regions — by approximately 1.9 days per decade in RDD and 2.6 days per decade in RVV. Over the 80–90 year study period this represents an advancement of roughly 15–23 days, consistent with the warming of 1.3°C in growing-season temperature projected and partly already observed in Portuguese wine regions (Cunha and Richter, 2016). Earlier harvest is a well-documented consequence of warming: grapes accumulate heat units faster, reaching physiological maturity sooner. Second, the trend is stronger in RVV than in RDD for every stage, which is consistent with the Atlantic climate\'s stronger sensitivity to warming — in a wetter regime, higher temperatures have a disproportionate effect on evapotranspiration and vine water status, accelerating phenological development.'),
  SP(),
  B('Second, the variability (SD) in phenological dates is larger in RVV than RDD for all stages, particularly for start of maturity (13.6 vs 8.6 days) and harvest (17.0 vs 13.3 days). This higher phenological variability in RVV is consistent with the region\'s more variable Atlantic weather: ocean-driven temperature swings create larger year-to-year variation in the timing of heat accumulation. This greater phenological variability also means that fixed-calendar aggregation windows would be a poor substitute for the phenological-anchor approach used in this thesis: a fixed "September temperature" window would capture completely different vine developmental states in an early harvest year vs a late harvest year.'),
  SP(),

  H3('B.2.1 Growing Season Duration'),
  SP(),
  B('Table B.4 shows the mean duration of each inter-stage phase for both regions.'),
  SP(),

  mkTable(
    ['Phase', 'RDD mean', 'RDD SD', 'RVV mean', 'RVV SD', 'Interpretation'],
    [
      ['Budburst → Flowering', '47.0 days', '7.9d', '64.3 days', '9.0d', 'RVV flowering 17d later than RDD on average — cooler Atlantic spring'],
      ['Flowering → Veraison', '44.8 days', '4.9d', '73.0 days', '6.9d', 'RVV takes 28d longer to reach veraison — slower ripening in Atlantic climate'],
      ['Veraison → Harvest', '78.1 days', '6.5d', '42.7 days', '7.7d', 'RDD takes longer from veraison to harvest — slower final ripening phase'],
      ['Full season (BB→Hv)', '169.9 days', '12.6d', '180.0 days', '13.8d', 'RVV growing season ~10 days longer overall'],
    ],
    [1800, 1100, 900, 1100, 900, 3300]
  ),
  CAP('Table B.4. Mean growing season phase durations for both regions.'),
  SP(),

  B('The phase durations reveal important differences in growing season structure. The flowering-to-veraison phase is 28 days longer in RVV (73 vs 45 days) — the Atlantic climate\'s slower heat accumulation extends this key berry development phase. Conversely, the veraison-to-harvest phase is substantially longer in RDD (78 vs 43 days), reflecting that Douro grapes undergo a longer slow-ripening phase under the drier semi-arid conditions before reaching harvest maturity. These structural differences mean that features from the same phenological window cover different absolute calendar periods in the two regions, and directly motivate the use of separate feature construction and model fitting for each region rather than a pooled approach.'),
  SP(),

  H2('B.3 Feature Correlations with Production'),
  SP(),
  B('Table B.5 shows the ten features most strongly correlated with wine production residuals (detrended) in each region. This analysis was part of the exploratory phase that informed the feature selection strategies evaluated in CarenR.'),
  SP(),

  mkTable(
    ['Rank', 'RDD feature', 'RDD |r|', 'Direction', 'RVV feature', 'RVV |r|', 'Direction'],
    [
      ['1', 'tm_hv_y1_c10\n(harvest temp)', '0.386', '+', 'iaf_hv_y1_c10\n(LAI at harvest)', '0.404', '−'],
      ['2', 'tn_hv_y0_a30\n(prev-yr harvest nights)', '0.347', '+', 'tm_hv_y1_b20\n(pre-harvest temp)', '0.402', '−'],
      ['3', 'swa_fl_y1_a30\n(post-fl soil water)', '0.283', '−', 'tm_sm_y1_c20\n(maturity temp)', '0.342', '−'],
      ['4', 'iaf_fl_y0_b20\n(prev-yr fl LAI)', '0.276', '−', 'tn_hv_y0_a30\n(prev-yr harvest nights)', '0.325', '−'],
      ['5', 'iaf_fl_y0_b30\n(prev-yr fl LAI, wide)', '0.268', '−', 'days.rf.above.1mm_fl_y1_a20\n(wet days post-fl)', '0.306', '−'],
      ['6', 'swa_fl_y1_a15\n(post-fl soil water)', '0.261', '−', 'tm_sm_y0_a20\n(prev-yr summer temp)', '0.302', '−'],
      ['7', 'tm_hv_y1_b20\n(pre-harvest temp)', '0.248', '+', 'rf_fl_y1_a10\n(post-fl rainfall)', '0.288', '−'],
      ['8', 'tm.days.less.15_fl_y1_b15\n(cold days at fl)', '0.245', '−', 'days.rf.above.1mm_fl_y1_c15\n(wet days at fl)', '0.264', '−'],
      ['9', 'iaf_fl_y0_b10\n(prev-yr fl LAI)', '0.227', '−', 'tm_hv_y1_c10\n(harvest temp)', '0.253', '−'],
      ['10', 'swa_hv_y1_b20\n(harvest soil water)', '0.223', '−', 'swa_hv_y1_b20\n(harvest soil water)', '0.245', '+'],
    ],
    [500, 2200, 700, 700, 2200, 700, 700]
  ),
  CAP('Table B.5. Top 10 features by absolute Pearson correlation with wine production residuals (detrended). Direction: + = positive correlation with production, − = negative.'),
  SP(),

  B('Several observations from this correlation table are noteworthy. First, swa_hv_y1_b20 (harvest soil water) appears in the top 10 for both regions, but with opposite signs — negative (r = −0.223) for RDD and positive (r = +0.245) for RVV. This is the same directional inversion identified in the CarenR rules (Section 4.1.3), now confirmed at the simple correlation level. Second, the top RDD correlate is harvest temperature (r = +0.386), which does not prominently appear in the top CarenR rules because CarenR uses discretised variables and the KS test detects distributional shift rather than linear correlation. The harvest temperature signal is captured by M5Rules (positive coefficients) and RIPPER (harvest temperature in Rules 1–2) instead. Third, the top RVV correlate is LAI at harvest (r = −0.404), consistent with its dominance in the CarenR RVV rule set. The negative sign confirms that higher LAI reduces production — the canopy vigour mechanism discussed throughout Chapter 4.'),
  SP(),

  H2('B.4 Feature Drift — The Era Confound Quantified'),
  SP(),
  B('A critical question for interpreting the CarenR rules is which features are genuinely informative about inter-annual climate variation and which are primarily capturing the long-term structural trend (era membership). Features strongly correlated with calendar year are suspected era-confounders: they co-vary with the production trend for structural reasons rather than because they drive production through a climate mechanism. Table B.6 shows the features most strongly correlated with year for both regions.'),
  SP(),

  mkTable(
    ['Rank', 'RDD feature', 'r(feat, year)', 'Concern level', 'RVV feature', 'r(feat, year)', 'Concern level'],
    [
      ['1', 'tm_hv_y1_c10\n(harvest temp)', '+0.412', 'HIGH — warming trend', 'tm_sm_y1_c20\n(maturity temp)', '+0.402', 'HIGH — warming trend'],
      ['2', 'tm_hv_y1_b20\n(pre-harvest temp)', '+0.373', 'HIGH', 'iaf_hv_y1_c10\n(LAI at harvest)', '+0.359', 'MODERATE'],
      ['3', 'tn_hv_y0_a30\n(prev-yr harvest nights)', '+0.371', 'HIGH — warming', 'tm_sm_y0_b20\n(prev-yr maturity temp)', '+0.348', 'HIGH'],
      ['4', 'tm_sm_y0_a20\n(prev-yr summer temp)', '+0.269', 'MODERATE', 'tm_hv_y1_b20\n(pre-harvest temp)', '+0.335', 'HIGH'],
      ['5', 'days.rf.above.1mm_hv\n(harvest rain days)', '−0.201', 'LOW', 'tm_sm_y0_a20\n(prev-yr summer temp)', '+0.295', 'MODERATE'],
    ],
    [500, 2200, 1100, 1200, 2200, 1100, 1200]
  ),
  CAP('Table B.6. Top features correlated with calendar year — potential era confounders. Features marked HIGH co-trend strongly with the production structural trend.'),
  SP(),

  B('The harvest temperature variable (tm_hv_y1_c10) is the most era-confounded feature in the dataset: it correlates with production at r = +0.386 (RDD) but also correlates with year at r = +0.412. This near-equal correlation with both production and time is a warning sign: the feature may appear predictive of production largely because both are trending upward over the study period (warming climate + increasing Douro production), rather than because harvest temperature genuinely drives production inter-annually. The LOESS validation (Section 4.2) addresses this precisely: after removing the era trend, the top features\'  correlations drop from ~0.51 to ~0.47, confirming that the era confound inflates but does not manufacture the relationship. For harvest temperature specifically, the LOESS-corrected correlation would likely drop more, which is consistent with CarenR\'s top rules focusing on soil water and rainfall rather than temperature — these latter variables have lower year-correlations (swa_hv_y1_b20: year r = −0.183 for RDD) and represent cleaner within-era signals.'),
  SP(),
  B('The RVV LAI at harvest (iaf_hv_y1_c10) shows a more interesting pattern: it correlates with production at r = −0.404 but also with year at r = +0.359. This suggests that LAI has been increasing over the study period — consistent with the general intensification of vineyard management and the shift to espalier systems — and this increasing LAI co-trends with declining production, inflating its apparent correlation. However, the LOESS validation confirms that even after era-trend removal, LAI remains one of the strongest within-era predictors in RVV, confirming its genuine within-era signal. The era confound inflates its correlation but does not invent the relationship.'),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/appendix_b_eda.docx', buf);
  console.log('Done: appendix_b_eda.docx');
});

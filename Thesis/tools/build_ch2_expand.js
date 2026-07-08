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

const children = [
  H1('2. Literature Review (Additions: Sections 2.8 and 2.9)'),
  SP(),
  B('This document contains two new sections for Chapter 2: Section 2.8 on Portuguese viticulture industry structure and policy, and Section 2.9 on time series properties and their implications for model evaluation. Both sections fill gaps identified in the existing literature review.'),
  SP(),

  // ── 2.8 Viticulture Policy ────────────────────────────────────────────────
  H2('2.8 Portuguese Viticulture Industry Structure and Policy'),
  SP(),
  B('The structural production trends observed in the two study regions — a persistent increase in the Douro and a sharp decline in Vinho Verde — cannot be attributed to climate alone. Both trajectories were substantially shaped by regulatory, economic, and demographic forces that are independent of meteorological conditions. Understanding these structural drivers is essential for interpreting the era confound identified throughout this thesis and for correctly attributing the limitations of the predictive models.'),
  SP(),

  H3('2.8.1 Douro Region — Regulatory and Market Drivers'),
  SP(),
  B('The Douro wine region has been formally demarcated since 1756, making it one of the first wine appellations in the world. The most significant modern regulatory influence on production is the Port wine benefício system, administered by the Instituto dos Vinhos do Douro e do Porto (IVDP). The benefício is an annual administrative quota that determines how much of the Douro\'s grape harvest may be used to produce Port wine, which commands a premium price over unfortified Douro wines. The quota is set annually by the IVDP based on market conditions, stock levels, and quality assessments, and has varied substantially across decades. Because Port production represents a significant fraction of total Douro output, the benefício quota partially decouples observed production from pure climate effects: in years where Port quota is restricted, total production is administratively capped regardless of how favourable the climate was. This administrative component is not captured by any of the meteorological features in the dataset and represents an unobserved structural covariate.'),
  SP(),
  B('The second major structural driver of RDD production growth was the creation of the Douro DOC (Denominação de Origem Controlada) for unfortified table wines in 1979. Before this designation, Douro grapes were used almost exclusively for Port wine. The Douro DOC enabled Port wine houses and independent producers to bottle and market premium dry red and white table wines from the region, opening new market channels. From the 1980s onward, Douro table wine production grew rapidly alongside Port, contributing to the gradual increase in total regional production documented in Appendix B. This structural expansion is largely responsible for the upward trend in the RDD production series and is the primary reason the RDD time series is less confounded by non-climate structural changes than the RVV series.'),
  SP(),

  H3('2.8.2 Vinho Verde Region — EU Policy and Structural Transformation'),
  SP(),
  B('The Vinho Verde region\'s dramatic production decline from the 1960s to the present is one of the most striking structural transformations in Portuguese viticulture. Three interacting factors explain the collapse from approximately 2,300 mhl in the 1960s to around 800 mhl today.'),
  SP(),
  B('Portugal\'s accession to the European Economic Community (now European Union) in January 1986 triggered a sequence of structural adjustments that profoundly reshaped the RVV. The Common Agricultural Policy (CAP) introduced grubbing-up subsidies — payments to farmers who permanently removed vineyard plantings — as part of an effort to reduce wine surpluses across member states. The Vinho Verde region, dominated by small family-operated plots producing bulk wine for local and low-value export markets, was particularly exposed to these incentives. Thousands of small producers received grubbing-up payments and removed vineyards that were marginally profitable or required high labour input, accelerating a process of plot abandonment that had already begun with rural exodus in the 1960s and 1970s.'),
  SP(),
  B('The EU quality standards introduced post-accession also penalised the traditional high-yield pergola (ramada) training system used across the Minho. Pergola systems, which train vines on high horizontal trellises above ground level, maximise vegetative growth and can produce 15,000–20,000 kg/ha — far above the yield limits imposed by EU quality regulations for premium appellations. The incentive to qualify for premium designations drove a progressive conversion from pergola to lower-yield espalier (cordon) training systems, which produce 6,000–10,000 kg/ha. This training system transition reduced per-hectare yields structurally, independently of climate, and is estimated to have contributed substantially to the post-1986 production decline.'),
  SP(),
  B('The third factor was demographic: the Minho region had experienced significant rural exodus from the 1960s onward as workers migrated to urban centres and abroad. The generation that maintained the region\'s characteristic small, labour-intensive plots retired or emigrated, and their children often did not continue the viticultural tradition. Plot fragmentation — the Minho has historically had some of the highest incidences of micro-holding in Portugal, with individual parcels often below 0.1 hectares — made consolidation difficult and increased the per-unit cost of cultivation. Many marginal plots were simply abandoned, reducing the effective cultivated area and contributing to the production decline independently of the EU policy shock.'),
  SP(),
  B('These structural factors directly explain why the era confound is substantially stronger in RVV than in RDD: the production decline in Vinho Verde is predominantly driven by non-climate forces, creating a strong co-variation between era membership and climate variables that makes it difficult to disentangle genuine inter-annual climate effects from structural level changes. The models in this thesis acknowledge and partially address this confound through linear detrending and the LOESS sensitivity analysis, but fully removing it would require incorporating vineyard area, number of certified producers, and training system composition as explicit detrending covariates.'),
  SP(),

  // ── 2.9 Time Series Properties ────────────────────────────────────────────
  H2('2.9 Time Series Properties and Evaluation Methodology'),
  SP(),
  B('Annual wine production data is a time series with properties that have important implications for model evaluation. This section reviews the relevant concepts and motivates the methodological choices made in Chapter 3.'),
  SP(),

  H3('2.9.1 Non-Stationarity and Structural Breaks'),
  SP(),
  B('A time series is stationary if its mean, variance, and autocovariance structure do not change over time. Both regional production series are non-stationary in the strict sense: the RDD series has a positive mean trend and the RVV series has a strongly negative trend. More subtly, both series exhibit structural breaks — abrupt changes in the level or trend that cannot be attributed to normal year-to-year variability. In the RVV series, the most prominent structural break occurs in the mid-1980s, coinciding with EU accession and the subsequent grubbing-up subsidies. In the RDD series, the structural break associated with the Douro DOC creation in 1979 is more gradual but identifiable in the decade-level averages (Appendix B, Table B.2).'),
  SP(),
  B('Non-stationarity poses a direct challenge for machine learning models: a model trained on data from one period of a non-stationary series may not generalise to data from a different period. The standard practice in such settings is to remove the deterministic trend component before modelling, leaving stationary residuals that represent genuine inter-annual variation around the structural trajectory. This is precisely the role of the detrending step described in Section 3.3. The appropriateness of linear vs. flexible detrending depends on the shape of the structural trend — a question examined empirically through the LOESS sensitivity analysis in Section 5.5.'),
  SP(),

  H3('2.9.2 Why Random Cross-Validation Is Invalid for Time Series'),
  SP(),
  B('The standard approach to model evaluation in machine learning — k-fold cross-validation with random assignment of observations to folds — is inappropriate for time series data. Random cross-validation implicitly assumes that observations are exchangeable: that a model trained on a random subset of years can validly predict another random subset. This assumption is violated in at least two ways for wine production time series.'),
  SP(),
  B('First, temporal autocorrelation means that adjacent years are more similar to each other than randomly selected years. A model that is "tested" on a year immediately surrounded by training years will have access, through temporal proximity, to information that would not be available in a genuine forecasting application. This inflates apparent model performance.'),
  SP(),
  B('Second, the structural non-stationarity described above means that the statistical properties of the series change over time. A model trained on a random mixture of early-era and modern-era observations is effectively trained on a stationary average of two different regimes, which does not correspond to any real forecasting scenario. In practice, a forecaster always trains on the past and predicts the future — not on a random mixture of years across the full history.'),
  SP(),
  B('Walk-forward (expanding window) cross-validation addresses both problems by strictly respecting the temporal ordering of observations: the model is always trained on all years up to time t and evaluated on year t+1 or later. This is the methodology adopted in Chapter 3 and represents the methodologically correct approach for time series model evaluation. The expanding rather than rolling (fixed-window) variant was chosen because the relevant climate patterns were expected to be relatively stable over the study period (each additional year of training data adds information without obsoleting the oldest observations), and because the expanding window avoids the instability of very short training windows in the early scenarios.'),
  SP(),

  H3('2.9.3 The Naive Baseline and What It Measures'),
  SP(),
  B('A central benchmark throughout this thesis is the Naive_Median baseline — the model that always predicts the training-set median residual (approximately zero after detrending, i.e. "on trend"). This baseline has an important theoretical property: the median minimises mean absolute error among all constant predictors. Therefore, no model that predicts a constant for every year can beat the Naive_Median on MAE, and any model that beats it must be doing so by identifying non-trivial structure in the data.'),
  SP(),
  B('The Naive_Median MAE can be interpreted as the minimum error achievable by any model that ignores the climate features entirely — it represents the "cost of not knowing the climate" in a given scenario. If the MAE from a climate-aware model is lower than the Naive_Median, the model has successfully extracted information from the climate features that reduces prediction error. If it is higher, the model has actively misused the climate information — adding noise rather than signal.'),
  SP(),
  B('The fact that no model consistently beats the Naive_Median in this study therefore means not that climate is irrelevant to wine production (the CarenR rules demonstrate it is not) but that the climate signal is insufficient in magnitude and consistency to overcome year-to-year production noise in a held-out forecasting evaluation. This distinction between "the signal exists" and "the signal is strong enough to beat the baseline" is the central finding of Chapter 5 and motivates the two-decision decomposition framework described in Section 3.3.'),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter2_expand.docx', buf);
  console.log('Done: chapter2_expand.docx');
});

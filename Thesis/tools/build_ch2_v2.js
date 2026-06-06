// Chapter 2 v2 — adds Section 2.6.2 CarenR dedicated subsection
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
  H1('2. Literature Review'),
  SP(),
  B('This chapter reviews the theoretical and empirical foundations of the thesis across three interconnected domains: the agroclimatic drivers of wine production in Portuguese wine regions; quantitative approaches to yield prediction, from classical regression to modern machine learning; and the data mining methods — subgroup discovery and distribution rules — that underpin the CarenR framework used in this study. The chapter concludes by identifying the gap that this thesis addresses and positions the contribution relative to the current state of the art.'),
  SP(),

  H2('2.1 Regional Wine Production in Portugal'),
  SP(),
  B('Portugal is home to several wine regions with distinct climatic identities. This thesis focuses on two: the Douro Demarcated Region (RDD) and the Vinho Verde Region (RVV). Together they represented approximately 32% of Portugal\'s total vineyard area and 37% of national wine production in 2018 (Cunha and Richter, 2020). Despite their geographical proximity in northern Portugal, the two regions have markedly different climates and production histories, making them a natural comparative setting for agroclimatic modelling.'),
  SP(),

  H3('2.1.1 Douro Region'),
  SP(),
  B('The Douro wine region is located in north-eastern Portugal, one of the oldest demarcated wine regions in the world. Its topography is dominated by steep schist terraces on the hillsides of the Douro valley. The climate is continental and semi-arid: mean growing-season temperatures reach approximately 19.5°C (Köppen Csa), annual precipitation ranges from 400 to 800 mm concentrated outside the growing season, and most vineyards are non-irrigated. Soil water reserves accumulated during the dormant winter period are the primary buffer against summer water stress. Cunha and Richter (2016) demonstrated that soil water in the previous two to three years exerts a positive carryover effect on current-year production, reflecting the slow recharge of deep schist soil profiles. Douro production has increased substantially over the study period, roughly doubling from ~770 mhl in the 1930s to ~1,400 mhl by the 2010s, partly reflecting the expansion of the Douro DOC appellation for table wines from 1979 onward and the administrative Port production quota system (IVDP benefício).'),
  SP(),

  H3('2.1.2 Vinho Verde Region'),
  SP(),
  B('The Vinho Verde Region is located in the Minho province of north-western Portugal, the most Atlantic European wine region (Köppen Csb). Mean annual precipitation ranges from 1,200 to 2,000 mm and growing-season temperatures average approximately 17.1°C. In contrast to the Douro, Vinho Verde production has declined dramatically — from approximately 2,300 mhl in the 1960s to around 800 mhl today (−65%). This structural collapse is primarily attributed to EU accession in 1986 (grubbing-up subsidies, quality standards penalising high-yield bulk production), rural exodus, and the conversion from high-yield pergola to lower-yield espalier training systems. Because rainfall is generally sufficient to refill soil water reserves year-round in RVV, carryover effects of water deficits are less prominent than in the Douro (Cunha and Richter, 2020).'),
  SP(),

  H2('2.2 Grapevine Yield Formation'),
  SP(),
  B('Grapevine yield is the product of five sequential components: vines per hectare, shoots per vine, inflorescences per shoot, berry number per bunch, and berry mass (Zhu et al., 2020). Each component is sensitive to different environmental stimuli at different phenological stages, creating a cascade of climate dependencies that spans two growing seasons. Inflorescence primordia — the embryonic structures that will become grape clusters — are initiated during the spring and summer of the year prior to the harvest year. Whether primordia develop into productive inflorescences or revert to tendrils is governed primarily by temperature and light at the time of differentiation (Zhu et al., 2020): high temperatures encourage inflorescence development, while cool or shaded conditions lead to tendril formation, reducing the following season\'s potential bunch number. This mechanism means that current-year production is physiologically dependent on prior-year climate — motivating the inclusion of lagged features in the agroclimatic models used in this thesis.'),
  SP(),
  B('The flowering stage, occurring in late spring of the harvest year, determines the number of berries through fruit-set efficiency. Both low and high temperatures disrupt pollination; wet conditions promote Botrytis cinerea and downy mildew, reducing fruit set. The veraison-to-harvest period is governed by temperature and soil water: in the Douro, post-veraison soil water stress typically promotes berry concentration; in Vinho Verde, the wetter Atlantic climate means that canopy management and moisture balance are the dominant ripening-phase constraints.'),
  SP(),

  H2('2.3 Meteorological Drivers of Wine Production Variability'),
  SP(),
  B('For the Douro, Cunha and Richter (2016) demonstrated that spring temperature explains approximately 60–65% of the long-term and short-term behaviour of production, with soil water as the second principal driver. Both current-year and lagged soil water values (up to three years prior) show significant positive correlations with production. Cunha and Richter (2020) further decomposed cyclical production properties using a time-frequency approach, identifying dominant production cycles of approximately 5.7 and 2.5 years linked to spring temperature and soil water oscillations. Climate change projections are of particular concern: Portugal is expected to experience the greatest growing-season temperature increase among major European wine regions (+1.3°C by 2049), with soil water becoming the primary limiting resource in the Douro. For Vinho Verde, the Atlantic climate provides more consistent growing-season moisture; the dominant vulnerability is cool, wet flowering conditions that promote disease and reduce fruit set. Zhu et al. (2020) showed that daily maximum temperature plays a critical role in inflorescence initiation, while both maximum and minimum temperature influence berry number and mass.'),
  SP(),

  H2('2.4 Quantitative Approaches to Wine Yield Prediction'),
  SP(),

  H3('2.4.1 Statistical and Regression Models'),
  SP(),
  B('The earliest quantitative yield prediction models relied on linear and generalised linear regression applied to seasonal climate aggregates. Santos et al. (2011) developed a multivariate linear regression model for Douro yield (1986–2008) using monthly mean temperatures and precipitation from March to June. Bock et al. (2013) analysed correlations between long-term yield records in Lower Franconia (1805–2010) and mean growing-season temperature. Cunha and Richter (2012) applied an autoregressive model and short-time Fourier transform to identify cyclical properties of production in Douro and Vinho Verde. These models are interpretable and grounded in agronomy but assume linear, stationary relationships and cannot capture threshold effects, interactions, or non-stationarity from structural production changes. A methodological review by Fraga et al. (2024) argued that statistical models for climate impacts in grape and wine research remain underused relative to their potential and should be combined with other approaches.'),
  SP(),

  H3('2.4.2 Machine Learning Approaches'),
  SP(),
  B('The past decade has seen rapid growth in the application of machine learning to grape yield and wine production prediction. Random Forest, Support Vector Machines, gradient boosting ensembles, and deep learning architectures have all been applied to predict yield from meteorological, phenological, and remote sensing data. Lopes et al. (2024) used Random Forest models to identify climatic drivers of wine production in Portugal\'s Côa region, demonstrating competitive predictive performance with feature importance rankings that align with agronomic expectations. Remote sensing approaches use satellite-derived indices (NDVI, EVI) as proxies for vine vigour at the parcel scale, enabling near-real-time sub-regional yield maps.'),
  SP(),
  B('These approaches offer strong predictive performance and the ability to capture complex non-linear interactions. However, they typically function as black-box or grey-box models: their internal decision logic is not directly interpretable by domain experts, and post-hoc explanation tools such as SHAP values provide approximations rather than explicit auditable rules. In scientific domains where the goal is not merely to predict but to explain — to provide actionable insights to viticulturalists and climate planners — this opacity is a significant limitation. As Molnar (2019) argues, the trade-off between predictive accuracy and interpretability must be considered in the context of the intended application. There remains a gap in interpretable rule-based discovery approaches that transform long meteorological time series into compact, statistically validated, human-readable patterns explaining annual regional wine production.'),
  SP(),

  H2('2.5 Subgroup Discovery'),
  SP(),
  B('Subgroup discovery (SGD) is a descriptive induction technique that identifies subsets of a dataset — defined by conjunctions of attribute-value conditions — within which the distribution of a target variable is statistically exceptional relative to the overall population (Herrera et al., 2011). Unlike classification, which aims to correctly assign every observation to a class, SGD aims to find the most interesting local patterns: subgroups that are both statistically significant and sufficiently large to be meaningful. The field was formalised by Wrobel (1997) and Klösgen (1996). Lavrac et al. (2004) extended CN2 to subgroup discovery (CN2-SD); Kavšek and Lavrac (2006) adapted the Apriori algorithm to SGD (APRIORI-SD).'),
  SP(),
  B('A significant recent development is Exceptional Model Mining (EMM), introduced by Duivesteijn et al. (2016) as a generalisation where the interestingness criterion is based on a model fitted to the subgroup rather than a simple distributional shift test. EMM identifies subgroups where the relationship between variables differs significantly from the global relationship. Lemmerich et al. (2022) introduced robust subgroup discovery with statistical reliability guarantees. Zimmermann and Atzmueller (2019) adapted SGD to temporal and sequential data, directly addressing the time-series structure of problems such as the one studied in this thesis.'),
  SP(),

  H2('2.6 Distribution Rules and the CarenR Framework'),
  SP(),

  H3('2.6.1 Distribution Rules'),
  SP(),
  B('Distribution rules (DR) were introduced by Jorge et al. (2006) for discovering and presenting conditional distributions of a numeric target variable with respect to a set of input variables. A distribution rule takes the form IF antecedent THEN distribution, where the antecedent is a conjunction of attribute intervals and the consequent is the empirical distribution of the target among matching observations. Unlike conventional association rules, distribution rules preserve the full distributional shape of the outcome. Statistical significance is assessed using the Kolmogorov–Smirnov (KS) test, which compares the empirical cumulative distribution of the target in the subgroup against the global distribution without assuming normality — well suited to small datasets where normality cannot be assumed.'),
  SP(),

  H3('2.6.2 The CarenR Framework'),
  SP(),
  B('CarenR (Contrast Association Rules for Extraordinary Nodes, implemented in R) is the primary algorithmic contribution of the line of work this thesis extends. It operationalises the distribution rule framework of Jorge et al. (2006) with several additions specifically designed for agroclimatic time series problems.'),
  SP(),
  B('The core algorithmic pipeline has five steps. First, continuous input features are discretised into equal-frequency bins (quartiles by default) within each training fold, ensuring uniform bin occupancy and preventing leakage of test-set information. Second, a configurable feature selection step reduces the input space to the most informative features before rule search — this is the dimension where the 10 CarenR variants evaluated in this thesis differ from one another. Third, CarenR performs a beam search over conjunctions of feature-bin conditions, evaluating each candidate subgroup using a two-sample KS test against the global training distribution. Fourth, rules are filtered by significance (p ≤ 0.10) and minimum support (≥ 15% of training observations) to retain only patterns that are both statistically sound and broadly applicable. Fifth, at prediction time, matching rules\' subgroup means are aggregated (support-weighted in some variants); if no rule fires, the model falls back to the training median.'),
  SP(),
  B('The key conceptual distinction between CarenR and RIPPER or M5Rules is that CarenR asks a distributional question ("does the shape of the production distribution shift significantly in this subgroup?") rather than a classification question ("which class does this year belong to?") or a regression question ("what is the expected production value?"). This distinction has a concrete practical consequence: a feature that reliably shifts the distribution mean — even if the distribution remains wide and overlapping with the global distribution — will be detected by CarenR but not by classification or squared-error criteria. This explains why LAI at harvest, the dominant feature in Vinho Verde CarenR rules, is not selected by RIPPER or M5Rules despite its genuine agronomic significance (Section 4.6.3).'),
  SP(),
  B('CarenR has been applied in agricultural settings in previous work by the research group at the University of Porto (Moreira et al.), primarily in the context of viticulture rule discovery. The present thesis represents its first systematic evaluation in a walk-forward predictive validation framework across two contrasting wine regions, with explicit statistical testing and era-composition analysis.'),
  SP(),

  H2('2.7 Positioning and Research Gap'),
  SP(),
  B('Table 2.1 situates this thesis within the broader landscape of literature reviewed in this chapter.'),
  SP(),

  mkTable(
    ['Research stream','Primary goal','Interpretable?','Continuous target?','Long time series?','Representative references'],
    [
      ['Statistical regression models','Prediction','Yes','Yes','Yes','Cunha & Richter 2016/2020; Santos et al. 2011'],
      ['Modern ML (RF, LSTM, SVM)','Prediction','Limited','Yes','Partial','Lopes et al. 2024; Santos et al.'],
      ['Explainable AI / SHAP','Post-hoc explanation','Partial','Yes','No','Molnar 2019'],
      ['Subgroup discovery (classical)','Pattern discovery','Yes','Partial','No','Lavrac et al. 2004; Herrera et al. 2011'],
      ['Exceptional model mining','Pattern discovery','Yes','Yes','Partial','Duivesteijn et al. 2016; Zimmermann 2019'],
      ['Distribution rules (CarenR)','Discovery + prediction','Yes','Yes','Yes','Jorge et al. 2006 — THIS THESIS'],
    ],
    [2200,1500,1100,1200,1100,2900]
  ),
  CAP('Table 2.1. Literature map positioning this thesis relative to existing research streams.'),
  SP(),

  B('The research gap is clear: no existing stream combines all five properties simultaneously. This thesis applies CarenR — the closest existing approach — to two Portuguese wine regions spanning 80–89 years, contributing (1) the first systematic walk-forward evaluation of distributional subgroup discovery for wine production forecasting with phenology-informed feature engineering, (2) a two-decision decomposition framework that precisely diagnoses when and why rule-based prediction succeeds or fails, and (3) a cross-regional comparison that reveals how structural production history interacts with climate signal availability.'),
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
  fs.writeFileSync('/sessions/charming-modest-galileo/mnt/outputs/chapter2_literature_v2.docx', buf);
  console.log('Done: chapter2_literature_v2.docx');
});

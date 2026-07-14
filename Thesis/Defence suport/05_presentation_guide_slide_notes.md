# Thesis Defense Presentation Guide
## Wine Production Forecasting Using Rule-Based Machine Learning
### Hugo Filipe Queiróz Nogueira — MECD, FEUP — June 2026

---

# PART 1 — SLIDE-BY-SLIDE PRESENTATION OUTLINE

---

## Slide 1 — Title & Opening Hook
**Purpose:** Set the scene and claim the jury's attention in the first 30 seconds.

**Recommended Content:**
- Thesis title (shortened): *"Can Rules Explain and Forecast Wine Production?"*
- Your name, programme, supervisors, date
- One compelling opening image: a Douro vineyard, or a production time-series chart showing the dramatic Vinho Verde decline alongside the Douro rise

**Visual to include:**
> Figure 3.1 + 3.2 side-by-side: the two production time series. They immediately tell the story of structural divergence — one region going up 80%, the other down 65% — without a single word of explanation.

**Speaker Notes:**
> Introduce the image before the title. "The two lines you see here represent 80 years of wine production in two neighbouring Portuguese regions. One has been rising for a century. The other collapsed by 65% in three decades. Same country, same climate zone — but structurally very different stories. My thesis asks: can interpretable machine learning rules explain *and* predict what drives production above or below trend in each region?"

---

## Slide 2 — The Research Problem
**Purpose:** State the scientific gap and why it matters. One minute to justify the entire thesis.

**Recommended Content:**
- Wine production is climate-sensitive AND structurally driven
- Existing ML approaches (Random Forest, LSTM) predict well but are black boxes
- Domain experts — agronomists, cooperatives, policy bodies — need *readable* rules
- No prior work applies interpretable distributional rule discovery to multi-decade wine production records
- Research gap in one sentence: **"The combination of long time series + interpretable rule output + rigorous predictive validation has not been attempted before."**

**Visual to include:**
> Table 2.1 (Literature map): a simple table showing that no prior work scores "yes" on all five dimensions (interpretable + continuous target + long-term data + two contrasting regions + rigorous validation).

**Speaker Notes:**
> "Machine learning has been applied extensively to wine yield. Random forests, gradient boosting, deep learning — they all predict reasonably well. But they do not explain. An agronomist cannot act on a SHAP value. What practitioners need is a statement like: 'If soil water before harvest falls between 74 and 101 mm, and post-flowering wet days are fewer than 3, production tends to run 120 thousand hectolitres above trend.' That is an actionable, auditable finding. No published work has applied this kind of interpretable distributional rule discovery to multi-decade wine production data. That is the gap this thesis fills."

---

## Slide 3 — Two Goals, Four Research Questions
**Purpose:** Orient the jury on the dual structure of the thesis and set up the story arc.

**Recommended Content:**
- **Goal 1 (Rule Discovery):** Discover interpretable agroclimatic rules explaining above/below-trend production years — *achieved*
- **Goal 2 (Predictive Validation):** Evaluate whether those rules beat a naive baseline in rigorous walk-forward testing — *not achieved in aggregate*
- Four research questions:
  - RQ1: Which climate variables distinguish above/below-trend years?
  - RQ2: Are those variables genuine within-era signals or era-membership artefacts?
  - RQ3: Do three different algorithms converge on the same features?
  - RQ4: Do discovered rules improve out-of-sample forecasting?
- **Foreshadow the twist:** "The most scientifically interesting finding was not that prediction failed — it was *why* it failed."

**Visual to include:**
> A clean two-column layout: Goal 1 (✓ Achieved) vs Goal 2 (✗ Not achieved in aggregate / ◑ Conditional RDD finding). Simple, memorable, honest.

**Speaker Notes:**
> "The thesis pursues two complementary goals applied to the same data. Goal 1 asks whether CarenR — a distributional subgroup discovery algorithm — can find statistically validated, human-readable climate rules. Goal 2 asks whether those rules are useful in a real forecasting scenario. The answer to Goal 1 is yes. The answer to Goal 2 is largely no. But that asymmetry — rule discovery succeeding where prediction fails — is not a failure of the methodology. It turns out to be one of the most informative findings of the entire study."

---

## Slide 4 — Data, Features & Experimental Design
**Purpose:** Give the jury confidence in the rigour of the setup. This is the credibility slide.

**Recommended Content:**
- Two datasets: RDD (89 years, 1934–2022), RVV (80 years, 1942–2021)
- 59 agroclimatic features per region, anchored on vine phenology (temperature, rainfall, soil water, LAI, heat/frost days — both current and previous year)
- Three algorithms compared: CarenR (distributional), RIPPER (classification), M5Rules (piecewise regression)
- Walk-forward expanding-window validation: 18 scenarios (RDD) / 16 scenarios (RVV)
- All preprocessing within each fold — no data leakage
- **Primary baseline: Naive_Median** (predicts training median residual; minimises MAE among all constant predictors)

**Visual to include:**
> Figure 3.4: the walk-forward expanding-window diagram. This single figure explains the entire validation design. Show it. Jury members will scrutinise the evaluation method most — this diagram pre-empts most methodology questions.

**Speaker Notes:**
> "The data spans 80 to 89 years of daily meteorological records. These were aggregated into 59 annual features anchored on vine phenological dates — so a feature like 'wet days post-flowering' adjusts to the actual flowering date of each year, not a fixed calendar window. Three rule-learning algorithms were compared, each with a fundamentally different optimisation criterion. The evaluation protocol is an expanding-window walk-forward design — the model is always trained on years up to time t and evaluated on year t+1 onwards. All preprocessing, including detrending and feature discretisation, is performed inside each training fold to prevent leakage. The primary benchmark is the Naive_Median — which always predicts the training median residual. Any model that beats this has genuinely extracted climate information."

---

## Slide 5 — The Two-Decision Framework (The Conceptual Contribution)
**Purpose:** Introduce the most original intellectual contribution of the thesis. This is the slide the jury will remember.

**Recommended Content:**
- Prediction error = **Decision 1** + **Decision 2**
- **Decision 1:** How accurately does the training trend extrapolate? Measured by Naive_Median MAE. This is the "floor" below which no model can improve without better trend modelling.
- **Decision 2:** Can a rule-based model beat the training median? This requires structured, non-trivial residuals to exploit.
- **The fundamental tension:** A better trend fit (Decision 1) produces smaller residuals, but smaller residuals contain less signal for rule induction (Decision 2). Improving one degrades the other.
- **Concrete example:** LOESS detrending reduces RVV Naive_Median from 140 to 90 mhl (−36%, better Decision 1), but eliminates all CarenR rules (0 rules fire — zero Decision 2). The more flexible the detrend, the less signal remains.

**Visual to include:**
> A custom diagram (draw it): two boxes ("Decision 1: Trend Quality" and "Decision 2: Climate Signal"), connected by a tension arrow, with Naive_Median MAE labelling Decision 1 and "Model MAE − Naive_Median MAE" labelling Decision 2. Add the LOESS example as a callout.

**Speaker Notes:**
> "This framework is, I believe, the most transferable contribution of the thesis. Any agroclimatic forecasting problem with a non-stationary structural trend faces the same dilemma. Decision 1 is about trend quality — how well your detrending model extrapolates to the test period. Decision 2 is about signal extraction — how much structured inter-annual variation remains after detrending, and whether a model can exploit it. These two demands pull in opposite directions. Make the detrend more flexible and you reduce Decision 1 error, but you strip out the very residual signal that rule induction needs. This tension is not a problem that can be solved by trying harder; it requires a different approach — specifically, structural detrending using non-climate covariates to separate industry changes from climate effects."

---

## Slide 6 — Rule Discovery Results: What CarenR Found (Goal 1)
**Purpose:** Show the scientific payload. These are the actual results that are interpretable, agronomically meaningful, and cross-validated.

**Recommended Content:**
- **Douro (RDD):** 48 rules; dominant themes: harvest soil water (54% of rules), wet days post-flowering (44%), heat stress days. Top rule: dry post-flowering + dry post-maturity → **+121 mhl above trend** (support 28%, p=0.004). Rule 4: multi-year rule → **+192 mhl** (largest Douro shift).
- **Vinho Verde (RVV):** 20 rules; 16 of 20 point BELOW trend (inverse asymmetry). Dominant features: heat stress days at maturity (50% of rules). Only 1 above-trend rule → **+311 mhl** (largest absolute shift in either region).
- **Directional inversion:** The same feature (soil water at harvest) predicts ABOVE-trend in Douro and BELOW-trend in Vinho Verde — CarenR detects region-specific distributional structure, not a global spurious correlation.
- **LOESS validation:** Top RDD features retain 90–125% of correlation after aggressive era-trend removal → genuine within-era signal, not era-membership artefact. (Exception: LAI at harvest in RVV — only 15% retention → era-driven.)

**Visual to include:**
> Tables 4.1 and 4.2 side by side (top 5 rules per region), reduced to the four most important columns (Conditions, Direction, Median Δ, Support). These are the most visually impactful tables in the thesis. Highlight Rule 4 (RDD, +192 mhl) and Rule 4 (RVV, +311 mhl).

**Speaker Notes:**
> "CarenR discovered 48 rules for Douro and 20 for Vinho Verde, all passing a KS significance threshold and a 15% minimum support filter. The Douro rules converge on a clear agroclimatic story: dry post-flowering conditions — fewer than 3.3 wet days — combined with moderate soil water deficit at harvest. This combination favours concentrated berries, reduced disease pressure, and above-trend production. The most powerful rule involves a multi-year mechanism: moderate pre-harvest soil water combined with dry post-harvest conditions in the previous year, capturing the vine's carryover reserve mechanism. It produces a +192 thousand hectoliter shift above trend. In Vinho Verde, the asymmetry reverses: 16 of 20 rules point below trend. The Atlantic climate means that cool, moist conditions — which in Douro would signal drought risk — in Vinho Verde are simply normal, and below-trend years are characterised by the *absence* of warm conditions. Goal 1 is clearly achieved."

---

## Slide 7 — Cross-Algorithm Validation (The Robustness Evidence)
**Purpose:** Pre-empt the "maybe it's just an artefact of CarenR" objection.

**Recommended Content:**
- RIPPER (classification criterion) and M5Rules (MSE criterion) independently select the **same variable families** as CarenR
- Three features confirmed by all three algorithms: harvest soil water, LAI family, previous-year summer temperature
- Cross-algorithm agreement means the signal is real — three fundamentally different optimisation criteria point to the same variables
- One divergence worth mentioning: M5Rules assigns a *negative* slope to harvest soil water overall, while CarenR identifies a *positive* above-trend zone at moderate deficit — because M5Rules cannot model the non-linear interaction without explicit interaction terms
- RIPPER CV accuracy: 42% (RDD), 28% (RVV) — poor classification, but variable selection is meaningful

**Visual to include:**
> Table 4.9 (Feature agreement across algorithms): a compact three-column table showing which features CarenR, RIPPER, and M5Rules agree on, with confidence ratings. The "3/3 HIGH universal" features are the headline.

**Speaker Notes:**
> "A sceptical jury member might ask: could these rules simply be artefacts of CarenR's particular search algorithm? The cross-algorithm comparison provides the strongest evidence against this concern. RIPPER, which asks 'which climate conditions best separate the three production tertiles', and M5Rules, which asks 'which feature enables the most accurate linear regression', both independently identify harvest soil water, LAI, and previous-year summer temperature. Three different questions, three different algorithms, same answer. That convergence is the evidence that these features carry genuine data structure, not algorithmic noise."

---

## Slide 8 — Predictive Validation: The Full Model Ranking (Goal 2)
**Purpose:** Present the honest, complete result. Transparency here builds credibility.

**Recommended Content:**
- **RDD result:** Naive_Median ranks 1st at 184 mhl. Best CarenR (Eta2) ranks 2nd at 189 mhl — a gap of 5 mhl (3%). Wilcoxon p=0.468: not significant. RIPPER (279 mhl) and Decision Tree (250 mhl) are worst.
- **RVV result:** Naive_Median ranks 1st at 140 mhl. Best CarenR (Sup_FS) ranks 4th at 172 mhl — a 22% gap. Confirmatory finding: when CarenR *fires* rules in RVV, those rules are significantly *worse* than the naive baseline (p=0.004). CarenR wins in 0 of 16 RVV scenarios.
- **The key comparison:** CarenR vs Naive_Median, not CarenR vs RIPPER. The relevant question is not "which rule learner is best" but "does any rule learner beat doing nothing with climate data?"

**Visual to include:**
> Figures 5.1 and 5.2 (MAE bar charts for RDD and RVV). These are the clearest visual summaries of Goal 2. Show them side-by-side. The bar chart immediately communicates that Naive_Median is on top and all rule learners are to its right (worse).

**Speaker Notes:**
> "No model consistently beats the Naive_Median baseline in aggregate in either region. In Douro, the best CarenR variant falls 5 thousand hectolitres behind — a 3% gap that is not statistically significant. In Vinho Verde, the gap grows to 22%, and a confirmatory statistical test shows that when CarenR fires rules at all, those rules actively harm prediction rather than helping. This is a negative predictive result, and I want to be clear that the thesis reports it honestly and without softening. Goal 2 is not achieved. The important question is not that it failed, but why."

---

## Slide 9 — Why Prediction Failed: Era Composition and the Structural Explanation
**Purpose:** Turn the negative result into the most intellectually interesting finding.

**Recommended Content:**
- **Era confound:** Training data spans pre- and post-1990 eras with structurally different production levels. Rules discovered on mixed-era training data learn era membership, not climate signal.
- **RDD era-balanced finding (exploratory):** In scenarios 10–14, once post-1990 share reaches 29% of training data, CarenR beats Naive_Median in *all 5 consecutive scenarios* (margins 17–32 mhl). Binomial p=0.031. This is a post-hoc exploratory finding — noted clearly, not over-interpreted.
- **RVV has no recoverable era window:** The structural break was so large and rapid (−65% driven by EU accession, land abandonment, training system change) that no stable climate-production relationship survives across eras. Adding more modern-era data makes RVV *worse*, not better.
- **Structural detrending is the path forward:** If planted area, Port quota, and number of certified producers were used as explicit detrending covariates, the climate residuals would be cleaner. This is the primary recommendation for future work.

**Visual to include:**
> Figure 5.3 (Per-scenario RDD: CarenR Eta2 vs Naive_Median) annotated with "Early: era-confounded (CarenR loses)" and "Balanced era: CarenR wins 5/5". This figure tells the whole era-composition story in one image.
> Table 6.1 (RDD vs RVV structural divergence table): shows the systematic contrast in era confound severity.

**Speaker Notes:**
> "The per-scenario analysis reveals structure that the aggregate weighted MAE conceals. In early scenarios, the training set is heavily dominated by pre-1990 data. CarenR learns rules that partly reflect era membership — the older era had lower production for structural, non-climate reasons. These rules misfire on modern test years. But once the training set reaches a rough 29% post-1990 share, CarenR wins in all five consecutive scenarios, by margins of 17 to 32 thousand hectolitres. I want to be explicit: this is a post-hoc exploratory finding. I identified these scenarios after observing the pattern, not before. It generates a testable hypothesis, not a confirmed result. For Vinho Verde, no such recovery appears. The structural break was too large and too rapid for a linear detrend to separate climate signal from structural change. This is why the two-decision framework is the most important contribution: it gives a precise diagnostic for *why* prediction fails and *what* would need to change to fix it."

---

## Slide 10 — The Douro vs Vinho Verde Contrast
**Purpose:** Frame the comparative experiment as a designed sensitivity analysis, not an accident.

**Recommended Content:**
- The two regions were chosen as a *natural comparative experiment*: same pipeline, same algorithms, different structural histories
- **RDD:** Gradual +80% increase; linear detrend achieves stationarity; era confound moderate; CarenR wins in 5/18 scenarios; top feature (harvest soil water) retains 125% correlation under LOESS
- **RVV:** Structural break (−65%, non-linear); linear detrend leaves residual non-stationarity; era confound severe; CarenR wins 0/16 scenarios; top features (LAI, soil water) collapse to 15–16% under LOESS — **era-driven artefacts**
- Directional inversion of the same feature across regions is a method validation point: CarenR detects region-specific structure, not global spurious correlations
- **Lesson:** The same algorithm, same feature engineering, same validation protocol produces qualitatively different outcomes depending on structural history. This comparison defines the conditions under which distributional rule discovery works.

**Visual to include:**
> Table 6.1 (Structural explanation of RDD vs RVV divergence). Three rows: production trend, era confound, CarenR performance. Clear and concise.

**Speaker Notes:**
> "The contrast between Douro and Vinho Verde is not a methodological weakness — it is an experimental design feature. By running identical pipelines on two structurally different regions, the thesis identifies what determines whether rule-based prediction succeeds. The answer is not the algorithm. The answer is the structural history of the production series. Douro's gradual, manageable upward trend allows a linear detrend to isolate genuine climate residuals. Vinho Verde's catastrophic structural break — driven by EU agricultural policy, not climate — contaminates the climate signal so severely that no rule-based approach can overcome it without structural covariates. This finding would not be visible from a single-region study."

---

## Slide 11 — Scientific Contributions
**Purpose:** State the contributions clearly and defensibly. Jury members will challenge each one.

**Recommended Content:**
Five contributions, ordered by originality:

1. **Two-Decision Decomposition Framework** — separates prediction error into trend quality and climate signal extraction; diagnostic tool transferable to any agroclimatic forecasting problem with structural non-stationarity.

2. **First systematic walk-forward evaluation of distributional subgroup discovery for wine forecasting** — 17 models, 18/16 scenarios, all preprocessing within fold, explicit statistical tests distinguishing confirmatory from exploratory findings.

3. **Systematic 11-variant CarenR sensitivity analysis** — demonstrates that optimal feature selection is problem-specific (η² in RDD, Sup_FS in RVV); motivates structured variant evaluation as standard practice.

4. **Era-composition analysis** — characterises not just *whether* rule-based prediction works, but *under what data conditions* it does (29% modern-era threshold in RDD; exploratory).

5. **Cross-algorithm validation** — RIPPER and M5Rules independently confirm CarenR's feature selections, providing evidence of genuine data structure.

**Visual to include:**
> A clean numbered list. No figure. The contributions speak for themselves.

**Speaker Notes:**
> "The thesis makes five contributions. The most fundamental is the two-decision decomposition: this framework explains precisely why an algorithm can discover valid patterns from historical data while failing to improve out-of-sample prediction. It is the analytical tool that makes the negative result in Goal 2 interpretable rather than merely disappointing. The second contribution is the validation framework itself — 17 models, rigorous walk-forward protocol, explicit classification of confirmatory versus exploratory findings. I want to be transparent about this: I report p-values with their status. The era-balanced result in Douro is exploratory, identified post-hoc, and cannot be treated as confirmed without prospective replication."

---

## Slide 12 — Limitations, Future Work & Closing
**Purpose:** Show intellectual maturity. Acknowledge limits honestly; point forward constructively.

**Recommended Content:**

**Limitations:**
- Structural detrending is the core limitation: time is the only detrending covariate; structural production changes (planted area, Port quota) are conflated with climate effects
- Small number of test scenarios (18/16) limits statistical power — the post-hoc RDD finding (n=5) is underpowered for confirmatory conclusions
- Single-region annual aggregates — no sub-regional or spatial analysis possible

**Future work (three specific, actionable proposals):**
1. **Structural detrending:** Use planted area + Port quota + number of certified producers as explicit detrending covariates. Data available from IVV/IVDP. This would directly resolve the Decision 1/Decision 2 tension.
2. **Prospective validation:** Pre-register the era-balanced hypothesis (29% threshold in RDD) and test it as additional modern-era years accumulate.
3. **Exceptional Model Mining:** Apply the EMM framework to detect subgroups where the *climate-production regression relationship* is exceptional — capturing interaction effects that the KS marginal-distribution test misses.

**Closing line:**
> "Rule-based machine learning cannot yet forecast Portuguese wine production reliably. But it can explain it — with validated, auditable, agronomically meaningful rules that three independent algorithms agree on. Understanding *why* forecasting fails has proven to be as valuable as forecasting itself."

**Visual to include:**
> The two production time series again (Figures 3.1+3.2), paired with one of the top CarenR rules displayed as a natural-language sentence. Full circle from the opening.

---

# PART 2 — DETAILED SPEAKER NOTES (Per Slide, Verbatim-Ready)

The speaker notes above in Part 1 are verbatim-ready. Each slide's notes are designed to:
- Sound conversational (not read from a slide)
- Take 50–70 seconds
- Lead naturally into the next slide

**Transition lines:**
- Slide 1→2: "Let me start with the problem this thesis was designed to solve."
- Slide 2→3: "That gap in the literature shaped two concrete goals."
- Slide 3→4: "To pursue those goals, I needed data and a rigorous evaluation framework."
- Slide 4→5: "Before showing results, I want to introduce the analytical framework that makes those results interpretable."
- Slide 5→6: "With that framework in place, let me show you what the rules look like."
- Slide 6→7: "These rules look compelling. But how do we know they're not artefacts of one algorithm?"
- Slide 7→8: "Cross-algorithm validation confirms the signal exists. Now the hard question: can the rules predict?"
- Slide 8→9: "Prediction failed in aggregate. The question is why — and the answer is more informative than a positive result would have been."
- Slide 9→10: "That era confound explains both regions, but in very different ways."
- Slide 10→11: "This comparison sharpens the contributions of the work."
- Slide 11→12: "Those contributions come with limitations I want to address directly."

---

# PART 3 — VISUAL DESIGN RECOMMENDATIONS

## Overall aesthetic
- Use a clean, minimal template with a white or very light grey background
- One accent colour: a deep red-purple (wine reference, but subtle) for section headers and key callouts
- Avoid bullet-heavy slides — use visual space intentionally
- Font: a serif for slide titles (Georgia or Garamond — academic register), sans-serif for body text (Calibri or Helvetica)

## Slide-specific design notes

**Slides 1, 12 (opening and closing):** Use Figures 3.1 + 3.2 as a full-width background or large central graphic. Let the two production curves do the visual storytelling.

**Slide 2 (Research Problem):** Show Table 2.1 (literature map) as a clean 5-column table. Use colour to highlight the empty cells (where no prior work scores "yes") — this makes the research gap visually obvious.

**Slide 4 (Walk-forward design):** Figure 3.4 should be large — at least 60% of the slide area. This is the most important methodology figure. Label the "train" and "test" segments clearly. Add a callout: "All preprocessing inside this window."

**Slide 5 (Two-Decision Framework):** Create a custom two-box diagram. Don't use any thesis figure here — draw it fresh, simplified. The tension arrow between Decision 1 and Decision 2 is the conceptual payload. Add a small callout with the LOESS numbers: "LOESS: Decision 1 −36%, Decision 2 → 0 rules."

**Slide 6 (Rule Discovery):** Show Tables 4.1 and 4.2 side-by-side, stripped to 4 columns (Conditions, Direction, Δ mhl, Support). Highlight Rule 4 (RDD +192 mhl) and Rule 4 (RVV +311 mhl) with a coloured background. These are the "hero numbers."

**Slide 8 (MAE Rankings):** Figures 5.1 and 5.2 side-by-side. Add a vertical red dashed line at the Naive_Median bar in each chart to visually emphasise the baseline. Label the CarenR bars explicitly.

**Slide 9 (Era Composition):** Figure 5.3 annotated with two bracketed zones: "Era-confounded (1–9)" and "Era-balanced (10–14, all CarenR wins)". The visual annotation makes the pattern undeniable.

**Slide 11 (Contributions):** A numbered list with no figure. Each contribution in one sentence. Consider a two-column layout with contribution on the left and one-line evidence on the right.

## What NOT to do
- Do not put the full CarenR algorithm steps on a slide
- Do not show the full 11-variant sensitivity analysis table on a slide — reference it briefly
- Do not show mathematical equations unless the jury specifically asks
- Do not put the full 48-rule RDD table on a slide — the top 5 is enough

---

# PART 4 — LIKELY JURY QUESTIONS AND STRONG ANSWERS

### Q1: "Why use CarenR specifically? Why not a simpler rule learner?"

**Strong answer:** "CarenR is motivated by two properties that simpler rule learners lack. First, it operates on the continuous target distribution using the Kolmogorov-Smirnov test — it doesn't require discretising the response variable, which avoids information loss. Second, it explicitly identifies distributional shifts, not just mean differences. A feature that shifts the distribution of production residuals without cleanly separating three tertile classes will be detected by CarenR but missed by RIPPER. This is precisely the situation for LAI at harvest in Vinho Verde — all three CarenR variants identify it as a dominant signal, while RIPPER and M5Rules do not. The cross-algorithm comparison validates that this is a genuine signal, not a CarenR artefact."

---

### Q2: "You report a p=0.031 finding for the era-balanced RDD window. Isn't that cherry-picking?"

**Strong answer:** "That is a fair and important challenge. I want to be completely explicit: yes, this finding is post-hoc and exploratory. I identified the scenarios 10–14 window after observing the pattern in the data, not before the analysis. The thesis labels it as 'post-hoc exploratory' in every table and paragraph where it appears, and explicitly states that it generates a testable hypothesis rather than confirming one. A binomial test on n=5 scenarios, identified post-hoc, does not meet the standard for a confirmatory scientific finding. What it does do is motivate a pre-registered prospective test: as additional modern-era years accumulate in the coming seasons, the 29% threshold hypothesis can be tested on new, unseen data. I treat it exactly as what it is — not a result, but a research direction."

---

### Q3: "Why not use a proper spatial or sub-regional dataset instead of regional aggregates?"

**Strong answer:** "Regional aggregate data was the data available for 80–89 years. Sub-regional or parcel-level data with consistent phenological records over this time span does not exist for these regions. The thesis is explicit about this as a scope limitation in Section 7.3. The theoretical advantage of regional aggregates is that they capture the production signal that cooperatives, IVDP, and policy bodies actually act on — the total annual output, not individual parcels. That said, the limitation is real: spatial heterogeneity within each region is not captured, and a future dataset with gridded meteorological data would sharpen the distributional signal considerably. This is listed as a concrete future work direction."

---

### Q4: "The Naive_Median always wins. Isn't that evidence that climate is irrelevant to production?"

**Strong answer:** "No — and this distinction is central to the thesis. The Naive_Median winning means the climate signal is real but insufficient in magnitude to overcome year-to-year production noise in a held-out forecasting test. The LOESS validation in Goal 1 shows that top features retain 90–125% of their correlation with production residuals after aggressive era-trend removal — the signal is genuine. The |r| ≈ 0.47 for top features means climate explains roughly 22% of residual variance. The remaining 78% is noise from factors outside the feature set — vine diseases, logistics, measurement aggregation. In a series with coefficient of variation ~47%, that noise floor is too high for rule-based prediction to consistently beat the median. This is not climate irrelevance; it is a property of the system being studied at regional annual aggregation."

---

### Q5: "Why 11 CarenR variants? That seems like data dredging."

**Strong answer:** "The 11 variants were designed as a structured sensitivity analysis, not a post-hoc search for the best result. They vary along three orthogonal dimensions: feature selection criterion (η², Pearson |r|, hard threshold), whether a support filter is applied, and how rule predictions are aggregated. The decision to evaluate all variants simultaneously — rather than selecting one a priori — was made precisely to avoid this objection. The key finding from the sensitivity analysis is itself a contribution: the optimal configuration is problem-specific. η² works best in Douro because it detects non-linear distributional associations. Sup_FS works best in Vinho Verde because its conservative fallback mechanism avoids firing spurious era-driven rules. A researcher who commits to one variant a priori loses this insight. The thesis recommends structured variant evaluation as standard practice for this reason."

---

### Q6: "What is the practical value of the rules if they cannot improve forecasting?"

**Strong answer:** "The rules have three practical uses that do not require beating a baseline forecast. First, they provide validated, human-readable agroclimatic descriptions that agronomists can use for vintage assessment and risk communication — 'this year's conditions match the dry post-flowering pattern associated with above-trend production in 28% of historical years.' Second, they identify which climate variables are genuinely informative, which is valuable for irrigation management, canopy control decisions, and insurance pricing. Third, they quantify the distributional shift magnitude — '+192 thousand hectolitres above trend' is an estimate, not just a directional signal. The distinction between 'climate affects production' and 'climate signal is strong enough to beat the baseline in forecasting' is real, but both halves are scientifically meaningful."

---

### Q7: "Did you consider ensemble methods or stacking?"

**Strong answer:** "Yes — CarenR_Dist_Stack is one of the 11 variants. It uses CarenR rule predictions as inputs to a second-stage linear model. It ranked 14th of 17 in RDD and 8th of 17 in RVV — worse than simple support-weighted CarenR. The likely reason is that stacking requires enough variance in the first-stage predictions to fit a meaningful second-stage model; with only 18/16 walk-forward scenarios, the stacking layer overfits the training residuals rather than learning a stable combination. The CarenR_Dist_Conf variant — which weights rules by their KS statistical significance — ranked 6th in RDD and 3rd in RVV. Both variants confirm that prediction aggregation method matters less than feature selection."

---

# PART 5 — WEAK POINTS AND HOW TO DEFEND THEM

### Weakness 1: The core finding is negative (Goal 2 not achieved)
**How to defend:** "A confirmed negative result in a rigorous validation framework is a scientific finding, not a failure. The literature is full of studies that claim positive forecasting results from inadequate evaluation designs — random cross-validation on time series, no data leakage control, no multiple-testing correction. This thesis uses the methodologically correct evaluation, reports the negative result honestly, and explains *why* the result is negative in mechanistic terms. The two-decision framework that explains the failure is a contribution that emerges *from* the negative result. A positive result would not have prompted that analytical decomposition."

### Weakness 2: Post-hoc exploratory finding (p=0.031, n=5)
**How to defend:** Already covered in Q2. Emphasise the labelling, the low sample size, and the prospective validation proposal. Do not try to defend this as a confirmatory finding — that would make it worse.

### Weakness 3: Only two regions; cannot generalise broadly
**How to defend:** "The two-region comparative design was deliberate. Two regions with contrasting structural histories allow the thesis to distinguish methodology-specific findings from domain-specific ones. A single-region study cannot do this. The conditions under which the framework succeeds (gradual trend, moderate era confound) and fails (structural break, severe era confound) are identified precisely because both cases are present. Generalisation to other regions requires applying the same structured evaluation, not assuming the same result."

### Weakness 4: 59 features may introduce multiple testing issues
**How to defend:** "CarenR applies a KS significance threshold and a 15% support filter inside its rule induction. The support filter is the practical guard against spurious rules covering only 2–3 observations. The LOESS validation provides an independent check that top features retain genuine within-era signal. The cross-algorithm convergence — three algorithms with different search spaces identifying the same features — is the strongest guard against false discoveries. A feature that appears in 54% of rules AND is selected by RIPPER AND appears in M5Rules coefficients is not a multiple testing artefact."

### Weakness 5: CarenR-Supervised was worst — doesn't that discredit the framework?
**How to defend:** "CarenR_Supervised uses per-fold supervised discretisation — bin boundaries are optimised on the same fold used to assess rule quality. This is a form of target leakage in the feature engineering step. Its consistent failure is a *methodological finding*, not a framework discredit: it demonstrates precisely where leakage enters and confirms that the unsupervised equal-frequency binning used by all other variants is the correct approach. The result is reproducible, consistent across both regions, and provides a clear practical recommendation: do not use supervised discretisation in agroclimatic rule induction."

### Weakness 6: RVV results are strongly negative — does this mean CarenR is only applicable to one type of region?
**How to defend:** "The RVV result is not that CarenR is inapplicable. It is that CarenR cannot overcome structural non-stationarity that a simple linear detrend cannot separate from the climate signal. The LOESS analysis shows that with proper era-trend removal, the RVV Naive_Median drops 36% — there *is* structural variation to explain. The problem is that removing that structural variation also removes the climate residuals. Structural detrending using non-climate covariates — planted area, number of producers, training system ratios — would give CarenR cleaner residuals to work with. The RVV result defines the problem and motivates the solution."

---

# PART 6 — THE 15-MINUTE SPEAKING SCRIPT

*This script is designed to be rehearsed and adapted. Approximate timing is shown in brackets. Speak naturally; do not read verbatim, but use this as your preparation base.*

---

**[SLIDE 1 — 0:00–1:00]**

"Before I introduce my title, I want you to look at these two lines. [point to Figure 3.1+3.2] They represent 80 years of annual wine production in two neighbouring regions of northern Portugal — the Douro and Vinho Verde. One has roughly doubled over the study period. The other has declined by 65% in just four decades. Same country, same general climate zone, but structurally very different trajectories. My thesis asks a deceptively simple question: can interpretable machine learning rules both *explain* and *predict* what drives production above or below trend in each region? The answer, it turns out, is more nuanced — and more interesting — than a simple yes or no."

---

**[SLIDE 2 — 1:00–2:00]**

"Wine yield is notoriously difficult to model. It's sensitive to temperature, rainfall, soil water, canopy development, and multi-year carryover effects from vine physiology. Modern machine learning handles all of this very well — random forests, gradient boosting, deep learning can all predict reasonably accurately. The problem is interpretability. An agronomist cannot act on a SHAP value. What practitioners need is something they can read, challenge, and apply — a statement like: 'If soil water before harvest falls below a certain threshold, and post-flowering wet days are fewer than three, production tends to run 120 thousand hectolitres above trend in one year out of four.' No published work has applied this kind of interpretable distributional rule discovery to multi-decade regional wine production records. That is the gap this thesis fills."

---

**[SLIDE 3 — 2:00–3:00]**

"The thesis pursues two complementary goals. Goal 1 is rule discovery: can CarenR — a distributional subgroup discovery algorithm — extract statistically validated, human-readable agroclimatic rules from 80–89 years of data? Goal 2 is predictive validation: do those rules improve forecasts in a rigorous walk-forward evaluation? I can tell you now that Goal 1 succeeds and Goal 2 does not succeed in aggregate. But I want to be clear: the asymmetry between these two outcomes is not a failure. It turns out to be the most scientifically informative finding of the entire study — and it generates a framework that explains when and why rule-based prediction can succeed."

---

**[SLIDE 4 — 3:00–4:00]**

"Let me briefly describe the experimental setup, because the methodological rigour is what makes the results credible. Two datasets: the Douro, 89 years from 1934 to 2022; Vinho Verde, 80 years from 1942 to 2021. From daily meteorological records I engineered 59 agroclimatic features per region — temperature, rainfall, soil water, leaf area index, heat and frost day counts — all anchored on vine phenological dates so that a feature like 'wet days post-flowering' adapts to the actual flowering date of each year. Three rule-learning algorithms were compared: CarenR for distributional rules, RIPPER for classification rules, and M5Rules for piecewise regression. [point to Figure 3.4] The validation design is an expanding-window walk-forward protocol — the model is always trained on the past and evaluated on what came next. Crucially, all preprocessing — detrending, discretisation — is done inside each training fold to prevent leakage. The primary benchmark is the Naive_Median: the model that always predicts the training median residual. Any rule learner that beats this has genuinely extracted climate information."

---

**[SLIDE 5 — 4:00–5:15]**

"Before showing results, I want to introduce the analytical framework that makes them interpretable. I call it the two-decision decomposition. Any agroclimatic forecasting pipeline involves two sequential problems. Decision 1: how well does your training-period trend extrapolate to test years? This is measured by the Naive_Median MAE — your error floor. A better trend fit lowers this floor. Decision 2: can a rule-based model predict the residuals better than zero? This requires structured residuals to exploit. Here is the fundamental tension: a more flexible detrend reduces Decision 1 error, but simultaneously strips out the residual signal that rule induction depends on. I can show you this concretely. In Vinho Verde, applying LOESS flexible detrending reduces the Naive_Median MAE from 140 to 90 thousand hectolitres — a 36% improvement in Decision 1. But it simultaneously eliminates all CarenR rules — zero rules fire in all 16 validation scenarios. Improving the trend kills the signal. This tension cannot be resolved with the current setup. It requires structural detrending — using non-climate covariates to separate industry changes from climate effects. I'll come back to this."

---

**[SLIDE 6 — 5:15–6:30]**

"Now, the rules themselves. [point to Tables 4.1+4.2] CarenR discovered 48 rules for Douro and 20 for Vinho Verde, all passing statistical significance and a 15% support filter. The Douro rules converge on a clear agroclimatic story. The dominant condition — appearing in 54% of all 48 rules — is moderate soil water deficit at harvest. Combine that with dry post-flowering conditions and bounded heat stress, and you get a production profile that is consistently above trend. The strongest rule is a multi-year mechanism: moderate pre-harvest soil water combined with dry post-harvest conditions in the *previous* year, capturing the vine's carbohydrate reserve mechanism. That rule is associated with a +192 thousand hectoliter shift above trend — in one year out of five in the historical record. In Vinho Verde, the story is structurally inverted: 16 of 20 rules point *below* trend. The Atlantic climate means that the conditions associated with below-trend years are cool and moist — the absence of the warm, dry conditions that benefit the Douro. The only above-trend rule in Vinho Verde is the most powerful in either region: a combination of warm previous-year budburst and mild current-year flowering that produces +311 thousand hectolitres above trend. These are agronomically meaningful, directionally consistent, and validated: top Douro features retain 90–125% of their correlation with production after aggressive era-trend removal. Goal 1 is achieved."

---

**[SLIDE 7 — 6:30–7:15]**

"A key concern with any algorithmic output is whether the patterns are genuine or artefacts. The cross-algorithm validation addresses this directly. [point to Table 4.9] RIPPER, using information gain to separate three production tertiles, and M5Rules, using squared-error minimisation for regression, both independently identify harvest soil water, the LAI feature family, and previous-year summer temperature as the most informative variables. Three different questions, three different algorithms, same answer. That convergence — under entirely different optimisation criteria — is the strongest evidence that these features reflect genuine data structure. One important detail: M5Rules assigns a *negative* coefficient to harvest soil water overall, while CarenR identifies an above-trend zone at *moderate* deficit. This apparent contradiction is resolved by the non-linearity: M5Rules cannot model a threshold effect without explicit interaction terms, so the global linear coefficient absorbs confounded co-occurrences. CarenR's conditional structure captures what the linear model cannot."

---

**[SLIDE 8 — 7:15–8:15]**

"Goal 2. [point to Figures 5.1+5.2] No rule-based model consistently beats the Naive_Median in either region. In Douro, the best CarenR variant — CarenR Eta2 — ranks second with 189 thousand hectolitres weighted MAE versus 184 for the baseline. A 3% gap, not statistically significant. In Vinho Verde, the gap grows to 22%, and a confirmatory statistical test shows that when CarenR fires rules at all, those rules are significantly *worse* than the naive baseline — p=0.004. CarenR wins in zero of sixteen Vinho Verde scenarios. RIPPER is the worst performer in both regions, confirming that hard classification boundaries are particularly unsuited to this small, high-noise dataset. I report these results without softening. Goal 2 is not achieved. The question is why."

---

**[SLIDE 9 — 8:15–9:30]**

"The per-scenario analysis reveals the answer. [point to Figure 5.3] In the Douro, when I plot CarenR's MAE against the Naive_Median scenario by scenario, a pattern emerges. In early scenarios, where training data is dominated by pre-1990 observations, CarenR loses to the baseline consistently. But from scenario 10 onwards — once post-1990 data reaches approximately 29% of the training set — CarenR wins in five consecutive scenarios, by margins of 17 to 32 thousand hectolitres. I want to be transparent: this is a post-hoc exploratory finding. I identified the 29% threshold after observing the pattern, not before. The binomial p-value of 0.031 on five scenarios does not constitute a confirmatory result. What it does is generate a testable hypothesis: as additional modern-era years accumulate, this threshold can be validated prospectively. For Vinho Verde, no such recovery appears. Adding more modern-era data makes Vinho Verde *worse*. The structural break was too large and too rapid for linear detrending to disentangle. CarenR's rules in Vinho Verde become increasingly tuned to era-specific patterns that don't generalise. [point to Table 6.1] This table captures the structural explanation: Douro's gradual trend permits modest era confound; Vinho Verde's structural break creates severe confound that eliminates the climate signal advantage."

---

**[SLIDE 10 — 9:30–10:15]**

"The contrast between the two regions is not an inconvenient inconsistency — it is an experimental design feature. By running the identical pipeline on two structurally different regions, the thesis identifies what determines whether distributional rule discovery succeeds. It is not the algorithm. It is the structural history of the production series. When structural change is gradual and manageable, linear detrending isolates genuine climate residuals and CarenR can find them. When structural change is abrupt and policy-driven, no amount of algorithmic sophistication overcomes the contaminated signal. This comparison would be invisible in a single-region study. The directional inversion — where the same feature, harvest soil water, predicts above-trend in Douro and below-trend in Vinho Verde — is a method validation point: CarenR is detecting region-specific distributional structure, not a global spurious correlation."

---

**[SLIDE 11 — 10:15–11:15]**

"Let me summarise the contributions. First: the two-decision decomposition framework, which separates prediction error into trend quality and climate signal extraction and provides a diagnostic for why rule-based forecasting fails even when genuine signal exists. This framework is transferable to any agroclimatic forecasting problem with structural non-stationarity. Second: the first systematic walk-forward evaluation of distributional subgroup discovery for wine forecasting — 17 models, rigorous protocol, explicit classification of confirmatory versus exploratory findings. Third: a structured 11-variant sensitivity analysis showing that optimal feature selection is problem-specific — AggressiveNomination works in regimes with strong signal, conservative fallback-preserving selection works in regimes with weak or contaminated signal. Fourth: era-composition analysis that characterises not just whether rule-based prediction works, but under what data conditions it does. Fifth: cross-algorithm validation showing that three independent algorithms converge on the same feature families."

---

**[SLIDE 12 — 11:15–12:30 + Q&A buffer]**

"Three limitations. First: the detrending step uses time as its sole covariate, conflating climate variation with structural industry changes. This is the core limitation and the primary target for future work. Second: 18 and 16 scenarios are a small number for statistical testing; the post-hoc finding is underpowered. Third: regional annual aggregates preclude spatial analysis.

The path forward has three concrete steps. Incorporate structural covariates — planted area, Port quota, number of certified producers — as explicit detrending variables. Pre-register the era-balanced RDD hypothesis and test it prospectively. Explore Exceptional Model Mining, which identifies subgroups where the climate-production *regression relationship* is exceptional, rather than where the marginal distribution shifts.

I want to close with the central finding in one sentence. [return to opening slide with production time series] Rule-based machine learning cannot yet forecast Portuguese wine production reliably. But it can explain it — with validated, auditable, agronomically meaningful rules that three independent algorithms agree on. And understanding precisely why forecasting fails has turned out to be as scientifically valuable as forecasting itself would have been. Thank you."

---

*[Pause. Make eye contact. Wait for questions.]*

---

# APPENDIX — TIMING BREAKDOWN

| Slide | Content | Target time |
|-------|---------|-------------|
| 1 | Title + hook | 1:00 |
| 2 | Research problem | 1:00 |
| 3 | Goals + RQs | 1:00 |
| 4 | Data + methodology | 1:15 |
| 5 | Two-decision framework | 1:15 |
| 6 | Rule discovery results | 1:15 |
| 7 | Cross-algorithm validation | 0:45 |
| 8 | Predictive validation results | 1:00 |
| 9 | Why prediction failed | 1:15 |
| 10 | RDD vs RVV contrast | 0:45 |
| 11 | Contributions | 1:00 |
| 12 | Limitations + closing | 1:15 |
| **Total** | | **~13:00** |

*Leave 2 minutes of buffer for natural pace variation. The script runs approximately 13 minutes at a steady speaking pace.*

---

*Guide prepared by Claude Sonnet 4.6 — Cowork mode — June 2026*
*Based on full thesis: "Wine Production Forecasting Using Rule-Based Machine Learning", Hugo Nogueira, MECD FEUP*

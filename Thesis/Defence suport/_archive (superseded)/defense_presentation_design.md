# Defense Presentation Design — 20-Minute Keynote

**Thesis:** Wine Production Forecasting Using Rule-Based Machine Learning — A Distributional Subgroup Discovery Approach for Portuguese Wine Regions
**Candidate:** Hugo Filipe Queiróz Nogueira (MECD, FEUP)
**Format:** ~20 min, committee has read the thesis. Goal: conference-keynote feel, one coherent scientific story.

---

## The single idea the whole talk hangs on

> **Distributional rule discovery can extract genuine, cross-validated climate signal from a century of non-stationary wine data — yet that same signal does not reliably predict. The gap between *explaining* and *predicting* is not a failure; it is a measurable, diagnosable property of the system, and characterizing it precisely is the contribution.**

Everything on every slide should serve this arc: **Discovery ≠ Prediction, and I can tell you exactly why.**

Design rules I applied throughout:
- One message per slide, stated as a sentence you could say out loud.
- Never re-walk the thesis chapter order. The story order ≠ the document order.
- Every claim on screen is anchored to a real number from the thesis.
- Slides carry images and ≤ ~25 words; the argument lives in your voice, not the slide.
- Budget: 20 min content + ~5 slack for questions. ~90 sec/slide average.

---

# VERSION 1 — Full slide-by-slide design

---

## Slide 1 — Title / The Question

- **Title:** *Can a rule explain a harvest it cannot predict?*
  (subtitle: full thesis title, name, supervisors, date)
- **Goal:** Open with the intellectual tension, not a table of contents.
- **Why this slide exists:** A keynote earns attention in the first 15 seconds. Frame the whole talk as a puzzle, not a report.
- **Exact message to remember:** This talk is about the gap between explanation and prediction.
- **Recommended visual:** Full-bleed Douro terraced-vineyard photo, title overlaid, dark gradient for legibility.
- **Figure/table reused:** None.
- **New diagram:** None (photo only).
- **Max text:** Title question + thesis title + author line.
- **Speaker notes:** "Every committee member has read the thesis, so I won't re-tell it. Instead I want to defend one uncomfortable, interesting result: I found climate rules that clearly explain 90 years of Portuguese wine production — and those same rules cannot reliably predict next year's harvest. My thesis is about *why*, and why that 'why' is worth knowing."
- **Duration:** 45 sec.
- **Transition:** "To see why the gap matters, start with the harvest itself."

---

## Slide 2 — Why This Problem Is Hard (and Worth It)

- **Title:** *A weak signal inside enormous noise*
- **Goal:** Establish problem importance AND difficulty in one stroke.
- **Why this slide exists:** The committee must accept that this is a hard, real problem before they judge the method. Difficulty is what makes a negative result publishable.
- **Exact message to remember:** Regional wine production has real climate signal (r ≈ 0.47) but ~47% year-to-year variability — hard by nature, not by neglect.
- **Recommended visual:** The two regional production time series side by side, with the structural trends drawn on top (Douro +80%, Vinho Verde −65%).
- **Figure/table reused:** `fig_context_ts_rdd.png` + `fig_context_ts_rvv.png` (paired).
- **New diagram:** Optional single "signal vs noise" bar: 22% explained / 78% unexplained variance.
- **Max text:** "Signal r≈0.47 · Variability CV≈47% · Douro +80% · Vinho Verde −65%".
- **Speaker notes:** "Wine is among Europe's most climate-sensitive crops, and forecasting matters for producers, cooperatives, and regulators. But regional annual production is the sum of thousands of growers, heterogeneous vineyards, and local weather no index captures. The climate signal is real — about r=0.47 — but it explains only ~22% of residual variance. And the two regions moved in opposite directions structurally. Any honest method has to survive that."
- **Duration:** 90 sec.
- **Transition:** "Given that difficulty, the usual ML answer would be a black box. I deliberately refused it."

---

## Slide 3 — Why Rules, Not a Black Box

- **Title:** *The goal is to be questioned, not just to be right*
- **Goal:** Justify the methodological stance (interpretability-first) as a scientific choice, not a limitation.
- **Why this slide exists:** Pre-empts the obvious committee question — "why not Random Forest / LSTM?" Answer it before they ask.
- **Exact message to remember:** In this domain the deliverable is an auditable rule an agronomist can challenge — accuracy without interpretability doesn't serve the user.
- **Recommended visual:** Two-column contrast — "Black box: SHAP approximates" vs "Rule: IF … THEN … (support Y%, p Z)". Show one real rule verbatim.
- **Figure/table reused:** None (pull one CarenR rule from Table 4.1).
- **New diagram:** Simple two-panel contrast card.
- **Max text:** One example rule + "Auditable · Falsifiable · Carries its own support".
- **Speaker notes:** "Deep models forecast well and explain badly; SHAP approximates influence, it doesn't give you a rule you can argue with. Where the question is *which conditions drive production*, opacity is disqualifying. A rule like 'dry post-flowering AND moderate harvest soil water → +121 mhl, support 28%, p=0.004' can be read, doubted, and checked by a viticulturist. That auditability is the whole point — and no prior work applies interpretable rule discovery to long regional wine records."
- **Duration:** 75 sec.
- **Transition:** "And I didn't test this on one dataset — I built a natural experiment."

---

## Slide 4 — The Natural Experiment: Two Regions

- **Title:** *Two regions, opposite histories, one method*
- **Goal:** Frame RDD vs RVV as a controlled contrast that will later explain the results.
- **Why this slide exists:** The two-region divergence is the backbone of the discussion; plant it early so the payoff lands.
- **Exact message to remember:** Douro (89 yrs, gradual +80%) vs Vinho Verde (80 yrs, structural −65%) — same method, contrasting non-stationarity.
- **Recommended visual:** Map of northern Portugal with both regions highlighted + a compact fact table (years, obs, trend, key driver).
- **Figure/table reused:** None; small custom fact strip.
- **New diagram:** Portugal map with RDD/RVV highlighted + 4-row comparison strip.
- **Max text:** "RDD: 1934–2022, +80%, quota-regulated · RVV: 1942–2021, −65%, EU-policy collapse".
- **Speaker notes:** "I chose two regions precisely because their histories differ. The Douro rose ~80% gradually, partly regulated by the Port benefício quota. Vinho Verde fell ~65% after EU accession in 1986 — grubbing-up subsidies, training-system change, rural exodus. Same algorithm, two very different kinds of non-stationarity. That contrast is what lets me separate climate signal from structural artefact later."
- **Duration:** 75 sec.
- **Transition:** "Now, the engine that reads these series."

---

## Slide 5 — The Method in One Idea

- **Title:** *CarenR: find where the distribution shifts*
- **Goal:** Convey the essence of distributional subgroup discovery without algorithm minutiae.
- **Why this slide exists:** The committee knows the mechanics; they want to see you understand *why the KS test is the right lens*.
- **Exact message to remember:** CarenR finds climate conditions under which the whole production *distribution* moves, tested by Kolmogorov–Smirnov — not just a mean shift, not a class boundary.
- **Recommended visual:** Two overlaid density curves (global vs subgroup) with the KS gap shaded — the visual heart of the method.
- **Figure/table reused:** `fig_ch3_carenr_flowchart.png` (optional, condensed).
- **New diagram:** Clean two-density KS-gap illustration (build this — it's the money visual).
- **Max text:** "Subgroup distribution ≠ global distribution → a rule. KS p, support, mean Δ."
- **Speaker notes:** "CarenR asks a distributional question: is there a set of climate conditions under which production residuals follow a *different distribution* than usual? It tests that with a Kolmogorov–Smirnov statistic, keeps rules with p ≤ 0.10 and ≥ 20% support, and reports the mean deviation from trend. This is stronger than 'the average is higher' — it detects shifts in shape and tails that mean-based or classification methods miss. That sensitivity is the reason it's the right primary tool here."
- **Duration:** 90 sec.
- **Transition:** "But discovering a rule is easy. Trusting it — and testing it — is where the rigor is."

---

## Slide 6 — How I Made the Test Honest

- **Title:** *Walk-forward + the two-decision decomposition*
- **Goal:** Show the evaluation is rigorous and, crucially, introduce the diagnostic framework that will explain everything.
- **Why this slide exists:** This is the intellectual spine. The two-decision idea is your single biggest transferable contribution — it must be crisp before results.
- **Exact message to remember:** Every prediction error splits into Decision 1 (trend quality → the Naive_Median floor) and Decision 2 (climate signal on top). They pull against each other.
- **Recommended visual:** Left: expanding-window walk-forward schematic. Right: the two-decision split diagram.
- **Figure/table reused:** `fig_ch3_walkforward.png` and `fig_ch3_two_decision.png`.
- **New diagram:** If combining, a single "error = trend floor + signal" equation graphic.
- **Max text:** "Expanding window, all preprocessing in-fold · Error = Trend (D1) + Climate signal (D2)".
- **Speaker notes:** "Two things make the test fair. First, walk-forward validation — expanding window, 18 scenarios for Douro, 16 for Vinho Verde, every step of preprocessing done *inside* each fold, no leakage. Second, and this is the lens for the whole talk: I decompose error into two decisions. Decision 1 is how well the trend is modelled — that sets the Naive_Median baseline. Decision 2 is whether climate rules beat that baseline. Remember this: a better trend *shrinks the residual that the rules need to survive*. Hold that thought."
- **Duration:** 100 sec.
- **Transition:** "With the test defined, here is what the method discovered."

---

## Slide 7 — Goal 1: The Rules Are Real

- **Title:** *68 rules — and three algorithms agree*
- **Goal:** Deliver the positive result with its strongest evidence: cross-algorithm convergence.
- **Why this slide exists:** This is the "what I discovered" peak. Convergence across three different optimizers is the most persuasive evidence of genuine structure.
- **Exact message to remember:** CarenR found 48 (RDD) + 20 (RVV) validated rules; RIPPER and M5Rules — with totally different criteria — select the same variable families. That agreement means real structure, not method artefact.
- **Recommended visual:** Feature-agreement matrix (3 methods × dominant features, checkmarks).
- **Figure/table reused:** Table 4.9 (cross-algorithm feature agreement) — reduced to the 3 "universal" features.
- **New diagram:** Clean 3-column convergence graphic: harvest soil water, LAI family, prev-year summer temp.
- **Max text:** "Harvest soil water · LAI family · prior-year summer temp — confirmed by KS, info-gain, and MSE."
- **Speaker notes:** "Goal 1 succeeds. CarenR retained 48 rules in the Douro, 20 in Vinho Verde. The dominant Douro condition — soil water 20 days before harvest — appears in 54% of rules. Now the key point: RIPPER optimizes information gain, M5Rules minimizes squared error, CarenR tests distributions. Three unrelated criteria, and they converge on the *same* variable families. When three different lenses see the same structure, it's the data talking, not the algorithm."
- **Duration:** 90 sec.
- **Transition:** "But 'statistically significant' isn't enough — could these just be echoes of the era trend?"

---

## Slide 8 — Goal 1: Signal, Not Era Artefact

- **Title:** *The LOESS stress test*
- **Goal:** Demonstrate scientific self-scepticism — you tried to break your own result.
- **Why this slide exists:** Committees reward candidates who attack their own findings. This slide is where you earn distinction-level credibility.
- **Exact message to remember:** Under aggressive LOESS detrending, genuine features keep their correlation (Douro 103–125%); era-driven features collapse (RVV soil water/LAI to 15–16%). The method distinguishes real signal from era membership — and honestly flags where RVV is contaminated.
- **Recommended visual:** Retention bar chart: features that survive (green, ~100%) vs collapse (red, ~15%).
- **Figure/table reused:** Table 4.4 (LOESS validation) as a bar chart.
- **New diagram:** Retention % bar chart (build from Table 4.4 numbers).
- **Max text:** "Genuine: 94–125% retained · Era artefact: 15–16% retained".
- **Speaker notes:** "I stress-tested every top feature. LOESS detrending removes not just the linear trend but slower era-level structure — if a feature only looked useful because it co-trends with the era, it should collapse. In the Douro, harvest soil water and post-flowering wet days retain 100%+ of their correlation: genuine within-era signal. In Vinho Verde, LAI and soil water collapse to ~15% — that's era contamination, and I report it plainly. Post-flowering wet days survive in both. This is the method proving it can tell signal from artefact — including on itself."
- **Duration:** 95 sec.
- **Transition:** "So the rules are real and validated. The obvious next question — do they predict?"

---

## Slide 9 — Goal 2: Nobody Beats the Median

- **Title:** *The honest result: real rules, no forecast edge*
- **Goal:** Deliver the negative result cleanly and without defensiveness — this is the pivot of the whole talk.
- **Why this slide exists:** The tension from Slide 1 resolves here. Stating it boldly is what makes the talk a keynote, not an apology.
- **Exact message to remember:** In rigorous walk-forward, no rule-based model beats Naive_Median: Douro +3% (p=0.468), Vinho Verde +22%, where rules actively harm (p=0.004).
- **Recommended visual:** Ranking bar chart with Naive_Median highlighted at the top — the reveal is that the "dumb" baseline wins.
- **Figure/table reused:** `fig51_weighted_mae_rdd.png` (and/or `fig52_weighted_mae_rvv.png`).
- **New diagram:** None; annotate the existing bar chart to spotlight Naive_Median rank 1.
- **Max text:** "RDD: 189 vs 184 mhl (p=0.468) · RVV: 172 vs 140 mhl, rules harmful (p=0.004)".
- **Speaker notes:** "Here's the result I won't dress up. Across every scenario, the simplest possible forecast — predict the historical median — wins. In the Douro the best CarenR variant is 3% behind, not significant. In Vinho Verde it's 22% behind, and when its rules fire they make things *worse*, significantly, p=0.004. So: I discovered validated, cross-confirmed climate rules that do not improve prediction. That is the puzzle from my first slide, now with numbers. The thesis is what I did next — I refused to leave it at 'it didn't work.'"
- **Duration:** 90 sec.
- **Transition:** "Because when I looked *inside* the aggregate, the failure had structure."

---

## Slide 10 — The Twist: When It Does Work

- **Title:** *Failure with a pattern: era balance flips the result*
- **Goal:** Show the negative result is not uniform — there's a conditional regime, which is far more scientifically interesting.
- **Why this slide exists:** Turns a flat "it failed" into "it fails *for a reason I can point to*." This is the actionable finding.
- **Exact message to remember:** Once Douro training reaches ~29% modern-era data, CarenR wins 5/5 consecutive scenarios (p=0.031, exploratory). Vinho Verde does the opposite — more modern data makes it worse (0/16).
- **Recommended visual:** Per-scenario gap plot, era-balanced winning window shaded green (RDD) beside RVV's monotonic degradation.
- **Figure/table reused:** `fig53_per_scenario_rdd.png` + `fig54_per_scenario_rvv.png`.
- **New diagram:** None; annotate the win-window on the RDD per-scenario figure.
- **Max text:** "RDD: 5/5 wins when era-balanced (p=0.031, post-hoc) · RVV: worsens as modern data grows".
- **Speaker notes:** "The aggregate hides structure. In the Douro, once the training set is about 29% modern-era, CarenR beats the baseline in five consecutive scenarios — every one. That's p=0.031, but I flag it honestly as *post-hoc and exploratory*; I found the window after seeing the wins, so it's a hypothesis, not a confirmation. And Vinho Verde behaves in the exact opposite way: the more modern data I add, the worse it gets. Same method, mirror-image behaviour. That contrast is the clue to the mechanism."
- **Duration:** 95 sec.
- **Transition:** "One framework explains both the failure and the exception."

---

## Slide 11 — The Explanation: Two Decisions in Tension

- **Title:** *You cannot have a clean trend and a rich residual*
- **Goal:** Deliver the core theoretical contribution — the mechanism behind everything shown.
- **Why this slide exists:** This is the "why the discoveries matter" slide. It converts observations into a transferable principle.
- **Exact message to remember:** Improving the trend (Decision 1) removes the very residual signal rules need (Decision 2). LOESS proves it: RVV baseline improves 36% but *all* rules vanish.
- **Recommended visual:** A see-saw / trade-off diagram — trend flexibility up, residual signal down — with the LOESS numbers as proof.
- **Figure/table reused:** `fig_detrend_rvv.png` (LOESS suppresses all rules) as evidence.
- **New diagram:** See-saw "Decision 1 ↔ Decision 2" with annotated LOESS outcome (RVV 140→90 mhl, rules 20→0).
- **Max text:** "LOESS: RVV baseline −36%, CarenR rules 20 → 0. The signal was the residual."
- **Speaker notes:** "Here's the mechanism. When I made the trend more flexible with LOESS, the Vinho Verde baseline improved 36% — and every single CarenR rule disappeared. The flexible trend absorbed the inter-annual variation *that the rules were built from*. Decision 1 and Decision 2 compete for the same variance, because both are parameterized by a single covariate: time. You can have a clean trend or an exploitable residual, not both. That's why the rules explain history but can't predict — and why era-balanced Douro is the one spot where the tension briefly relaxes."
- **Duration:** 100 sec.
- **Transition:** "This also explains why my two regions ended up so different."

---

## Slide 12 — Why the Regions Diverge

- **Title:** *Structure, not climate, decides who wins*
- **Goal:** Close the natural-experiment loop opened on Slide 4.
- **Why this slide exists:** Demonstrates the finding generalizes and that you understand the domain causally, not just statistically.
- **Exact message to remember:** Same climate signal magnitude in both regions; Vinho Verde's sharp policy-driven structural break (−65%) drowns it, Douro's gradual quota-smoothed rise (+80%) leaves it exploitable.
- **Recommended visual:** Two-panel: gradual Douro trend vs Vinho Verde's structural collapse, annotated with EU-1986, benefício.
- **Figure/table reused:** Table 6.1 (structural divergence) condensed; or the two context TS again with structural annotations.
- **New diagram:** 4-row RDD/RVV structural comparison card.
- **Max text:** "Same signal (r≈0.47). Different noise floor. Structure wins."
- **Speaker notes:** "The 3%-vs-22% gap isn't about climate — the climate signal is the same size in both regions. It's about structural noise. The Douro rose gradually, partly buffered by the Port quota, so a linear trend captures it and leaves clean residuals. Vinho Verde collapsed 65% after 1986 — EU subsidies, training-system change, rural exodus — a non-linear break that no time-only detrend can separate from climate. Same signal, different noise floor. The structural history, not the weather, decides whether rules can predict."
- **Duration:** 90 sec.
- **Transition:** "I want to be equally clear about what this work does not establish."

---

## Slide 13 — Limitations, Owned

- **Title:** *What I would not yet claim*
- **Goal:** Pre-empt the committee's critiques by stating them first, with precision.
- **Why this slide exists:** Distinction-level defenses show the candidate is the harshest critic in the room.
- **Exact message to remember:** Time-only detrending is the root limitation; the winning window is n=5 and underpowered; single-region annual aggregates preclude spatial analysis.
- **Recommended visual:** Three-item honest list, no decoration.
- **Figure/table reused:** None.
- **New diagram:** None.
- **Max text:** "Time-only detrend · n=5 post-hoc window · annual aggregates only".
- **Speaker notes:** "Three limitations I'll state before you do. One: detrending uses time as its only covariate — that's the structural root of the whole Decision 1/Decision 2 tension. Two: the era-balanced win is five scenarios, post-hoc, underpowered — a hypothesis, not a result, and I've labelled it that way throughout. Three: these are single-region annual aggregates, so no sub-regional or spatial signal is available. None of these are fatal, but they bound exactly what I claim."
- **Duration:** 80 sec.
- **Transition:** "And each of them points to a concrete next step."

---

## Slide 14 — What Should Happen Next

- **Title:** *Break the tension: structural detrending*
- **Goal:** Show the path forward follows directly from the diagnosis — the framework is generative.
- **Why this slide exists:** Converts limitations into a research agenda; proves the two-decision framework has predictive value for *future* work.
- **Exact message to remember:** Detrend with structural covariates (planted area, quota, producer count) instead of time — this decouples Decision 1 from Decision 2 and should recover predictive signal.
- **Recommended visual:** The structural-detrend equation with area/quota terms, arrow to "cleaner residual → CarenR."
- **Figure/table reused:** Eq. 6.1 (structural detrend).
- **New diagram:** Before/after residual schematic: time-only (contaminated) → structural (clean).
- **Max text:** "y = α + β₁t + β₂·area + β₃·quota + ε  →  climate lives in ε".
- **Speaker notes:** "The fix follows from the diagnosis. If I detrend with *structural* covariates — planted area, the Port quota, number of certified producers — instead of time alone, the structural trajectory goes into the model and the climate signal stays in the residual. Decision 1 and Decision 2 stop competing. That, plus pre-registering the era-balanced hypothesis and testing higher-resolution climate data, is the agenda. The two-decision framework didn't just explain my failure — it tells the next researcher exactly where to push."
- **Duration:** 90 sec.
- **Transition:** "Let me close by saying what I think this really contributes."

---

## Slide 15 — Take-Home

- **Title:** *A negative result that knows why*
- **Goal:** Land the single idea with force; leave the committee with a transferable principle.
- **Why this slide exists:** The final image the committee carries into deliberation. Must restate the thesis of the talk in one breath.
- **Exact message to remember:** Distributional subgroup discovery extracts genuine, cross-confirmed patterns from a century of non-stationary data; those patterns don't generalize because era structure contaminates the signal; and the two-decision framework precisely locates the fix. The contribution is the *diagnosis*, transferable to any structurally non-stationary time series.
- **Recommended visual:** Three-beat closing: Discover → Diagnose → Direct, under the Slide-1 question now answered.
- **Figure/table reused:** None; callback to the opening vineyard image.
- **New diagram:** Three-word arc graphic (Discover · Diagnose · Direct).
- **Max text:** "Rules explained 90 years. They couldn't predict one. Now we know why — and where to look next."
- **Speaker notes:** "So — can a rule explain a harvest it cannot predict? Yes, and I can tell you why. Distributional subgroup discovery pulled genuine, three-way-confirmed climate patterns out of ninety years of non-stationary data. They don't predict, because the era structure that makes them interpretable also contaminates them — and the two-decision framework says precisely where to intervene. My contribution isn't a forecasting model; it's a diagnosis that transfers to any long, structurally non-stationary series. Thank you — I'd welcome your questions."
- **Duration:** 75 sec.
- **Transition:** (End — move to Q&A. Keep the backup slides below hidden.)

---

## Backup / appendix slides (hidden, for Q&A only)

- **B1 — Full 17-model ranking tables** (Tables 5.1 / 5.2) — for "how did the other models do?"
- **B2 — The directional inversion** (Table 4.3): same soil-water variable, opposite direction across regions — powerful method-sensitivity anecdote if asked "how do you know it's not overfitting?"
- **B3 — Feature-selector rank reversal** (Table 5.10 / `fig` ): η² wins RDD, Sup_FS wins RVV — for "why 11 variants?"
- **B4 — Fallback mechanism** (Table 5.13): CarenR saying "I don't know" vs RIPPER's forced guess — for "isn't a model that abstains useless?"
- **B5 — Supervised discretisation negative result:** target leakage, worst variant — for "did you try learning the bins?"

**V1 slide count:** 15 main + 5 backup. Estimated main-deck runtime ≈ 20.5 min.

---
---

# CRITICAL REVIEW OF VERSION 1

*(Reviewing as I would a candidate I want to send into the room at distinction level.)*

### Unnecessary / at-risk slides
1. **Slide 12 (Why regions diverge)** partially duplicates Slide 4 (natural experiment) and Slide 11 (two-decision). The divergence is *already implied* once Slides 10–11 land. Risk: three slides making one structural point. **Fix:** fold the region divergence into Slide 11 as its concrete illustration, freeing ~90 sec.
2. **Slide 3 (Why rules)** and **Slide 5 (Method)** both spend time motivating interpretability. Some redundancy. **Fix:** tighten Slide 3 to the single verbatim-rule contrast; move the "no prior work" novelty claim up so it isn't buried.
3. **Slide 2's optional signal/noise bar** competes with the time-series panels for attention. One visual per slide — drop the bar or drop the panels.

### Missing slides / gaps
1. **No explicit "contributions" beat.** The take-home implies them, but a committee scoring a thesis wants to *see* the contribution list at least once. The conclusions chapter lists five contributions; the talk names ~two. **Fix:** add a compact contributions slide, or a contributions strip on the take-home.
2. **No agenda/roadmap microslide.** A keynote can skip a TOC, but 15 dense slides with a mid-talk pivot (positive → negative) benefit from a 10-second "here's the arc" signpost so the committee knows the negative result is *coming and intended*, not a stumble.
3. **The KS test — the actual named method in the title — gets one slide and no worked intuition.** For a distributional-SD thesis, that's thin. Consider strengthening Slide 5's visual to *show* a tail shift, not just a mean shift.
4. **No slide answers "so what for the winegrower?"** The talk is method-centric (correct for a keynote) but a wine-domain committee (Prof. Cunha, FCUP) will want one sentence of agronomic payoff. Currently only in speaker notes.

### Weak transitions
- Slide 6→7 ("here is what the method discovered") is generic. The transition should *foreshadow the convergence* punch.
- Slide 13→14 is fine but 12→13 is abrupt (structure → limitations). Needs a bridge acknowledging that the divergence itself exposes a limitation.

### Weak scientific arguments
1. **The p=0.031 finding is fragile and appears on a headline slide (10).** Correctly hedged, but a hostile examiner can spend ten minutes here. **Fix:** keep it, but frame the *pattern* (monotonic era-dependence, mirror-imaged across regions) as the finding, and the p-value as secondary — the pattern is robust even if the single test is underpowered.
2. **Slide 9's "nobody beats the median" can read as total failure** if the pivot to Slide 10 isn't instantaneous. The emotional low point is dangerous if a slide-advance stalls. **Fix:** merge the reveal and the "but it has structure" hook so the recovery is on the *same* breath.
3. **Cross-algorithm convergence (Slide 7)** is strong but under-defended: an examiner may note the three methods share the same feature-engineering pipeline, so convergence could be a pipeline artefact. **Fix:** add one sentence in notes that the discretization/optimization criteria differ even though inputs are shared.

### Simplification opportunities
- 15 main slides for 20 minutes is slightly over-packed (avg 82 sec leaves no recovery buffer). Target **12–13 main slides** so each can breathe and Q&A starts on time.
- Slides 2 and 4 both carry the ±80/−65% trend numbers. State once, reference later.

### Memorability opportunities
- The **Slide-1 question ("Can a rule explain a harvest it cannot predict?")** is strong — but it's only used at the start. **Callback it explicitly at Slide 9 (the reveal) and Slide 15 (the answer)** to create a three-point spine.
- Give the core contribution a **name the committee will repeat**: e.g. *"the two-decision trade-off"* is good — say it as a named law ("the trade-off"), and put the see-saw image on both Slide 6 (teaser) and Slide 11 (payoff) so it becomes a motif.
- The **directional inversion** (same variable, opposite sign across regions) is the single most memorable *scientific* image in the thesis and it's buried in backup. Promote a one-line version into the convergence slide.

---
---

# VERSION 2 — Improved final deck (recommendations applied)

**Changes made:** 15 → 13 main slides; added a 10-sec roadmap and a real contributions beat; merged region-divergence into the two-decision payoff; merged the negative-result reveal with its recovery hook (no dangerous pause); promoted the directional inversion out of backup; reframed the p=0.031 slide around the *pattern* not the p-value; installed the opening question as a 3-point callback spine; one visual per slide; trend numbers stated once. Target runtime ≈ 19 min, leaving buffer.

| # | Title | One-sentence message | Primary visual | ~sec |
|---|-------|----------------------|----------------|------|
| 1 | **Can a rule explain a harvest it cannot predict?** | This talk defends one result: validated climate rules that explain 90 years yet cannot forecast one. | Vineyard photo + title question | 45 |
| 2 | **A weak signal in enormous noise** *(+ 10-sec roadmap line at bottom: Discover → Diagnose → Direct)* | Real climate signal (r≈0.47) but ~47% variability and two opposite structural histories (+80% / −65%) make this genuinely hard. | Paired RDD/RVV time series w/ trends | 90 |
| 3 | **The goal is to be questioned, not just right** | The deliverable is an auditable rule an agronomist can falsify — the first such application to long regional wine records. | One verbatim rule + black-box contrast | 70 |
| 4 | **Two regions, opposite histories, one method** | RDD and RVV form a natural experiment isolating structural non-stationarity from climate. | Portugal map + 4-row fact strip | 70 |
| 5 | **CarenR: find where the distribution shifts** | The method detects conditions under which the whole production distribution (tails included) moves, via a KS test. | Two-density KS tail-shift graphic | 90 |
| 6 | **An honest test, and the lens for everything** | Walk-forward validation + the *two-decision trade-off*: error = trend floor (D1) + climate signal (D2), and they compete. | Walk-forward + see-saw (teaser) | 100 |
| 7 | **The rules are real — three algorithms agree** | 68 validated rules; CarenR, RIPPER, M5Rules converge on the same features via three different criteria — and the *same variable flips sign* between regions, proving genuine structure. | Convergence matrix + inversion call-out | 95 |
| 8 | **I tried to break them: the LOESS stress test** | Genuine features keep 94–125% of their signal after era-removal; era artefacts collapse to ~15% — the method self-audits, RVV contamination reported. | Retention bar chart (green vs red) | 90 |
| 9 | **The honest result — and it has structure** *(reveal + recovery on one slide)* | No model beats Naive_Median (RDD +3% p=.468; RVV +22%, rules harmful p=.004) — **but** the failure is patterned, not random. | Ranking bar (median = rank 1) w/ hook arrow | 90 |
| 10 | **When it works, and when it inverts** | Douro wins 5/5 once ~29% era-balanced; Vinho Verde monotonically *worsens* with modern data — a robust mirror-image pattern (p=.031 secondary/exploratory). | RDD win-window vs RVV degradation | 95 |
| 11 | **One law explains all of it: the two-decision trade-off** *(absorbs region divergence)* | Improving the trend deletes the residual the rules need — LOESS proves it (RVV −36% baseline, rules 20→0); structural break size is why RVV loses and RDD nearly wins. | See-saw payoff + LOESS + RDD/RVV strip | 100 |
| 12 | **What I claim, what I don't, and what's next** *(limitations + future work fused)* | Root limit is time-only detrending (n=5 window underpowered, aggregates only) → fix is structural-covariate detrending that decouples D1 from D2. | Structural-detrend eqn + before/after residual | 100 |
| 13 | **A negative result that knows why** | Discovery ≠ prediction; the two-decision diagnosis locates the fix and transfers to any structurally non-stationary series — that diagnosis is the contribution. | Discover · Diagnose · Direct + Slide-1 callback + 5-contribution strip | 80 |

**Backup (unchanged, Q&A only):** full ranking tables · feature-selector rank reversal · fallback "I don't know" mechanism · supervised-discretisation leakage · per-scenario detail.

### Why V2 is the distinction-level deck
- **One spine, three callbacks.** The opening question returns at the reveal (9) and the close (13); the see-saw motif teases at 6 and pays off at 11. The committee leaves with *one* idea, reinforced three times.
- **No emotional cliff.** The negative result (9) and its recovery live on the same slide — no risky pause where "it failed" hangs alone.
- **The strongest evidence is front-loaded and de-duplicated.** Convergence + directional inversion now share one high-impact slide (7); region divergence is demonstration, not a separate claim (11).
- **Honesty is structural, not defensive.** LOESS self-audit (8), the p-value reframed as a pattern (10), and fused limitations/next-steps (12) show you as the room's toughest critic — the single best predictor of a distinction.
- **Runtime ≈ 19 min**, leaving genuine buffer and an on-time handover to Q&A.

### Delivery notes
- Rehearse Slides 6 and 11 hardest — the trade-off is your thesis's intellectual signature; if the committee remembers one thing, make it "the two-decision trade-off."
- Say the phrase **"the two-decision trade-off"** verbatim at least three times (6, 11, 13) so it becomes the name they use in deliberation.
- On Slide 10, if pressed on p=0.031, retreat to the *pattern* ("monotonic, mirror-imaged across two regions") — that's what's robust; concede the single test is underpowered before they say it.
- Keep backup slides one keystroke away; invite the hard questions — abstention (B4) and target leakage (B5) are your strongest "I already thought about that" moments.

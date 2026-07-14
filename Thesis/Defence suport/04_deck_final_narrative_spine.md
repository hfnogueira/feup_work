# Defense Deck — Final (Your 14-Slide Narrative, Five-Idea Spine)

**Structure:** the candidate's chosen 14-slide story.
**Discipline:** every slide is bolted to exactly one of the five ideas, so the coherent narrative survives.

## The five ideas (the spine — Slide 1 promises them, Slide 14 recalls them)
1. **HARD** — real climate signal, buried in enormous noise (r≈0.47, ~22% variance, CV≈47%).
2. **AUDITABLE** — rules you can question, not a model you must trust.
3. **REAL** — genuine structure: 68 rules, three algorithms agree, they survive stress-testing.
4. **LAWFUL FAILURE** — they don't predict, and one law says why: the two-decision trade-off.
5. **DIAGNOSIS** — the contribution is knowing where to fix it, and it transfers beyond wine.

## One narrative sentence
> Hard problem (1) → auditable rules, not a black box (2) → the rules are real (3) → yet they can't predict, and I can prove why (4) → that "why" is a transferable diagnosis (5).

## Coverage map (proof the narrative stays coherent)
| Slide | Idea | Role |
|-------|------|------|
| 1 Title | (all) | promise the five |
| 2 The problem | **1 HARD** | signal buried in noise |
| 3 Research questions | **1 HARD** → bridge | frame the two goals |
| 4 Why these two regions | **1 HARD** | natural experiment |
| 5 Methodology (pipeline) | **2 AUDITABLE** | one honest pipeline + two-decision teaser |
| 6 Why CarenR | **2 AUDITABLE** | KS method = the auditable stance |
| 7 Goal 1 – Rule discovery | **3 REAL** | 68 rules + 3-algorithm convergence + **LOESS stress test** |
| 8 What did we learn (agronomy) | **3 REAL** | domain insight, inversion |
| 9 Thresholds & biology | **3 REAL / 2 AUDITABLE** | interpretability pays off |
| 10 Goal 2 – Predictive validation | **4 LAWFUL FAILURE** | median wins |
| 11 Discovery ≠ Prediction | **4 LAWFUL FAILURE** | finding **+ two-decision mechanism** |
| 12 Contributions | **5 DIAGNOSIS** | what transfers |
| 13 Limitations & Future Work | **5 DIAGNOSIS** | structural detrending fix |
| 14 Final take-home | (all) | recall the five |

Coverage: HARD ×3, AUDITABLE ×2 (+1 shared), REAL ×3, LAWFUL FAILURE ×2, DIAGNOSIS ×3. Idea 4 is thin in slide *count* but carries the two heaviest slides (10–11). Nothing orphaned.

---

# Slide-by-slide

### Slide 1 — Title / Promise
- **Idea:** all five.
- **Goal:** open with the five claims, not a table of contents.
- **Message:** here is the whole talk in five things to remember.
- **Visual:** vineyard photo, thesis title, and the five one-word labels (HARD · AUDITABLE · REAL · LAWFUL FAILURE · DIAGNOSIS).
- **Figure:** none.
- **Max text:** title + five labels.
- **Notes:** "You've read the thesis, so I won't retell it. I'll defend five claims — remember these five and you have my thesis."
- **~40s** → "The first claim: this problem is harder than it looks."

### Slide 2 — The Problem
- **Idea:** **1 HARD.**
- **Goal:** establish importance and difficulty together.
- **Message:** real climate signal (r≈0.47) but only ~22% of variance, in a series swinging ~47% year to year.
- **Visual:** paired RDD/RVV production series with trends (+80% / −65%).
- **Figure:** `fig_context_ts_rdd.png` + `fig_context_ts_rvv.png`.
- **Max text:** "Signal r≈0.47 · ~22% of variance · noise CV≈47%".
- **Notes:** "Wine is Europe's most climate-sensitive crop, and forecasting matters. But regional production is thousands of growers and local weather no index sees. The signal is real yet explains barely a fifth of the variance. Easy wins don't exist here."
- **~80s** → "So I posed two precise questions."

### Slide 3 — Research Questions
- **Idea:** **1 HARD** (bridge to method).
- **Goal:** frame the two goals cleanly — not a wall of four bullets.
- **Message:** Goal 1 — can we *discover* interpretable climate rules? Goal 2 — do those rules *predict*? (RQ1–4 nested under these two.)
- **Visual:** two big boxes — DISCOVER / PREDICT — with the four RQs as small captions.
- **Figure:** none.
- **Max text:** "Goal 1: DISCOVER interpretable rules · Goal 2: do they PREDICT?"
- **Notes:** "Two goals. Discover rules that explain production; then test — brutally — whether those rules predict. The whole talk is the gap between those two verbs."
- **~55s** → "Both questions I asked in two regions chosen on purpose."

### Slide 4 — Why These Two Regions
- **Idea:** **1 HARD** (controlled contrast).
- **Goal:** frame RDD vs RVV as a natural experiment that will explain the results later.
- **Message:** Douro (89 yrs, gradual +80%, quota-buffered) vs Vinho Verde (80 yrs, structural −65% after EU-1986) — same method, opposite non-stationarity.
- **Visual:** Portugal map, both regions highlighted + 4-row fact strip.
- **Figure:** none (custom strip).
- **Max text:** "RDD +80% gradual · RVV −65% structural break — same method, two histories".
- **Notes:** "I chose two regions with opposite histories. The Douro rose gradually, partly buffered by the Port quota; Vinho Verde collapsed after EU accession. Same algorithm on both — that contrast is what lets me separate climate from structure later."
- **~70s** → "Here's the pipeline that treats both fairly."

### Slide 5 — Methodology (One Pipeline Slide)
- **Idea:** **2 AUDITABLE.**
- **Goal:** show one rigorous, leak-free pipeline — and plant the two-decision idea.
- **Message:** detrend → discover rules in-fold → walk-forward test; error splits into Decision 1 (trend) + Decision 2 (climate signal). *(Teaser only — payoff on Slide 11.)*
- **Visual:** the pipeline diagram; small see-saw inset labelled "D1 ↔ D2 (return to this)".
- **Figure:** `fig_ch3_pipeline.png` + `fig_ch3_walkforward.png`.
- **Max text:** "Expanding window · all preprocessing in-fold · Error = Trend (D1) + Signal (D2)".
- **Notes:** "One pipeline. Expanding-window walk-forward, every preprocessing step inside the fold — no leakage. And remember one idea: error is a trend part and a signal part. I'll come back to why those two fight."
- **~85s** → "The engine inside this pipeline is CarenR — and I chose it deliberately."

### Slide 6 — Why CarenR
- **Idea:** **2 AUDITABLE.**
- **Goal:** justify interpretability-first + the distributional (KS) choice, pre-empting "why not a black box?"
- **Message:** the deliverable is a falsifiable rule an agronomist can attack; CarenR finds where the whole distribution shifts (KS), each rule carrying its own p-value and support.
- **Visual:** one verbatim rule + two-density KS tail-shift graphic; greyed "black-box/SHAP≈approx" panel.
- **Figure:** rule from Table 4.1; new KS graphic.
- **Max text:** "IF dry post-flowering AND moderate harvest soil water → +121 mhl (support 28%, p=.004)".
- **Notes:** "A deep model forecasts and can't explain. Where the question is *which conditions drive production*, that's disqualifying. CarenR asks a distributional question — does a set of conditions shift the whole distribution, tails included — and every rule reports its own evidence. That auditability is the point, first application to long regional wine records."
- **~85s** → "So what did that engine discover?"

### Slide 7 — Goal 1: Rule Discovery
- **Idea:** **3 REAL.**
- **Goal:** deliver the positive result with its two strongest proofs — convergence and the stress test.
- **Message:** 48 + 20 validated rules; RIPPER and M5Rules converge on the same variables via different criteria; and under aggressive era-removal (LOESS) genuine features keep 94–125% of their signal while artefacts collapse to ~15%.
- **Visual:** convergence matrix (left) + LOESS retention bars (right, green survive / red collapse).
- **Figure:** Table 4.9 (reduced) + Table 4.4 as bars.
- **Max text:** "68 rules · 3 algorithms agree · genuine 94–125% retained, era artefact 15%".
- **Notes:** "Goal 1 succeeds. 68 rules. Three unrelated optimizers land on the same features. Then I tried to break them: LOESS removes era structure — genuine features survive intact, era-contaminated ones collapse to 15%, which I report honestly for Vinho Verde. Real structure, not artefact." *(This is where the LOESS argument lives.)*
- **~95s** → "And these rules aren't just statistical — they're agronomically legible."

### Slide 8 — What Did We Actually Learn (Agronomic Insights)
- **Idea:** **3 REAL** (domain validity).
- **Goal:** show the rules mean something to a viticulturist — critical for the FCUP examiner.
- **Message:** the dominant driver is harvest-period soil water; the *same variable* flips sign between regions (+192 mhl Douro, −338 mhl Vinho Verde) — a moderate deficit is a sweet spot in the dry Douro, a cool-wet penalty in Atlantic Vinho Verde.
- **Visual:** the directional-inversion call-out + top-feature themes (soil water, post-flowering moisture, heat moderation).
- **Figure:** Table 4.3 (inversion) + Table 4.11/4.12 feature frequency.
- **Max text:** "Same soil-water variable: +192 mhl in Douro, −338 mhl in Vinho Verde".
- **Notes:** "The rules read like agronomy. Harvest soil water dominates both regions — but in opposite directions, and CarenR detects that without ever seeing the regions together. A mild deficit concentrates berries in the dry Douro; the same range signals cool-wet, low-quality years in Vinho Verde. The method resolves a sign reversal correctly — that's real domain structure."
- **~85s** → "But 'a variable matters' isn't enough — where exactly are the biological thresholds?"

### Slide 9 — Thresholds & Biological Interpretation
- **Idea:** **3 REAL / 2 AUDITABLE** (the interpretability payoff).
- **Goal:** show the concrete, actionable output only a rule-based method gives — the reward for choosing auditability.
- **Message:** the rules give explicit, biologically coherent thresholds (e.g. soil water 74–101 mm → above-trend; heat days >35 °C bounded), each with support — this is what a black box cannot hand a grower.
- **Visual:** annotated threshold bands on a soil-water / heat axis, mapped to vine phenology stages.
- **Figure:** rule bins from Tables 4.1–4.2; vine-cycle schematic if available.
- **Max text:** "Moderate deficit 74–101 mm → +121–192 mhl · heat >35 °C must stay bounded".
- **Notes:** "Interpretability pays off here. The rules don't just name variables — they give thresholds tied to vine biology: a specific soil-water band, bounded heat-stress windows, each with a support estimate. That's an auditable, actionable statement a grower or regulator can use and challenge. No SHAP plot delivers that."
- **~80s** → "Real, legible, thresholded rules. The decisive question: do they predict?"

### Slide 10 — Goal 2: Predictive Validation
- **Idea:** **4 LAWFUL FAILURE.**
- **Goal:** deliver the negative result cleanly, no defensiveness.
- **Message:** in rigorous walk-forward, no rule-based model beats Naive_Median — RDD +3% (p=.468), RVV +22% where rules actively harm (p=.004).
- **Visual:** ranking bar chart, Naive_Median at rank 1; arrow "…but look inside".
- **Figure:** `fig51_weighted_mae_rdd.png`.
- **Max text:** "Naive_Median wins · RDD 189 vs 184 (p=.468) · RVV 172 vs 140, harmful (p=.004)".
- **Notes:** "I won't dress this up. Predict the historical median and you beat every rule model — 3% in the Douro, 22% in Vinho Verde, where firing rules makes it worse. Real rules, no forecast edge. That's the puzzle. I refused to stop at 'it didn't work.'"
- **~85s** → "Because the failure isn't random — it obeys a law."

### Slide 11 — Discovery ≠ Prediction (Central Finding + Mechanism)
- **Idea:** **4 LAWFUL FAILURE** (the heart of the thesis).
- **Goal:** state the central scientific finding AND the mechanism that proves it isn't a dead end.
- **Message:** the failure is lawful — the *two-decision trade-off*: improving the trend (D1) deletes the residual signal the rules need (D2). LOESS proves it — RVV baseline improves 36%, and all 20 rules vanish. Era-balance flips the Douro (5/5 wins) because it briefly relaxes the tension.
- **Visual:** see-saw payoff (D1 ↑ → D2 ↓) with LOESS numbers + the RDD win-window / RVV degradation mirror.
- **Figure:** `fig_detrend_rvv.png` + `fig53/fig54` per-scenario.
- **Max text:** "Better trend deletes the signal. LOESS: RVV baseline −36%, rules 20 → 0."
- **Notes:** "Here's the finding and why it's not a dead end. Make the trend more flexible and Vinho Verde's baseline improves 36% — but every rule disappears, because the flexible trend ate the variation the rules were built from. Decision 1 and Decision 2 fight over the same variance because both are parameterized by time alone. Discovery and prediction ask different things of the same signal. That's the two-decision trade-off — the name I'd like you to remember."
- **~100s** → "Naming that trade-off is itself the contribution."

### Slide 12 — Contributions
- **Idea:** **5 DIAGNOSIS.**
- **Goal:** make the contribution explicit and scoreable — don't leave it implied.
- **Message:** (1) the two-decision diagnostic framework; (2) a structured 11-variant sensitivity analysis (optimal config is problem-specific); (3) the era-composition finding; (4) cross-algorithm validation; (5) method validation on a demanding 90-year non-stationary testbed.
- **Visual:** five compact contribution cards.
- **Figure:** none.
- **Max text:** five 3–4 word contribution labels.
- **Notes:** "Five contributions. The headline is the two-decision framework — a diagnostic that explains *why* rule-based prediction fails even when real signal exists. Then: a systematic variant analysis showing the best configuration is region-specific; the era-composition regime; three-way cross-algorithm validation; and a demanding real-world testbed."
- **~80s** → "Every contribution comes with a boundary I'll state plainly."

### Slide 13 — Limitations & Future Work
- **Idea:** **5 DIAGNOSIS** (the fix follows from the diagnosis).
- **Goal:** own the limits, then show the framework generates the next step.
- **Message:** root limitation is time-only detrending (n=5 era window underpowered; annual aggregates only) → the fix the diagnosis points to is structural detrending: add planted area, quota, producer count so climate stays in the residual.
- **Visual:** structural-detrend equation → before/after residual (contaminated → clean).
- **Figure:** Eq. 6.1.
- **Max text:** "y = α + β₁t + β₂·area + β₃·quota + ε → climate lives in ε".
- **Notes:** "Three limits: detrending on time alone — the structural root of the whole trade-off; the era-balanced win is five scenarios, post-hoc, underpowered — a hypothesis, and I label it so; and single-region annual aggregates. But the diagnosis is generative: put structural drivers into the trend, and Decisions 1 and 2 stop competing. My own framework hands the next researcher the fix."
- **~85s** → "So, five things to carry out of the room."

### Slide 14 — Final Take-Home
- **Idea:** all five (payoff bookend).
- **Goal:** recall the five ideas as answers; leave one transferable principle.
- **Message:** hard problem; auditable rules; the rules are real; they don't predict and I know why; the diagnosis transfers to any structurally non-stationary series.
- **Visual:** the five labels from Slide 1, each checked with a one-line resolution.
- **Figure:** callback to opening vineyard image.
- **Max text:** five labels + 4–5 word answers each.
- **Notes:** "Five things. It's a hard problem. I chose auditable rules. The rules are real, three ways over. They don't predict — and the two-decision trade-off says exactly why. And that diagnosis is the contribution: it transfers to any series where structure and signal are entangled. Thank you — your questions."
- **~70s** → *(End → Q&A.)*

---

## Verdict on your narrative
**It works, and it's better than my keynote for a scoring committee** — it adds research questions, agronomic payoff, and explicit contributions. Adopted as-is with two non-negotiables:
- **Slide 7 must carry the LOESS stress test** — it's your strongest proof the rules are real. Without it, "REAL" rests on convergence alone.
- **Slide 11 must be finding + mechanism** — "Discovery ≠ Prediction" is the headline; the two-decision trade-off is what turns it from an admission of failure into a contribution. Never show 11 without the see-saw.

## Timing
14 slides ≈ 19.5 min at the durations above, leaving buffer for Q&A. If you run long, compress Slides 3 (RQs, 55→40s) and 9 (thresholds, 80→60s) — they're the most cuttable without breaking the spine.

## Two rehearsal locks
1. Say the five one-word labels aloud on Slide 1 and Slide 14 — the verbal bookend is what cements recall.
2. Say **"the two-decision trade-off"** verbatim on Slides 11 and 12 so the committee adopts your phrase in deliberation.

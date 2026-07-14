# Defense Deck — The Five-Idea Spine (Final Coherent Narrative)

**Test this deck must pass:** an examiner who half-listened can still name five ideas on the way out. If the five aren't unmissable, the deck is wrong. So the five ideas are the *architecture*, not a takeaway — every slide is bolted to exactly one, the five are promised up front and recalled at the end, and a rail on every slide shows which one is live.

---

## The one narrative sentence (everything serves this)

> **This is a hard problem (1); I attacked it with auditable rules instead of a black box (2); the rules are genuinely real (3); yet they cannot predict — and I can prove exactly why (4); and that "why" is a transferable diagnosis, not a dead end (5).**

Read it again: it is a single sentence with five clauses. The deck is that sentence, slowed down.

## The five ideas (verbatim — put these on Slide 1 and the last slide)

1. **HARD** — Real climate signal, buried in enormous noise. (r≈0.47, ~22% variance, CV≈47%)
2. **AUDITABLE** — I chose rules you can question, not a model you must trust.
3. **REAL** — The rules are genuine structure: 68 rules, three algorithms agree, they survive stress-testing.
4. **LAWFUL FAILURE** — They don't predict — and one law explains why: the two-decision trade-off.
5. **DIAGNOSIS** — The contribution is knowing *where to fix it* — and it transfers beyond wine.

---

## How "obvious" is engineered (the three mechanisms)

1. **Promise → Payoff bookend.** Slide 1 shows all five as "what you'll leave knowing." The final slide shows the same five, now as answers. The audience is told the five, walked through the five, and reminded of the five.
2. **Idea rail on every slide.** A five-dot strip (●●●●●) in the corner, active idea filled. At any instant the committee sees *which of the five* this slide is about. No slide is unassigned.
3. **One slide = one idea = one sentence.** Every slide title is a claim about its idea. If a slide can't be tagged to a single idea, it's cut.

---

# The deck — 11 slides, each locked to one idea

Legend: **[n]** = which of the five ideas this slide reinforces.

---

### Slide 1 — Promise
- **Idea:** *(meta — introduces all five)*
- **Title:** *Five things I want you to leave knowing*
- **Message:** Here is the whole talk in five claims; I'll defend each.
- **On screen:** The five ideas as five short lines (HARD · AUDITABLE · REAL · LAWFUL FAILURE · DIAGNOSIS) under the thesis title. Idea rail introduced (all five dots outlined).
- **Max text:** the five one-word labels + thesis title/name.
- **Notes:** "You've read the thesis, so I won't retell it. Instead I'll defend five claims. By the end, if you remember nothing else, remember these five." *(Read them.)*
- **~40s** → "Start with the first: this problem is harder than it looks."

---

### Slide 2 — HARD
- **Idea:** **[1]** Real signal, buried in noise.
- **Title:** *A real signal, drowning in noise*
- **Message:** The climate signal is genuine but explains only ~22% of variance in a series that swings ~47% year to year — and the two regions moved in opposite directions.
- **Visual:** Paired RDD/RVV production series with structural trends overlaid (+80% / −65%).
- **Figure reused:** `fig_context_ts_rdd.png` + `fig_context_ts_rvv.png`.
- **Max text:** "Signal r≈0.47 · only ~22% of variance · noise CV≈47%".
- **Notes:** "Regional production is thousands of growers, heterogeneous vineyards, local weather no index sees. The climate signal is real — r≈0.47 — but it explains barely a fifth of the variance. This is why the problem is worth a careful method and why easy wins don't exist here."
- **~85s** → "Facing that noise, the tempting move is a black box. I refused it — on purpose."

---

### Slide 3 — AUDITABLE
- **Idea:** **[2]** Rules you can question, not a model you must trust.
- **Title:** *I chose to be falsifiable*
- **Message:** In this domain the deliverable is a rule an agronomist can read and attack — accuracy without auditability doesn't serve the user; this is the first such application to long regional wine records.
- **Visual:** One real rule verbatim beside a greyed-out "black box + SHAP≈approximation" panel.
- **Table reused:** one CarenR rule from Table 4.1.
- **Max text:** "IF dry post-flowering AND moderate harvest soil water → +121 mhl (support 28%, p=.004)".
- **Notes:** "A deep model forecasts and can't explain; SHAP approximates. Where the question is *which conditions drive production*, that's disqualifying. A rule like this can be read, doubted, checked. I'm not trading accuracy for interpretability naively — I'm saying interpretability *is* the requirement here."
- **~70s** → "So how does the method decide a rule is worth stating?"

---

### Slide 4 — AUDITABLE
- **Idea:** **[2]** The method embodies the auditable stance.
- **Title:** *CarenR: find where the distribution shifts*
- **Message:** The method flags climate conditions under which the whole production distribution — tails included — moves, tested by Kolmogorov–Smirnov; each rule carries its own p-value and support.
- **Visual:** Two overlaid densities (global vs subgroup) with the KS gap shaded — a tail shift, not just a mean shift.
- **Figure reused:** condensed `fig_ch3_carenr_flowchart.png` (optional).
- **New diagram:** the two-density KS tail-shift graphic *(build this)*.
- **Max text:** "Subgroup distribution ≠ global distribution → a rule. Keep if p≤.10, support≥20%."
- **Notes:** "CarenR asks a distributional question — is there a set of conditions where residuals follow a *different* distribution? It tests that with a KS statistic. That's stronger than a mean shift or a class boundary; it catches shifts in shape and tails. And every rule reports its own evidence — support and p — which is exactly the auditability from the previous slide, built into the algorithm."
- **~85s** → "Fine — it produces rules. But are they *real*?"

---

### Slide 5 — REAL
- **Idea:** **[3]** Genuine structure, not method artefact.
- **Title:** *68 rules — and three algorithms independently agree*
- **Message:** CarenR found 48 + 20 validated rules; RIPPER (info-gain) and M5Rules (MSE) converge on the *same* variables — and the same variable even flips sign between regions. Three different lenses, one structure.
- **Visual:** 3-method convergence matrix + a call-out box: "same soil-water variable → +192 mhl in Douro, −338 mhl in Vinho Verde."
- **Table reused:** Table 4.9 (reduced to the three universal features) + Table 4.3 inversion.
- **Max text:** "Harvest soil water · LAI family · prior-year summer temp — confirmed by KS, info-gain, MSE."
- **Notes:** "Three unrelated optimizers — distributional, classification, regression — land on the same feature families. And CarenR detects that the same variable carries *opposite* signals in the two regions, without ever seeing them together. When independent methods see the same thing, and the method resolves a sign reversal correctly, that's the data talking, not the algorithm."
- **~90s** → "Statistically significant isn't enough. Could these just be echoes of the era trend? I tried to break them."

---

### Slide 6 — REAL
- **Idea:** **[3]** They survive my attempt to destroy them.
- **Title:** *The stress test: signal survives, artefact collapses*
- **Message:** After aggressive era-removal (LOESS), genuine features keep 94–125% of their correlation; era-contaminated features collapse to ~15% — and I report the collapse honestly for Vinho Verde.
- **Visual:** Retention bar chart — survivors green (~100%), collapses red (~15%).
- **Table reused:** Table 4.4 as a bar chart.
- **Max text:** "Genuine: 94–125% retained · Era artefact: 15–16% retained".
- **Notes:** "If a feature only looked useful because it co-trends with the era, removing the era should kill it. Douro's harvest soil water and post-flowering wet days retain 100%+ — genuine. Vinho Verde's LAI and soil water collapse to ~15% — era contamination, which I flag rather than hide. The method audits itself, including where it's weak."
- **~85s** → "So the rules are real and validated. The obvious question — do they predict?"

---

### Slide 7 — LAWFUL FAILURE
- **Idea:** **[4]** The honest negative result — with a hook.
- **Title:** *No model beats the median — and the failure has structure*
- **Message:** In rigorous walk-forward, the simplest forecast wins (RDD +3%, p=.468; RVV +22%, rules actively harmful, p=.004) — **but** the failure is not random.
- **Visual:** Ranking bar chart, Naive_Median highlighted at rank 1, arrow pointing to "…but look inside."
- **Figure reused:** `fig51_weighted_mae_rdd.png`.
- **Max text:** "Naive_Median wins. RDD 189 vs 184 (p=.468) · RVV 172 vs 140, harmful (p=.004)."
- **Notes:** "Here's the result I won't dress up: predict the historical median, and you beat every rule-based model. In the Douro by 3%; in Vinho Verde by 22%, where firing rules makes it *worse*. That's the puzzle — real rules, no forecast edge. But I refused to stop at 'it didn't work,' because when I looked inside the aggregate, the failure had a shape."
- **~90s** → "That shape: the failure switches on and off with one variable."

---

### Slide 8 — LAWFUL FAILURE
- **Idea:** **[4]** The failure is conditional and mirror-imaged.
- **Title:** *Failure with a pattern: era balance flips it*
- **Message:** Once Douro training is ~29% modern-era, CarenR wins 5 of 5 consecutive scenarios; Vinho Verde does the mirror-opposite — more modern data makes it steadily worse. Same method, opposite behaviour.
- **Visual:** RDD per-scenario gap with the winning window shaded, beside RVV's monotonic degradation.
- **Figure reused:** `fig53_per_scenario_rdd.png` + `fig54_per_scenario_rvv.png`.
- **Max text:** "RDD: 5/5 wins when era-balanced · RVV: worsens monotonically (pattern robust; p=.031 secondary)."
- **Notes:** "In the Douro, once the training set is era-balanced, CarenR beats the baseline five times in a row. Vinho Verde runs exactly backwards. I lead with the *pattern* — monotonic, mirror-imaged across two regions — because that's what's robust. The single p-value is post-hoc and underpowered, and I'll say so before you do. The pattern is the clue to the mechanism."
- **~90s** → "One law explains both the failure and the exception."

---

### Slide 9 — LAWFUL FAILURE
- **Idea:** **[4]** The mechanism — the two-decision trade-off.
- **Title:** *The two-decision trade-off*
- **Message:** Error = trend quality (Decision 1) + climate signal (Decision 2), and they compete: improving the trend deletes the residual the rules need. LOESS proves it — Vinho Verde baseline improves 36%, and all 20 rules vanish.
- **Visual:** See-saw diagram (trend flexibility ↑ → residual signal ↓) with LOESS numbers as proof.
- **Figure reused:** `fig_detrend_rvv.png`.
- **New diagram:** the see-saw *(same motif teased on Slide 4-area; this is its payoff)*.
- **Max text:** "LOESS: RVV baseline −36%, rules 20 → 0. The signal *was* the residual."
- **Notes:** "This is the intellectual core. Make the trend more flexible and Vinho Verde's baseline improves 36% — but every rule disappears, because the flexible trend ate the variation the rules were built from. Decision 1 and Decision 2 fight over the same variance, because both are parameterized by time alone. That's why rules explain history but can't predict — and why era-balanced Douro is the one place the tension briefly relaxes."
- **~95s** → "If the problem is that time is doing two jobs, the fix is to give it help."

---

### Slide 10 — DIAGNOSIS
- **Idea:** **[5]** The fix follows directly from the diagnosis.
- **Title:** *Break the tension: structural detrending*
- **Message:** Detrend with structural covariates — planted area, Port quota, producer count — instead of time alone; the structural trajectory leaves the residual, so climate signal stays exploitable and Decisions 1 and 2 stop competing.
- **Visual:** The structural-detrend equation → before/after residual schematic (contaminated → clean).
- **Figure reused:** Eq. 6.1.
- **Max text:** "y = α + β₁t + β₂·area + β₃·quota + ε → climate lives in ε".
- **Notes:** "The diagnosis is generative. Put the structural drivers into the trend model, and the climate signal is what's left over. Decision 1 stops stealing from Decision 2. That, plus pre-registering the era-balanced hypothesis on new data, is the concrete agenda my own framework hands to the next researcher."
- **~85s** → "And that's the real contribution — not a wine model, a diagnosis that travels."

---

### Slide 11 — DIAGNOSIS + Payoff bookend
- **Idea:** **[5]** The contribution transfers — and recall all five.
- **Title:** *A negative result that knows why*
- **Message:** The contribution isn't a forecaster; it's a diagnosis — the two-decision framework locates the failure precisely and transfers to any long, structurally non-stationary time series. *(Then re-show the five ideas, now as answers.)*
- **Visual:** The five ideas from Slide 1, each now checked with a one-line answer; idea rail fully filled.
- **Max text:** the five labels, each with a 4–5 word resolution.
- **Notes:** "So — five things. It's a hard problem. I chose auditable rules. The rules are real, three ways over. They don't predict — and one law, the two-decision trade-off, says exactly why. And that diagnosis is the contribution: it transfers to any series where structure and signal are entangled. That's what I'd like you to remember. Thank you — your questions."
- **~70s** → *(End → Q&A)*

---

## Coherence check — every slide maps to exactly one idea

| Slide | Idea | Reinforces |
|-------|------|-----------|
| 1 Promise | (all) | states the five |
| 2 | **1 HARD** | signal buried in noise |
| 3 | **2 AUDITABLE** | rules over black box |
| 4 | **2 AUDITABLE** | KS method = auditable stance |
| 5 | **3 REAL** | 3-algorithm convergence + inversion |
| 6 | **3 REAL** | LOESS stress test survives |
| 7 | **4 LAWFUL FAILURE** | median wins, but structured |
| 8 | **4 LAWFUL FAILURE** | era-balance flips it |
| 9 | **4 LAWFUL FAILURE** | two-decision trade-off = mechanism |
| 10 | **5 DIAGNOSIS** | structural-detrend fix |
| 11 | **5 DIAGNOSIS** | transfers + recall five |

Coverage: HARD ×1, AUDITABLE ×2, REAL ×2, LAWFUL FAILURE ×3, DIAGNOSIS ×2. Idea 4 (the thesis's signature) carries the most weight; nothing is unassigned; no slide serves two ideas. **One narrative, five load-bearing beats, zero orphan slides.**

## The obviousness guarantee
- **Told the five** (Slide 1), **walked the five** (2–10, rail-tagged), **reminded of the five** (Slide 11).
- Every title is a claim about one idea; every idea rail shows the live beat.
- If an examiner is asked "name five things from that defense," the deck's own skeleton answers for them.

## Two rehearsal instructions
1. Say the five one-word labels **out loud** on Slide 1 and again on Slide 11 — the verbal bookend is what cements recall.
2. Say **"the two-decision trade-off"** verbatim on Slides 9 and 11 so the committee uses your phrase in deliberation.

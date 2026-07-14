# Defense Keynote — 20-Minute Narrative Script

**Thesis:** Wine Production Forecasting Using Rule-Based Machine Learning — A Distributional Subgroup Discovery Approach
**Author:** Hugo Filipe Queiróz Nogueira · MECD, FEUP
**Format:** MSc examination committee · keynote-style · ~20 minutes · 14 slides

---

## The spine (say it, then keep returning to it)

> **A pattern you can *discover* is not always a pattern you can *predict*. This thesis shows exactly when the two come apart — and gives a framework that explains why.**

Every slide either sets up this claim, delivers it, defends it, or generalizes it. The word "discover" and the word "predict" should appear in the committee's ears at least five times. Do not organize by chapters; organize by this tension.

---

## SLIDE 1 — The paradox (title + hook)

- **Title:** *"The rule was real. The forecast still failed."*
- **Objective:** Open with a genuine scientific puzzle, not a table of contents. Earn attention in 30 seconds.
- **Key message:** There is a gap between explaining the past and predicting the future — and that gap is the subject of this talk.
- **What to say:** "Imagine you find a rule, validated across three independent algorithms, that reliably describes ninety years of wine production. Every domain expert nods — it's agronomically correct. Then you try to forecast next year with it, and it loses to predicting the median. That is not a bug in my code. That is the finding. Over the next twenty minutes I'll show you when discovery and prediction come apart, and I'll give you a framework that says *why*." (Put your name, region map, one production time-series curve behind you — no bullet points.)
- **Duration:** ~1.0 min
- **Transition:** "To see why this happens, start with what we actually want from machine learning on real-world data."

---

## SLIDE 2 — Two things we ask of ML, and why they conflict

- **Title:** *"Explain vs. Predict — not the same job"*
- **Objective:** Frame the whole talk as a tension between two legitimate goals, so the later null result reads as insight, not failure.
- **Key message:** Interpretable ML is asked to do two things — extract auditable patterns and forecast — and nobody checks whether the second follows from the first.
- **What to say:** "Black-box models forecast well but can't be audited. So in high-stakes domains — agriculture, climate, policy — we reach for rule-based methods that a human can read. But we quietly assume that if a rule *describes* the data well, it will also *forecast* well. My thesis is a stress test of that assumption. Goal 1: can we discover interpretable climate rules? Goal 2: do those rules predict? I'll argue the answer is *yes* and *no* — and that the *no* is the interesting part."
- **Duration:** ~1.5 min
- **Transition:** "To test this honestly, I needed not one dataset but a controlled contrast."

---

## SLIDE 3 — The natural experiment (why two regions, not one)

- **Title:** *"One pipeline, two opposite histories"*
- **Objective:** Establish that this is a controlled comparison, not a case study — your strongest design decision.
- **Key message:** Douro (+80%, gradual) and Vinho Verde (−65%, policy-driven collapse) are the same experiment run under opposite structural non-stationarity.
- **What to say:** "Two Portuguese regions, side by side. Douro production nearly doubled over ninety years — a smooth rise. Vinho Verde collapsed by two-thirds after EU accession in 1986 — grubbing-up subsidies, a training-system switch, rural exodus. Same climate physics, same feature pipeline, same algorithms. The *only* thing that differs is the strength and shape of the structural trend. That makes structural confounding my independent variable — I can watch what it does to both discovery and prediction." (Show the two contrasting production curves together.)
- **Duration:** ~1.5 min
- **Transition:** "Now — the method. In one idea."

---

## SLIDE 4 — The method in one idea

- **Title:** *"Don't ask what value. Ask whether the distribution moved."*
- **Objective:** Convey what makes distributional subgroup discovery (CarenR) different, without algorithm minutiae.
- **Key message:** CarenR asks a distributional question via a Kolmogorov–Smirnov test — which detects signals that classification and regression systematically miss.
- **What to say:** "Most rule learners ask 'what number will this year produce?' CarenR asks something else: 'is there a set of climate conditions under which the whole *distribution* of production shifts, significantly, versus normal years?' It tests that with a Kolmogorov–Smirnov statistic — non-parametric, no normality assumption, ideal for eighty noisy years. The payoff: a feature can reliably move the distribution without cleanly predicting any single year. Hold that thought — it's the seed of the entire discovery-versus-prediction gap. And critically: when no rule confidently fires, the model *abstains* and predicts the median. So it can only lose by acting, never by staying silent."
- **Duration:** ~2.0 min
- **Transition:** "So — Act One. Does discovery work?"

---

## SLIDE 5 — ACT I: Discovery works (and it's real, not an artifact)

- **Title:** *"48 rules, 20 rules, three algorithms agreeing"*
- **Objective:** Deliver the positive result convincingly and pre-empt the "it's just the trend" objection.
- **Key message:** The rules are statistically validated, agronomically coherent, cross-algorithm-confirmed, and — via LOESS — shown to be genuine within-era signal, not era-membership.
- **What to say:** "CarenR finds 48 validated rules in Douro, 20 in Vinho Verde. Harvest soil water appears in over half the Douro rules. Two completely different algorithms — RIPPER on information gain, M5Rules on variance — independently pick the same variable families. That's three optimizers with three different objective functions pointing at the same physics. And to kill the obvious objection — 'aren't these just tracking the long-run trend?' — I re-ran the validation after LOESS detrending, which strips era structure. In Douro the feature correlations survive almost fully. The signal is real. Goal 1: achieved." *(Honesty flag: state plainly that in Vinho Verde the dominant features collapse to ~15% retention under LOESS — foreshadow it here as "and that collapse becomes the key to Act Two.")*
- **Duration:** ~2.0 min
- **Transition:** "A real, expert-verified, triple-confirmed pattern. Surely it forecasts. Let's test it — honestly."

---

## SLIDE 6 — The turn: an honest predictive test

- **Title:** *"The hardest fair test I could design"*
- **Objective:** Establish methodological credibility *before* revealing the null, so the result can't be dismissed as a weak setup.
- **Key message:** Walk-forward, all preprocessing inside each fold, against a baseline that is provably the best any constant predictor can do.
- **What to say:** "No random cross-validation — that leaks the future into the past on a non-stationary series. Instead, expanding-window walk-forward: train on the past, predict the next year, step forward. Every step re-does detrending, discretization, and feature selection inside the fold — zero leakage. And the bar is the Naive-Median: predict 'on trend' every year. That baseline is not lazy — the median provably minimizes mean absolute error among all constant predictors. So *anything* that beats it must be extracting real structure. This is the fairest, most demanding test I could build."
- **Duration:** ~1.5 min
- **Transition:** "Here's what happened."

---

## SLIDE 7 — ACT II: Prediction fails — the pivot

- **Title:** *"It doesn't beat the median. That's the result."*
- **Objective:** Deliver the null result with confidence, as the hinge of the story rather than an apology.
- **Key message:** No rule-based model consistently beats Naive-Median: +3% behind in Douro (n.s.), +22% behind in Vinho Verde — and in Vinho Verde the rules *actively hurt* when they fire (p = 0.004).
- **What to say:** "Douro: best model 189 versus 184 — three percent behind, not statistically distinguishable. Vinho Verde: twenty-two percent behind, and when the rules fire, they're significantly *worse* than doing nothing. So we have a paradox in full view: the exact same rules that Act One proved were real and expert-validated cannot beat a one-line baseline. A weaker student stops here and calls it a failed thesis. I'm going to argue this is the most informative result in the work — because I can tell you precisely *why* it happens." *(This is your central-message reinforcement moment — say the spine sentence out loud.)*
- **Duration:** ~1.5 min
- **Transition:** "The 'why' is a single framework. This is the intellectual core of the thesis."

---

## SLIDE 8 — The diagnosis: the two-decision decomposition

- **Title:** *"Every forecast hides two decisions"*
- **Objective:** Present the thesis's genuine methodological contribution as the explanatory key.
- **Key message:** Forecast error = Decision 1 (does the trend extrapolate?) + Decision 2 (do climate rules beat the leftover residual?). Separating them localizes the failure.
- **What to say:** "Any forecast on a trending series makes two decisions, usually silently. Decision One: how well does the structural trend extrapolate into the future — that sets the baseline error. Decision Two: given that trend, can climate rules predict the leftover residuals better than zero? 'Model loses to baseline' tells you nothing. My decomposition splits the error and asks *which decision failed*. And the answer here is not Decision Two — the rule step is already near the ceiling the signal allows. The bottleneck is Decision One: the trend itself. That reframes the entire problem." (Show a simple two-box diagram: Trend → Residual → Rules.)
- **Duration:** ~2.5 min
- **Transition:** "And these two decisions aren't just separate. They're in direct conflict. I can prove it with one experiment."

---

## SLIDE 9 — The paradox made concrete: the LOESS trade-off

- **Title:** *"Fix the trend, and you murder the signal"*
- **Objective:** Give the committee the single most memorable, counter-intuitive result — the empirical proof of the two-decision tension.
- **Key message:** Flexible detrending (LOESS) cut the Vinho Verde baseline error by 36% — and simultaneously erased *every* rule. Improving Decision 1 destroyed Decision 2.
- **What to say:** "Here's the experiment that convinced me the framework is right. I swapped the linear trend for a flexible LOESS trend in Vinho Verde. Decision One got dramatically better — baseline error dropped thirty-six percent. But the flexible trend absorbed so much year-to-year variation that CarenR found *zero* rules — the KS test had nothing left to detect. I improved trend quality by consuming the exact signal the rules needed. That is the two-decision trade-off, not as theory, but as a measured outcome. As long as *time* is your only detrending covariate, sharpening the trend and preserving the climate signal pull against each other."
- **Duration:** ~2.0 min
- **Transition:** "This same tension explains why my two regions behave so differently."

---

## SLIDE 10 — Why the two regions diverge

- **Title:** *"Structural confounding is the hidden variable"*
- **Objective:** Close the loop on the natural experiment — the controlled contrast pays off.
- **Key message:** Douro's gradual, policy-smoothed trend leaves exploitable residual; Vinho Verde's abrupt policy collapse leaves residuals contaminated by era, so its rules are era-tuned and don't generalize.
- **What to say:** "Remember the two histories. Douro rose gradually — a linear trend captures it, residuals stay clean, and in the right conditions the climate signal survives. Vinho Verde collapsed abruptly around EU accession — no time-based trend separates policy from weather, so the residuals are polluted with structural change. The proof: as I feed *more* modern data into Vinho Verde, the rules get *worse*, because they lock onto era-specific artifacts. Same climate physics in both regions — but structural noise erases the advantage in one. The confound isn't background; it's the governing variable." (Callback to the LOESS 15% collapse from Slide 5 — 'that number was telling us this all along.')
- **Duration:** ~1.5 min
- **Transition:** "But there is one honest glimmer of when prediction *can* work."

---

## SLIDE 11 — The conditional glimmer (with integrity)

- **Title:** *"When the eras balance, the rules win"*
- **Objective:** Show the constructive conditional result — and model scientific honesty about its status.
- **Key message:** In Douro, once training data reaches ~29% modern-era representation, CarenR beats the baseline in 5 of 5 consecutive scenarios — but this is post-hoc and exploratory.
- **What to say:** "In Douro, there's a window — once the modern era makes up about twenty-nine percent of the training data — where CarenR beats the baseline in five consecutive scenarios. That's consistent with the framework: balance the eras and Decision One stops fighting Decision Two. But I want to be completely straight with this committee: I found that window *after* looking at the results. It's five data points. I report it as a hypothesis to be pre-registered and tested prospectively — not as a claim. I'd rather give you an honest signpost than an oversold finding."
- **Duration:** ~1.0 min
- **Transition:** "Step back from wine — because none of this is really about wine."

---

## SLIDE 12 — Beyond wine: why this generalizes

- **Title:** *"This is a lesson about non-stationary time series"*
- **Objective:** Elevate from application to general contribution — the keynote payoff.
- **Key message:** Discoverability ≠ predictability, the trend/signal trade-off, and regime-composition effects on evaluation apply to any structurally non-stationary series.
- **What to say:** "Replace 'wine' with crop yield, disease incidence, energy demand, macroeconomic series — anything with structural regime shifts. Three transferable lessons. One: a validated, interpretable pattern is not evidence it will forecast — always test out-of-sample against an honest baseline. Two: don't reach for a more flexible detrender without asking what signal it eats. Three: your evaluation metric silently interacts with regime composition — my weighted metric loaded the earliest, most-confounded folds, and the verdict could flip as modern data accumulates. These are cautions the whole field needs."
- **Duration:** ~1.0 min
- **Transition:** "So what does the thesis actually contribute, and where does it go next?"

---

## SLIDE 13 — Contributions and the fix

- **Title:** *"A diagnosis — and the prescription"*
- **Objective:** State contributions crisply and point to the concrete path forward.
- **Key message:** The contribution is the *diagnostic apparatus* (two-decision framework, LOESS era-confound test, controlled two-region design); the fix is structural detrending with non-climate covariates.
- **What to say:** "My contribution is not a winning forecaster — it's a diagnosis. The two-decision framework, transferable to any agroclimatic forecasting problem. A LOESS-based test that separates genuine signal from era artifact. A controlled two-region design that isolates structural confounding. And an honest characterization of *when* rule-based prediction works. The prescription follows directly: stop detrending with time alone. Add structural covariates — planted area, production quotas, training-system mix — so the trend models the industry and the residual is pure climate. That decouples the two decisions the current pipeline forces into conflict."
- **Duration:** ~1.0 min
- **Transition:** "Let me leave you with the one sentence I want you to remember."

---

## SLIDE 14 — Closing: return to the spine

- **Title:** *"Discovery is not prediction — and knowing the difference is the science"*
- **Objective:** Land the central message one final time; make the null result feel like a contribution.
- **Key message:** The thesis precisely characterizes the discovery/prediction gap under structural non-stationarity and shows the way to close it.
- **What to say:** "I set out to forecast wine production with interpretable rules. I discovered something more useful than a forecast: I found *where* interpretable discovery and reliable prediction come apart, I built a framework that explains why, and I showed the structural fix. On ninety years of real, messy, non-stationary data, distributional subgroup discovery extracted patterns that are real — and I can tell you, precisely, why they don't yet predict. That precision is the result. Thank you — I'd welcome your questions." *(Have a backup slide ready: the LOESS-retention inconsistency correction, the n=5 power caveat, and the ~3-cycle test-window limitation — the three things they'll probe.)*
- **Duration:** ~0.5 min
- **Transition:** → Q&A

---

## Timing ledger

| # | Slide | Min |
|---|-------|-----|
| 1 | Paradox / title | 1.0 |
| 2 | Explain vs predict | 1.5 |
| 3 | Natural experiment | 1.5 |
| 4 | Method in one idea | 2.0 |
| 5 | Discovery works | 2.0 |
| 6 | Honest predictive test | 1.5 |
| 7 | Prediction fails (pivot) | 1.5 |
| 8 | Two-decision framework | 2.5 |
| 9 | LOESS trade-off | 2.0 |
| 10 | Regional divergence | 1.5 |
| 11 | Conditional glimmer | 1.0 |
| 12 | Beyond wine | 1.0 |
| 13 | Contributions + fix | 1.0 |
| 14 | Closing | 0.5 |
| | **Total** | **~20.5** (aim 19–20 live; slide 8 is your buffer) |

## Delivery notes

- **Structure = three acts:** Discovery works (5) → Prediction fails (7) → Here's why (8–10). The pivot at slide 7 is the emotional and intellectual center — pause there.
- **Reinforce the spine** verbally at slides 1, 7, 12, and 14. Same words each time.
- **Lead with strength, not chapters.** No "Chapter 3 covered methods." The committee reads chapters; a keynote tells a story.
- **Own the null.** Your tone at slide 7 decides the defense. Confident, not apologetic: "That's the result" — then explain it.
- **Pre-load the three attacks** on a backup slide: (1) fix the abstract's "90–92% retention" claim, which contradicts Table 4.4's 15–16% for Vinho Verde — say it correctly on stage before they catch it; (2) n=5 post-hoc power; (3) 17-year window ≈ 3 production cycles.

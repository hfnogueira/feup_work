# Prompt for Gemini — Build the Defense Presentation

Copy everything below the line into Gemini. It is self-contained: Gemini does not have the thesis, so all facts, numbers, and the narrative are embedded here.

---

## ROLE & TASK

You are an expert academic presentation designer. Build a **20-minute MSc thesis defense slide deck** (14 slides) for an examination committee. Do **not** organize it by thesis chapters. Organize it as a **keynote-style scientific story** with a single message reinforced throughout. Output speaker notes for each slide.

## THE ONE MESSAGE (repeat it across the deck)

> **A pattern you can *discover* is not always a pattern you can *predict* — and diagnosing that gap is the real science.**

The words "discover" and "predict" must recur. The deck has a deliberate emotional arc: **promise → betrayal → understanding.**

## CONTEXT (facts you may use)

- **Thesis:** Wine Production Forecasting Using Rule-Based Machine Learning — A Distributional Subgroup Discovery Approach. Author: Hugo Filipe Queiróz Nogueira, MECD, FEUP.
- **Method:** CarenR — distributional subgroup discovery. It finds climate conditions under which the *distribution* of wine production shifts significantly, tested with a **Kolmogorov–Smirnov (KS)** test. When no rule confidently fires, it **abstains and predicts the median** (so it can only lose by acting, never by abstaining).
- **Two regions = a controlled natural experiment:** Douro (RDD), production **+80% gradual rise**, 89 years (1934–2022); Vinho Verde (RVV), production **−65% policy-driven collapse** after EU accession 1986, 80 years (1942–2021). Same pipeline, opposite structural histories.
- **Goal 1 (Discovery) — SUCCEEDS:** 48 validated rules (RDD), 20 (RVV). Harvest soil water in 54% of Douro rules. Two other algorithms (RIPPER, M5Rules) independently converge on the same variable families. A **LOESS detrending test** confirms Douro features are genuine within-era signal (correlations preserved); in RVV the dominant features collapse to ~15% retention — exposing them as era artifacts (this collapse is a *result*, not a caveat).
- **Goal 2 (Prediction) — FAILS:** No model beats the **Naive-Median baseline** (which provably minimizes MAE among constant predictors), under honest expanding-window walk-forward validation with all preprocessing inside each fold. RDD: best model 189 vs 184 mhl (+3%, not significant, Wilcoxon p=0.468). RVV: 172 vs 140 mhl (+22%); when rules fire they are significantly *worse* than doing nothing (p=0.004).
- **The diagnosis — two-decision framework:** every forecast hides Decision 1 (does the trend extrapolate? → sets the baseline error) and Decision 2 (do climate rules beat the leftover residual?). They conflict. Proof: switching to flexible **LOESS detrending in RVV cut baseline error 36% but erased every rule** — improving the trend consumed the signal the rules needed. Because *time* is the only detrending covariate, sharpening the trend and preserving climate signal pull against each other.
- **Why the regions diverge:** Douro's gradual trend leaves clean, exploitable residual; RVV's abrupt policy collapse leaves residual contaminated by era, so its rules are era-tuned and don't generalize. As more modern data enters RVV training, CarenR gets *worse*.
- **Honest conditional glimmer (label as exploratory):** In Douro, once training data reaches ~29% modern-era share, CarenR beats baseline in 5 of 5 consecutive scenarios (binomial p=0.031) — but this is **post-hoc, n=5, must be pre-registered and tested prospectively.**
- **Contribution:** not a winning forecaster but a **diagnosis** — the two-decision framework, the LOESS era-confound test, the controlled two-region design. **The fix:** structural detrending (add planted area, production quotas, producer counts as covariates) to decouple the two decisions.
- **Generalizes beyond wine:** to any structurally non-stationary time series (crop yield, disease incidence, energy demand, macroeconomics).

## SLIDE-BY-SLIDE NARRATIVE (build these 14 slides)

**ACT I — THE PROMISE**
1. **Title / hook — "The rule was real. The forecast still failed."** Open with the paradox, not an agenda. ~1 min.
2. **Explain vs. Predict — two jobs we ask of ML, and why they conflict.** Set up the tension. ~1.5 min.
3. **One pipeline, two opposite histories.** The two-region natural experiment; structural confound is the independent variable. Show both production curves. ~1.5 min.
4. **The method in one idea — "Don't ask what value. Ask whether the distribution moved."** KS test; distributional shift; the abstain-to-median design. ~2 min.
5. **Discovery works — 48 rules, 20 rules, three algorithms agreeing.** Cross-algorithm convergence + LOESS validation proves signal is real. Foreshadow the RVV 15% collapse. ~2 min.

**ACT II — THE BETRAYAL**
6. **The hardest fair test I could design.** Walk-forward, no leakage, provably-optimal median baseline. Build credibility *before* the null. ~1.5 min.
7. **It doesn't beat the median. That's the result.** Deliver the null as the pivot, confidently. State the message aloud. ~1.5 min.

**ACT III — THE UNDERSTANDING**
8. **Every forecast hides two decisions.** The two-decision decomposition; the bottleneck is Decision 1, not the rules. (Simple 2-box diagram.) ~2.5 min.
9. **Fix the trend, and you murder the signal.** The LOESS experiment: −36% baseline error, zero rules. The trade-off, measured. ~2 min.
10. **Structural confounding is the hidden variable.** Why RDD and RVV diverge; more modern data makes RVV worse. Callback to the 15% collapse. ~1.5 min.
11. **When the eras balance, the rules win — but be honest.** The 5/5 conditional window, explicitly labeled post-hoc/exploratory. ~1 min.
12. **This is a lesson about non-stationary time series.** Generalize beyond wine; three transferable cautions. ~1 min.
13. **A diagnosis — and the prescription.** Contributions + structural detrending as the fix. ~1 min.
14. **Discovery is not prediction — and knowing the difference is the science.** Land the message; invite questions. ~0.5 min.

## DESIGN REQUIREMENTS

- Academic but bold; minimal text per slide (max ~20 words on-slide); one idea per slide.
- Prefer visuals: the two production time-series curves (slide 3), a distribution-shift illustration (slide 4), a two-box trend→residual→rules diagram (slide 8), a before/after LOESS bar comparison (slide 9).
- Every slide includes **speaker notes** (~30–60 seconds of spoken script) written in first person for the presenter.
- Reinforce the spine verbally on slides 1, 7, 12, 14.
- Total spoken time ≈ 20 minutes; slide 8 is the buffer.
- Tone at slide 7 (the null result): confident, not apologetic — "That's the result."

Produce the full deck: for each slide give a **title**, **on-slide content**, and **speaker notes**.

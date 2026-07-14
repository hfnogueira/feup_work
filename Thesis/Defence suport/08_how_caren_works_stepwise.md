# How CarenR Works — Step by Step

A plain-language walkthrough of the CAREN distributional-rule pipeline, built up one step at a time. Use this to be able to narrate the algorithm end-to-end (and to answer Azevedo at the whiteboard). See `01_defense_prep_azevedo.md` Part 4 for the lattice diagram.

---

## Step 1 — Discretise the features (turn numbers into interval-items)

CAREN works with **items**, and an item is a condition of the form `feature ∈ interval`. The climate features arrive as continuous numbers (soil water 88 mm, 143 mm, 210 mm…), and CAREN cannot build rules directly on raw continuous values — so the first thing it does is **cut each feature into bins**.

The thesis uses **equal-frequency quartiles**: each feature is split into 4 bins, with cut-points chosen so each bin holds roughly the same *number of years*, not the same *width*. Over 89 Douro years, each of the 4 bins contains ≈22 years.

Example — `swa_hv_y1_b20` (soil water 20 days before harvest) becomes four interval-items:

```
swa ∈ [20, 74] mm     ← bottom quartile (~22 years)
swa ∈ [74, 101] mm    ← 2nd quartile
swa ∈ [101, 152] mm   ← 3rd quartile
swa ∈ [152, 350] mm   ← top quartile
```

Two points Azevedo may probe:

- **Why equal-frequency, not equal-width?** Equal-frequency guarantees every bin is well-populated (~22 obs). The KS test later compares a subgroup's distribution against the global one; if a bin held only 2–3 years, that test would be unreliable. Balanced bin occupancy keeps the statistics trustworthy at small *n*.
- **Where are the cut-points computed?** For Goal 1 (discovery), on the full dataset. For Goal 2 (prediction), *inside each training fold only* — so no test-year information leaks into the bin boundaries. This is the leakage guard.

**Output of Step 1:** the 59 continuous features become a pool of **59 × 4 = 236 interval-items**, and each of the 89 years is described by which bin it occupies for every feature. Step 1 just prepares the *vocabulary of conditions* the search will later combine.

---

## Step 2 — Search: build candidate subgroups level by level

CAREN combines the interval-items into subgroups, one **level** at a time (a level = how many conditions are joined with AND). The engine is **Apriori**.

- **Level 1 — single conditions.** Every item on its own. For each, count the **support** (fraction of years satisfying it). Keep only those ≥ minimum support (thesis threshold: **20%**, ≈18 of 89 years); discard the rest.
- **Level 2 — pairs.** Form two-condition subgroups *only from level-1 survivors*, across **different** features. Count support again; keep those ≥ 20%.
- **Level 3–4 — triples/quadruples**, built only from surviving smaller combinations, up to the maximum rule length (3–4).

### The monotonicity direction (get this right)
As you **add** a condition (single → pair), support can only **decrease or stay equal — never increase** (a narrower rule covers fewer or equal years). Equivalently: a shorter rule always has support **≥** any longer rule extending it. This is **anti-monotonicity**, and it powers the pruning: if a subgroup is already below 20%, *every* larger subgroup containing it is guaranteed below 20% too — CAREN never generates it. That's why the search is **exhaustive over the support-frequent region yet tractable** (contrast with a beam/greedy search, which keeps only top-*k* branches and can miss good combinations; CAREN keeps **all** frequent candidates → completeness guarantee).

Note: two bins of the *same* variable are never paired — a year can't be in `swa ∈ [20,74]` AND `swa ∈ [74,101]` at once (support 0). Pairs are always across different features.

### Concrete example — pairs
Level 1 survivors (support ≥ 20% = ≥18 of 89 years):

| Item | Condition | Years | Support |
|------|-----------|-------|---------|
| A | `swa ∈ [20,74]` mm | 22 | 25% |
| B | `rain_fl ∈ [8,15]` days | 27 | 30% |
| C | `heat_sm ∈ [3,9]` days | 24 | 27% |

*(A fourth item, `tmax_hv ∈ [28,31]`, had only 12% support → dropped at Level 1, never appears in a pair.)*

Level 2 forms every cross-feature pair and counts years satisfying **both**:

| Pair | Years with BOTH | Support | Decision |
|------|-----------------|---------|----------|
| A∧B | 20 | 22% | ✅ keep |
| A∧C | 8 | 9% | ❌ drop |
| B∧C | 19 | 21% | ✅ keep |

Every pair's support (22%, 9%, 21%) is **≤** each parent's (A 25%, B 30%, C 27%) — adding a condition only shrank it. And A∧B∧C is **never generated**, because A∧C already died at 9% (its support can't exceed 9%).

**Output of Step 2:** the surviving candidate subgroups — here `{A, B, C, A∧B, B∧C}`. Judged **only on support (coverage)** so far. Production has not entered yet — there is **no p-value at this stage**. The p-value arrives in Step 3.

---

## Step 3 — The KS test: does production actually shift in this subgroup?

For each surviving candidate subgroup, CAREN now brings in the **target** — the production residual (deviation from trend, mhl). This is the first time production is used at all.

It builds two distributions:
- residuals of the **years inside the subgroup** (e.g. the 20 years satisfying A∧B), and
- residuals of **all years** — the global/reference distribution.

Then it runs a **two-sample Kolmogorov–Smirnov (KS) test**:
1. Turn each residual set into an **ECDF** (empirical cumulative distribution — "fraction of years with residual ≤ x").
2. The **KS statistic D** = the *maximum vertical gap* between the two ECDF curves. Large D = the subgroup's whole distribution is shifted/reshaped versus global (the "KS gap" on slide 9).
3. From D and the two sample sizes, CAREN computes the **p-value** = probability of a gap that large *by chance* if the subgroup years were really drawn from the same distribution as all years.

**Support vs p-value — two different questions:**
- **Support** = "is this pattern common enough to matter?" (coverage — Step 2)
- **p-value** = "is the production shift real, or could it be noise?" (signal — Step 3)

This is why every rule reports both: `+121 mhl above trend (support 28%, p = 0.004)`. Support: 28% of years match. p = 0.004: a shift this strong would happen by luck < 0.5% of the time.

**Two points for Azevedo:**
- **Why KS, not a t-test?** KS is non-parametric — compares the *whole* distribution (tails included), assumes no normality, valid at small n (80–89). Production residuals aren't Gaussian, so a mean-only test would miss shape shifts.
- **The reference set includes the subgroup.** That overlap makes the test *conservative* (the two ECDFs are pulled together, so D is understated, not inflated) — say this before he raises it.

**Output of Step 3:** every candidate subgroup now carries a **(D, p-value)**. Nothing kept or discarded yet — that is Step 4.

---

## Step 4 — Retain: keep the rules that pass both filters

Every candidate now has two numbers: **support** (Step 2) and **p-value** (Step 3). Step 4 applies **two thresholds at once** and keeps only subgroups clearing both:

- **Statistical filter:** p ≤ **0.10** — the shift is unlikely to be noise.
- **Support filter:** support ≥ **20%** (≈18 of 89 years) — common enough to matter.

Pass both → it becomes a **distribution rule**. Fail either → dropped. (A candidate can die three ways: too rare → support fails, pruned in Step 2; common but not distinct → p fails here; or both.)

Final human-readable form:

```
IF  rain_fl ∈ [0, 3.3]  AND  rain_mat ∈ [0, 3.5]
THEN production is +121 mhl above trend      (support 28%, p = 0.004)
```

The `+121 mhl` is the **median residual of the subgroup's years** — the typical shift, and the value the rule "predicts" if used for forecasting. This step produces the headline counts: **48 rules (Douro), 20 (Vinho Verde)**.

**Two points for Azevedo:**
- **The thresholds are *thesis* design choices, not the CAREN algorithm.** CAREN gives the (support, p) pair; "≥20% and ≤0.10" is your decision. Figure 3.x separates blue (algorithm) from green (your choices) exactly here — own that boundary.
- **p ≤ 0.10, not 0.05, is deliberate.** At n=80–89 with 20% support, subgroups have ~18–25 obs where KS has limited power; 0.05 empties the rule set in several folds. Rules face a second, harsher filter downstream (out-of-sample MAE in Goal 2), so the lenient discovery threshold is compensated later.

**Core pipeline complete:** discretise → search (support) → KS test (p-value) → retain (both filters) = a set of interpretable distribution rules.

---

## Step 5 — Prediction (Goal 2 only): turn rules into a forecast

Steps 1–4 are rule *discovery* (Goal 1). Goal 2 asks whether those rules *forecast*. When a **test year** arrives:

1. **Which rules fire?** Check the test year's climate conditions against every retained rule; a rule "fires" if the year satisfies all its conditions.
2. **Combine the fired rules.** Each fired rule carries a median residual (its `+/− mhl` shift). CAREN combines them by **support-weighted average** — each rule's shift weighted by its support fraction, weights normalised to sum to 1. (Most variants; a few use confidence weighting from the KS p-value, or stacking.)
3. **Fallback if none fire.** If no rule matches the test year, predict the **training median residual** (≈0) — identical to the Naive_Median baseline.
4. **Re-trend.** Add the shift back onto the extrapolated linear trend to get a production figure in mhl, comparable to the actual value for MAE.

**Why the fallback matters (say this):** because CAREN abstains to the median when unsure, it can only beat or lose to the baseline *when it actually fires a rule*. That makes the walk-forward test a clean question — "do the fired rules add value beyond the median?" — and it's why the RVV result (fired rules are significantly *worse*, p=0.004) is so pointed: the rules aren't just unhelpful there, they're actively harmful when they act.

**All of Step 5 runs inside each training fold** — discretisation, support/KS filtering, and the median shifts are all computed on training years only, then applied to the held-out test year. That is the no-leakage guarantee.

**Output of Step 5:** one production forecast per test year → the walk-forward MAE that Goal 2 evaluates against Naive_Median.

---

## Small-dataset implications & advantages (n = 80–89)

**One-line version for the viva:** *CAREN is well-matched to small data because it's non-parametric, distribution-level, exhaustive, and interpretable — it asks little of the data and gives back something a human can audit. But small data also caps what it can deliver: low power, unavoidable multiplicity, and the discovery-vs-prediction gap that is itself the thesis's main finding.*

### Why CAREN is a sensible choice for small data — advantages
- **Non-parametric by design.** The KS test estimates nothing — no mean, variance, or shape. Parametric models spend degrees of freedom they can't afford at n<90; CAREN spends none. The single biggest reason it fits here.
- **Distributional sensitivity beats mean-tests at low n.** Comparing whole ECDFs (tails included) can detect a shift even when group means overlap — signal a t-test or a tertile classifier (RIPPER) would miss.
- **The support floor is a small-data safeguard.** Requiring ≥20% coverage (≈18 years) stops "rules" that fit 3 lucky years; every rule is backed by a meaningful slice of a short record.
- **Exhaustive search is affordable — and complete.** Few observations → small candidate space → search it fully. Completeness guarantee (can't miss a frequent pattern) with no need for heuristics.
- **Interpretability is the right currency when power is low.** When you can't prove much statistically, auditable rules an agronomist can sanity-check give **face validity** — a second, human form of evidence a black box gives neither.
- **Uses every observation for discovery + abstains when unsure.** Goal 1 mines all data (no hold-out waste); the median fallback means prediction only risks being wrong *when it fires* → graceful degradation.

### What small data implies — cautions (state these before he does)
- **Low power — hence p ≤ 0.10.** ~18–25 obs per subgroup → limited KS power, borderline p-values. The loosened threshold is a reasoned response to small n, not a fudge.
- **Multiplicity → expected false discoveries.** Large candidate space vs few observations → some of the 48 rules are chance. No permutation/FDR null; name the label-permutation test as the fix.
- **The discovery ≠ prediction asymmetry is a small-n phenomenon.** A pattern real enough to discover in-sample (|r|≈0.47, ~22% variance) isn't strong enough to beat the median out-of-sample at CV≈47% over ~17 test years. The central finding — largely a consequence of small, noisy data.
- **Instability of preprocessing.** Quartile cut-points and feature selection on 60–89 fold-internal points can shift between folds; selection-stability wasn't reported (a fair ask).
- **Structural/era overfitting.** A short non-stationary series lets rules encode era membership rather than climate (RVV: rules become harmful). Hard to separate climate from a structural break with only *time* as the detrend covariate.
- **Coarse ECDFs & exact-vs-asymptotic p-values.** Each ECDF moves in steps of 1/n → granular D; the exact/asymptotic distinction shifts p-values — one more reason to treat rules as exploratory.

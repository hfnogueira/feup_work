# Defense Preparation Dossier — Arguente: Prof. Paulo Jorge Azevedo

**Who he is:** Assistant professor, Dep. Informática, U. Minho; senior researcher at INESC TEC (HASLab). PhD from Imperial College (logic programming). **He is the author of the CAREN engine and co-author of the Distribution Rules paper your entire thesis is built on.** Research: association rules, distribution rules, subgroup discovery, rule interest measures, time-series motifs, model-performance analysis via rules.

His publications most relevant to you:

| Paper | Why it matters for your defense |
|---|---|
| Jorge, Azevedo & Pereira (2006), *Distribution Rules with Numeric Attributes of Interest*, PKDD 2006, LNAI 4213, pp. 247–258 | The DR formalism + KS test you use. He co-invented it. Know it cold. |
| Azevedo (2003), *CAREN — a Java based Apriori implementation for classification purposes*, Tech. Report, U. Minho | CAREN is **his** engine — Apriori-based, exhaustive levelwise search. |
| Azevedo & Jorge (2007), *Comparing Rule Measures for Predictive Association Rules*, ECML | His work on interest measures (conviction wins). He may ask why you weight rules by support rather than by an interest measure. |
| Pimentel, Azevedo & Torgo (2023), *Subgroup mining for performance analysis of regression models* (Error Distribution Rules), Expert Systems | He uses rules to analyze **where regression models fail** — exactly what your era-composition analysis does informally. Expect a question; cite it. |
| Soares, Azevedo, Cerqueira & Torgo (2025), *Meta Subspace Analysis*, Discovery Science 2025 | Recent extension of the same idea to metafeature space. |

**Strategic framing:** he will be *sympathetic* to the method (it's his) and *demanding* on how faithfully you describe it and how rigorously you evaluate it. Your risk is not "why CarenR?" — it's misdescribing CAREN and sloppy statistics.

---

## PART 0 — CRITICAL: HE IS READING THE SUBMITTED (UNCORRECTED) VERSION

The jury reads the version you deposited — **the errors are still in his copy.** You cannot rely on the fixes being visible to him; you must be able to answer *out loud* when he points at the printed page. The universal move:

> **Acknowledge instantly → state the correct fact → note it is on your errata sheet.** No over-apologising, no defensiveness. He is meticulous about his own work; owning an error cleanly *raises* your credibility, flailing lowers it.

Bring the printed **errata sheet** and offer it at the start ("I identified some editorial corrections after submission and prepared an errata; may I hand it to the jury?"). Ask your supervisor if handing it up front is the local custom — if yes, it pre-empts half of these.

### If he points to the submitted page, say this:

| He points at (his copy) | What he sees | Your spoken answer |
|---|---|---|
| **p.36** — "CAREN … originally developed by Jorge et al. (2006)" | CAREN credited to the 2006 paper | "That's a mis-attribution I've corrected — the CAREN engine is **your** 2003 work; Jorge, Azevedo and Pereira 2006 added the distribution-rule layer on top. It's item 1 on my errata." *(Then state the provenance confidently — it flatters him and proves you know the lineage.)* |
| **p.36 / p.28** — "beam search" / "breadth-first, most promising conditions" | Two different, both wrong, search descriptions | "CAREN does an **Apriori-style levelwise search pruned by minimum support** — exhaustive over the support-frequent itemsets, not a heuristic beam. The two passages were inconsistent; corrected to that. I can sketch the lattice if useful." *(Be ready to draw it — see Part 1 item 4.)* |
| **p.36–38 / Appx A, B** — "support ≥ 15%" | 15% support threshold | "The value actually used in the code is **20%** — `carenr_min_sup = 0.2`. The results chapters are already at 20%; the earlier chapters lagged. My Appendix E logged the correction; the errata propagates it. So: a rule must cover **≥18 of 89 years** in RDD." |
| **p.99** — DOI `…11871637_25` | Wrong DOI on the 2006 paper | "Typo — correct DOI ends `_26`, LNCS 4213, pp. 247–258. On the errata." |
| **p.100** — "Contrast Association Rules for Extraordinary Nodes" + "TODO: confirm…" | Fabricated title + a literal TODO in the bibliography | "That title expansion was wrong and the TODO shouldn't have printed — both removed. I'm confirming the exact package citation with Prof. Moreira." *(Don't invent a replacement title on the spot.)* |
| **p.99–100** — "TODO: confirm…" on Fraga/Lopes | Placeholder notes visible in refs | "Editorial placeholders that slipped through; removed, metadata being confirmed with my supervisors." |

**Tone rule:** each answer is one or two sentences. Acknowledge, correct, move on — do **not** spiral into apology, and do **not** argue the error doesn't matter. "Good catch, here's the correct version, it's on my errata" is the whole play. None of these touches a result, and you can say that plainly if he asks the significance: *"none of the corrections affects any table, test, or conclusion — the 20% threshold was used in all computations; only the prose lagged."*

---

## PART 1 — CORRECTIONS (already applied in your working LaTeX; his copy still has them)

These were factual errors about *his own work*. **All are now fixed in the LaTeX and the PDF recompiles cleanly** (see `ERRATA.md`). Listed here so you know what changed and never say the old versions aloud. If any is raised in the defense, the honest answer is "yes — that's on my errata sheet, already corrected."

1. **CAREN mis-attribution [FIXED].** Ch3 said CAREN was "originally developed by Jorge et al. (2006)". Wrong: the CAREN engine is **Azevedo's** (2003 tech report, U. Minho); Jorge, Azevedo & Pereira (2006) added the **Distribution Rules** framework on top. Now reads "…developed by Azevedo (2003) and extended to distribution rules by Jorge et al. (2006)". New `Azevedo2003` bib entry added. **Say it this way out loud when you introduce the method.**

2. **Wrong DOI [FIXED].** Was `10.1007/11871637_25`; corrected to `10.1007/11871637_26` (PKDD 2006, LNCS 4213, pp. 247–258, Springer), with volume/pages added.

3. **Invented backronym [FIXED].** "CarenR: Contrast Association Rules for Extraordinary Nodes" was a fabricated expansion carrying a printed TODO. Removed; the entry now neutrally describes the R package. **Still confirm the exact Moreira (2022) citation with Prof. Moreira** — the definitive version should not guess. (CAREN itself traces to Azevedo's "class project association rule engine".)

4. **Search-strategy inconsistency [FIXED].** Ch2 said "beam search", Ch3 said "breadth-first, most promising conditions" — contradictory, and neither matched CAREN, which is **Apriori-style levelwise search pruned by minimum support**. Both now say that (verified against the `carenR` package call in `6_scenario_validation.R`). This was the single most likely whiteboard question — **be ready to draw the levelwise lattice**: level 1 = all single feature-bin conditions passing min-support; level *k* built from support-frequent level-(*k*−1) itemsets; KS test applied to each surviving subgroup; max length 3–4.

5. **Support threshold [FIXED — value is 20%].** Text said 15% in Ch2/Ch3/Appx A/B but 20% in Ch4–6. The **code uses 0.20** (`carenr_min_sup <- 0.2`), so 20% is correct; all text now says 20% (and derived counts: ≈18 years of 89). If asked "what was your support threshold?", the answer is **20%, i.e. a rule must cover ≥18 of 89 years (RDD)**. Note this correction was already logged in your Appendix E — it had just not been propagated to every chapter.

6. **Printed TODO notes [FIXED].** The submitted bibliography printed literal "TODO: confirm…" notes on the Moreira, Fraga and Lopes entries — visible to any reader. Removed. **Confirm the real metadata for all three with your supervisors** before the definitive submission.

7. **Terminology.** In his vocabulary: *distribution rules (DR)*, *antecedent/consequent*, *support*, *KS interest*. Don't say "beam", "gain", or "coverage heuristic" for CAREN.

**Turn Appendix E into an asset.** Your AI-transparency appendix already documents a numerical self-verification pass that *caught* the 15→20% error (among others). If the support-threshold or any numeric point comes up, note that the thesis includes an explicit verification appendix — it demonstrates exactly the critical-engagement rigor an examiner wants to see, and reframes "there were errors" as "I built a process that finds and fixes them."

---

## PART 2 — LIKELY QUESTIONS AND PREPARED ANSWERS

### A. CAREN / Distribution Rules internals (his home turf)

**Q1. "Explain exactly what a distribution rule is and how your usage differs from the 2006 paper."**
A: A DR is IF antecedent THEN empirical distribution of the numeric property of interest, deemed interesting when the KS test rejects equality with the default (whole-dataset) distribution. My usage follows the 2006 formalism; the thesis-level additions (explicitly separated in Fig. 3.x) are: the property of interest is a *detrended residual* computed within-fold; a support filter of 20%; p ≤ 0.10; support-weighted aggregation of subgroup medians for point prediction; and a fallback to the training median when no rule fires. The algorithm is unchanged — the contribution is the temporal validation harness around it.

**Q2. "You test each subgroup against the full dataset, which contains the subgroup itself. The two samples are not independent."**
A: Correct — this follows the original DR design (subgroup vs. default distribution), which makes the test conservative in the direction that matters: overlap pulls the two ECDFs together, so D is understated, not overstated. Comparing subgroup vs. complement is a known alternative; with n=80–89 and 20% support the difference is modest, and I kept the default-distribution convention for consistency with the DR literature.

**Q3. "Thousands of candidate subgroups, p ≤ 0.10, no multiple-testing correction. How many of your 48 RDD rules are false discoveries?"**
This is the single most probable hard question. A: I don't rely on the raw p-values alone for validity. Four independent guards: (1) 20% minimum support (≥18 of 89 years) removes the most fragile candidates; (2) the LOESS re-validation shows the top features retain 90–92% of their correlation once era co-trending is removed — an era-artefact rule would not survive that; (3) RIPPER and M5Rules converge on the same variable families under completely different criteria; (4) the walk-forward evaluation is itself an out-of-sample test of the rules. I present the rule set as validated *exploratory* findings, not confirmatory hypotheses — and the honest statistical framing in Ch5 (confirmatory vs. exploratory tests, uncorrected p-values disclosed) applies the same standard. If pressed: acknowledge a permutation/FDR analysis (e.g., swap-randomization of the target) is a natural extension.

**Q4. "Why p ≤ 0.10 and not 0.05?"**
A: A deliberate sensitivity/power trade-off at n=80–89: with 20% support a subgroup has ~18–25 observations, where the KS test still has limited power; 0.05 empties the rule set in several folds. Since rules face a second, harsher filter — out-of-sample MAE — the lenient discovery threshold is compensated downstream. (Know the number: how many rules survive at 0.05. If you don't, run it before the defense.)

**Q5. "The KS test is asymptotic; with a subgroup of 12 years, are the p-values even valid? And ties?"**
A: Subgroup sizes are small but the reference set is 60–89; the exact/asymptotic distinction at these sizes shifts p-values slightly, which is one more reason the rules are treated as exploratory. Residuals are continuous, so ties are rare. (If you used R's `ks.test`, it computes exact p-values for small samples without ties — check which the caren package uses and say so.)

**Q6. "The DR paper filters redundant rules against their ancestors (improvement). Do you?"**
A: Yes — Section 4.x analyses rule overlap and unique-vs-redundant rules; the rule sets organise around a small number of backbone features with contextual refinements. (Re-read §4 "Rule Set Overlap and Internal Coherence" the week before — be able to give the counts.)

**Q7. "Why quartile equal-frequency discretisation of antecedents? CAREN supports supervised discretisation (Fayyad–Irani)."**
A: I tested exactly that — CarenR_Supervised derives cut-points maximising residual separation within each training fold. It was the **worst** variant in both regions (240/347 mhl): the bin boundaries are optimised on the same data that assesses rule quality, a target-leakage effect that inflates rule counts (46–146 per scenario) and collapses out of sample. Equal-frequency quartiles guarantee uniform bin occupancy, which the KS test needs at this n. This negative result is one of the replicable findings of the thesis.

**Q8. "You aggregate fired rules by support-weighted medians. Why support and not an interest measure — KS statistic, conviction?"**
A: I evaluated confidence-style weighting (CarenR_Dist_Conf, weights from KS p-values) and stacking; the aggregation mechanism turned out to be second-order — variant ranking is driven by feature selection, not weighting (§3.x). Support weighting favours estimates whose subgroup medians are computed on more observations, i.e., reliability weighting. If he presses on conviction: conviction is defined for categorical consequents; for DRs the natural analogues are KS-derived, which is what Dist_Conf tested.

**Q9. "DRs give you an entire distribution. Why throw it away and predict only the median? Why not evaluate distributional forecasts (CRPS, quantile loss)?"**
A: Fair criticism and a very natural extension. The point forecast was chosen for comparability with the baselines and the agronomic use case (a single expected-production figure). But the fallback design already exposes the distributional view — when no rule fires the forecast *is* the training distribution's median. Evaluating the full predictive ECDF with CRPS is listed as future work; with 17 test years per region the CRPS estimates would themselves be noisy. (If this isn't in future work yet — add one sentence to ch7.)

### B. Evaluation methodology (his EDR / model-performance angle)

**Q10. "Your Wilcoxon test treats 18 expanding-window scenarios as independent samples. They share most of their training data and their test sets overlap."**
A: Correct, the scenarios are positively correlated, which inflates the effective sample size of any test. This affects both directions equally and mostly makes the tests *less* likely to reach significance — consistent with the honest null result (p=0.468). The RVV negative result (W=0, p=0.004) is a unanimous sign pattern across all 9 rule-firing scenarios, robust to the dependence caveat. I flag the p-values as indicative, not exact.

**Q11. "You selected the best of 11 CarenR variants after seeing the results, then compared it to the baseline."**
A: Yes — and that selection bias *strengthens* the headline conclusion, because even the post-hoc best variant fails to beat Naive_Median (3% behind in RDD, 22% in RVV). The bias would only matter if I claimed victory. The one positive claim (era-balanced scenarios 10–14, p=0.031) is explicitly labelled post-hoc/exploratory throughout, requiring prospective validation.

**Q12. "Your era-composition analysis asks 'where does the model fail'. Are you aware this is exactly what Error Distribution Rules do?"**
A: Prepare to say YES. "Pimentel, Azevedo & Torgo (2023) formalise this as subgroup mining over model errors — EDRs. My era-composition analysis does this manually with one covariate (post-1990 training share). Applying EDRs to the walk-forward error series, with era share and climate features as descriptors, would be the principled generalisation and is an excellent direction for the future-work section." Consider actually adding a citation to this paper in ch6/ch7 before the defense — it is respectful, correct, and defuses the question.

**Q13. "Why weighted MAE? Weighting by test-set size makes early, era-confounded scenarios dominate."**
A: That interaction is analysed explicitly in §6.4: weighting by test size is the statistically correct choice (more observations, more influence), and I report per-scenario MAE and mean MAE alongside so the reader sees the era structure. The sensitivity is a property of the data's era composition, not an artefact I hid.

**Q14. "Naive_Median is a straw man / why is it so hard to beat?"**
A: It's the opposite of a straw man — it's the MAE-optimal constant predictor on the residual scale, so beating it is precisely the claim "climate features carry exploitable signal." The two-decision decomposition separates trend accuracy (Decision 1) from climate signal (Decision 2), so the baseline is doing diagnostic work, not just benchmarking.

### C. Results and interpretation

**Q15. "Goal 2 failed. What did we learn?"**
A: Three transferable findings: (1) the discovery–prediction asymmetry is a property of the system — |r|≈0.47 ⇒ ~22% variance explained is enough for statistically valid pattern discovery, not for beating a median at CV≈47% noise; (2) the two-decision decomposition localises the bottleneck in structural detrending, not rule learning — time as sole detrending covariate conflates industry restructuring with climate; (3) confirmed negative results with practical value: supervised discretisation leaks, LOESS consumes the signal it should expose, and RVV rules actively harm (p=0.004). The path forward is structural covariates (area, benefício quota, training-system share) in the detrend model — data exists at IVV/IVDP.

**Q16. "In RVV your rules make predictions worse. Shouldn't the method detect that its own rules are era artefacts?"**
A: That's the correct reading — and it's why the RVV result matters. The KS test validates rules against the *training* distribution; when train and test straddle a structural break (1986 EU accession, pergola→espalier conversion, −65% production), statistically valid training-era rules encode era membership. CarenR partially resists this (the detrending reframing, §6.6), but no within-sample statistic can certify out-of-era generalisation. That's an argument for the walk-forward protocol itself — it caught what discovery-only evaluation would have missed.

**Q17. "Only 17 test years per region. Are any of these conclusions stable?"**
A: Acknowledged explicitly in ch2 (§ limitations): 17 years spans ~3 cycles of the dominant 5.7-year oscillation. This is why the thesis reports per-scenario results, distinguishes confirmatory from exploratory tests, and avoids strong positive claims. The alternative — random CV — would be temporally dishonest and inflate performance.

**Q18. "What exactly is new here versus the group's previous CarenR work (Moreira et al.)?"**
A: Previous work applied CarenR for discovery on agricultural data. This thesis adds: first walk-forward predictive validation of DRs (fold-internal discretisation, feature selection, and rule induction — zero leakage); the two-decision decomposition as a diagnostic; the era-confound identification and LOESS re-validation methodology; the 11-variant structured sensitivity analysis; and the cross-regional structural comparison. In short: the thesis converts CarenR from a discovery tool into a fully evaluated forecasting pipeline, and shows precisely where and why the forecasting half breaks.

**Q19. "Why not Exceptional Model Mining / pysubgroup / Cortana? Why CAREN specifically?"**
A: DRs are the right primitive: the target is a single numeric variable and the question is distributional shift — exactly the DR interestingness criterion, with the KS test giving nonparametric validity at n<90. EMM would fit a model per subgroup — with ~12–25 observations per subgroup there is no capacity for that. And CAREN's exhaustive support-pruned search gives completeness guarantees a heuristic searcher doesn't. (Bonus: this answer compliments his design decisions — accurately.)

**Q20. Rapid-fire facts to have memorized.**
- RDD: n=89 (1934–2022), 18 scenarios; RVV: n=80 (1942–2021), 16 scenarios; 17 test years each.
- RDD: Naive_Median 184.0 vs CarenR_Dist_Eta2 189.2 (3% gap); RVV: 140.4 vs Sup_FS 171.6 (22%).
- Tests: RDD Wilcoxon W=68 p=0.468 (confirmatory, ns); RDD era-balanced sign test p=0.031 (post-hoc); RVV rule-firing Wilcoxon W=0 p=0.004 (confirmatory negative).
- Rules: 48 RDD, 20 RVV; support ≥20% (≈18/89 yrs); p ≤ 0.10; quartile equal-frequency bins; 59 features; max rule length 3–4.
- LOESS span 0.40: RVV baseline 140→~90 but 0 rules; RDD baseline 184→290.
- LOESS retention of top features: 90–92% (harvest temperature only 56% — era-inflated, correctly not selected by CarenR).

---

## PART 3 — DEFENSE STRATEGY

1. **Part 1 items are already fixed in the LaTeX** (PDF recompiles cleanly). Bring the errata sheet (`ERRATA.md`) to the defense; if any item comes up, "already on my errata list" turns it into a point in your favour. Still to do on your side: confirm the Moreira/Fraga/Lopes citation metadata with your supervisors for the definitive deposit.
2. **Cite him back.** Work one slide/sentence connecting your era-failure analysis to his EDR paper (2023). Examiners soften when their line of work is engaged seriously, and here the connection is real, not flattery.
3. **Own the negative result.** Your strongest asset is the honest statistical framing (confirmatory vs. exploratory, disclosed uncorrected p-values, leakage-free folds). Lead with it; he values it — his own papers are meticulous about evaluation.
4. **Search strategy — confirmed and fixed.** The `carenR` package call performs CAREN's support-pruned levelwise (Apriori-style) candidate generation; the thesis-level cap is rule length 3–4. Safe verbal answer: "the R package inherits CAREN's support-pruned levelwise candidate generation; I cap rule length at 3–4." Be ready to sketch the lattice (see Part 1, item 4).
5. **Prepare the one number you don't have:** how many rules survive at p ≤ 0.05, and rule counts in the redundancy analysis. He asks for numbers.

---

## PART 4 — WHITEBOARD: "How does CAREN search?" (the most likely live question)

If he asks how CAREN generates rules, **draw a three-row lattice and talk through it.** This is his algorithm — getting it right here is worth more than any slide.

### What an "item" is
An *item* is a single feature-bin condition, where the bin is an **explicit numeric interval** produced by your Step-1 equal-frequency (quartile) discretisation — **not** a categorical label like "low." For example `swa_hv_y1_b20 ∈ [20–74 mm]` (the bottom quartile of pre-harvest soil water), or `∈ [74–101 mm]`, `∈ [101–152 mm]`, `∈ [152–350 mm]`. Each of your 59 features contributes 4 such interval-items, so the pool is 59 × 4 items. The search explores *conjunctions* of these interval conditions, one level at a time.

*(Be precise on this if he probes: bins are quartile intervals with real boundaries computed on the training fold — equal-frequency, so ~22 of 89 obs per bin in RDD — and a rule antecedent is a conjunction like `swa ∈ [20–74] ∧ rain_fl ∈ […]`. Say "interval," never "low/high," when describing an item.)*

### Draw three rows

Label the nodes with **interval items** (write the interval, not "low/high"). Use short names — `swa` = pre-harvest soil water, `rain_fl` = wet days post-flowering, `heat_sm` = hot days pre-maturity, `tmax_hv` = max temp at harvest:

- **Level 1 — single interval conditions.** Compute each condition's **support** (fraction of years it covers). Keep those ≥ 20%, cross out the rest. *(In the sketch: `tmax_hv ∈ [28,31]` dropped at 12%.)*
- **Level 2 — pairs, formed only from level-1 survivors.** Compute support again; drop those below 20%. *(`swa∈[20,74] ∧ heat_sm∈[3,9]` dies at 9%.)*
- **Level 3 — triples, formed only from surviving pairs.** The triple is **never generated**, because one of its pairs already failed. Draw it as a ghost box. Cap length at 3–4 conditions.

```
              A = swa∈[20,74]   B = rain_fl∈[8,15]   C = heat_sm∈[3,9]   D = tmax_hv∈[28,31]

Level 1:   [A ✓ 34%]   [B ✓ 44%]   [C ✓ 27%]   [D ✗ 12%]        ← support filter
                \        /   \        /
Level 2:       [A∧B ✓ 22%]   [A∧C ✗ 9%]   [B∧C ✓ 21%]           ← support filter
                       \          |          /
Level 3:                ( A∧B∧C  never generated )               ← pruned via A∧C
```

Each node is a **conjunction of intervals**; a surviving node that also passes the KS test becomes a rule like *"IF swa∈[20,74] mm AND rain_fl∈[8,15] days THEN production distribution shifts +… mhl."*

### The one idea that makes it worth drawing — anti-monotonicity
Support can only **stay equal or shrink** as you add interval conditions (a narrower conjunction covers fewer or equal years). So if a small itemset is already below threshold, *every* larger itemset containing it is guaranteed to be below too — you skip it without testing. That is the cascade: dropping `tmax_hv∈[28,31]` removes all its pairs; dropping `A∧C` removes the `A∧B∧C` triple. This is what keeps an exhaustive search tractable.

### Two filters, not one (say this explicitly)
Support is only the **first** filter — it *builds the lattice*. Then the **KS test** runs on each surviving subgroup, comparing its residual distribution against the global one; subgroups with p ≤ 0.10 become distribution rules (the green boxes). So there are two distinct criteria: **support = structural/coverage; KS = statistical.** Students routinely conflate them — keeping them separate signals that you understand his method.

### Why "exhaustive, not greedy" matters (turns the old error into a strength)
Contrast it aloud with the discarded "beam search" wording. A beam/greedy learner keeps only the top-*k* branches per level and can permanently miss a good combination. CAREN keeps **all** frequent candidates, so it has a **completeness guarantee** over the frequent region — a genuine strength of his design.

### The sentence that answers the whole question
> *"Levelwise candidate generation with Apriori support-pruning by anti-monotonicity — exhaustive over the frequent itemsets, not a heuristic beam — and then the KS test as a second, statistical filter that selects the rules. I cap rule length at 3–4 conditions."*

**Caveat — stay honest:** the supports in the sketch (34%, 12%, 9%…) are illustrative, not from your data. Draw the *structure*; don't quote those specific numbers as real results.

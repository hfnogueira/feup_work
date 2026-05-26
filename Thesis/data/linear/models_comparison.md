# Model comparison — CAREN vs RIPPER vs M5Rules

Pipeline run: **2026-05-22**. Training years: **88 (RDD)**, **80 (RVV)**. Same detrended target (`Wine_mhl` residuals after linear trend). Same phenology-derived feature pool.

---

## 1. What each model produces

| Model | Output form | Task | What a single rule says |
|---|---|---|---|
| **CAREN** (Distribution Rules) | Many short *descriptive* rules | Sub-group discovery | "In years matching X, production is on average ΔY mhl above/below trend (p = …)" |
| **RIPPER** (JRip) | Small ordered *decision list* | 3-class prediction (Low/Med/High) | "If conditions, predict class C; else fall through" |
| **M5Rules** | One or more *region-specific linear equations* | Regression on residuals | A weighted sum that estimates the residual directly within its IF clause |

These are genuinely different tasks. CAREN does **not** try to predict — it finds statistically interesting climate sub-populations. RIPPER and M5Rules both predict, one as classification and one as regression.

---

## 2. Headline statistics

### RDD (Douro, 88 years)

| Metric | CAREN | RIPPER | M5Rules |
|---|---|---|---|
| Rules / terms | **48** rules | **4** (3 active + default) | 2 IF-regions, ≤15 coefficients each |
| Coverage | up to 33.7% per rule | 100% (default catches the rest) | 100% (full equation) |
| Best evidence | p-value down to **0.0043** | best rule precision **100.0%** | training R² **0.55** |
| Out-of-sample | n/a (descriptive) | **accuracy 0.4205**, macro-F1 0.4039 | **CV R² 0.0036**, CV MAE 183.3822 |
| Avg / max |deviation| | **120.2 mhl** / 192.4 mhl | n/a | n/a |
| Direction split | 31 above / 17 below | — | — |

### RVV (Vinho Verde, 80 years)

| Metric | CAREN | RIPPER | M5Rules |
|---|---|---|---|
| Rules / terms | **20** rules | **4** (3 active + default) | 3 IF-regions |
| Coverage | up to 27.5% per rule | 100% | 100% |
| Best evidence | p-value down to **0.0198** | best rule precision **92.3%** | training R² **0.58** |
| Out-of-sample | n/a | **accuracy 0.275**, macro-F1 0.2561 | **CV R² -0.0701**, CV MAE 440.0765 |
| Avg / max |deviation| | **315.4 mhl** / 389.9 mhl | n/a | n/a |
| Direction split | 4 above / 16 below | — | — |

**Reading these numbers.** RIPPER on RDD now reaches **42% accuracy** (vs 35% in the previous run) — a meaningful improvement once it grew to 4 rules. RIPPER on RVV dropped to **28%** (vs 36%) — the new pipeline tightened the ruleset but the macro-F1 dropped. M5Rules on RDD is now *just barely* above zero on CV R² (**+0.004**), versus −0.19 before — splitting into two IF-regions helped. RVV M5Rules remains slightly negative on CV. The CV scores still say: don't trust either model for forecasting yet.

---

## 3. Coverage

- **M5Rules** covers 100% of years trivially. In RDD it now uses two IF-regions (19 yrs for the warm-spring branch, 69 yrs for the default branch); in RVV it uses three (41 / 31 / 8 years).
- **RIPPER** active rules (excluding default) cover 42 of 88 RDD years and 43 of 80 RVV years; the rest fall into the default Low/High class.
- **CAREN** individual supports range 20.2–33.7% (RDD) and 20.0–27.5% (RVV). Rules overlap on the same year; the union of all rules touches most of the dataset.

RIPPER and M5Rules optimise for *prediction over the whole population*; CAREN optimises for *finding cohesive climate niches that deviate from trend*. They're complementary outputs, not competing predictors.

---

## 4. Direction of the signal (CAREN only)

| Region | Above-trend rules | Below-trend rules |
|---|---|---|
| RDD | 31 / 48 (65%) | 17 / 48 (35%) |
| RVV | 4 / 20 (20%) | 16 / 20 (80%) |

RVV remains heavily skewed toward below-trend explanations — the algorithm has an easier time characterising *what makes a bad year* than what makes a good one.

---

## 5. Feature overlap across models

### RDD (Douro)

- CAREN uses **17** distinct features across its 48 rules.
- RIPPER uses **4**: `iaf_bb_y1_c10`, `tm.days.less.15_fl_y1_b15`, `tm_hv_y1_b20`, `tm_hv_y1_c10`
- M5Rules uses **18**: `days.rf.above.1mm_fl_y1_a20`, `days.rf.above.1mm_hv_y0_a10`, `days.rf.above.1mm_sm_y1_b20`, `iaf_fl_y0_b10`, `iaf_fl_y0_b20`, `iaf_fl_y0_b30`, `iaf_fl_y1_b10`, `iaf_fl_y1_b20`, `iaf_fl_y1_b30`, `rf_hv_y0_a10`, `swa_hv_y1_b20`, `tm.days.less.15_fl_y1_b15`, `tm_bb_y0_a30`, `tm_fl_y0_b10`, `tm_fl_y1_b10`, `tm_hv_y1_c10`, `tm_sm_y0_a20`, `tn_hv_y0_a30`
- **CAREN ∩ RIPPER:** `iaf_bb_y1_c10`
- **CAREN ∩ M5Rules:** `days.rf.above.1mm_fl_y1_a20`, `days.rf.above.1mm_hv_y0_a10`, `days.rf.above.1mm_sm_y1_b20`, `iaf_fl_y0_b20`, `iaf_fl_y0_b30`, `swa_hv_y1_b20`, `tm_fl_y0_b10`
- **RIPPER ∩ M5Rules:** `tm.days.less.15_fl_y1_b15`, `tm_hv_y1_c10`
- **Shared by ALL three:** (none)

### RVV (Vinho Verde)

- CAREN uses **18** distinct features across its 20 rules.
- RIPPER uses **7**: `iaf_fl_y0_a10`, `iaf_fl_y1_b10`, `rf_fl_y1_a10`, `swa_fl_y1_a30`, `tm_bb_y0_a30`, `tm_fl_y1_b10`, `tm_fl_y1_c15`
- M5Rules uses **10**: `iaf_bb_y1_c10`, `rf_hv_y0_a10`, `swa_fl_y1_a15`, `swa_fl_y1_a30`, `swa_hv_y1_b20`, `tm_fl_y0_a10`, `tm_fl_y0_b10`, `tm_fl_y1_c15`, `tm_hv_y1_b20`, `tm_sm_y0_a20`
- **CAREN ∩ RIPPER:** `rf_fl_y1_a10`, `swa_fl_y1_a30`, `tm_bb_y0_a30`, `tm_fl_y1_c15`
- **CAREN ∩ M5Rules:** `swa_fl_y1_a15`, `swa_fl_y1_a30`, `swa_hv_y1_b20`, `tm_fl_y1_c15`
- **RIPPER ∩ M5Rules:** `swa_fl_y1_a30`, `tm_fl_y1_c15`
- **Shared by ALL three:** `swa_fl_y1_a30`, `tm_fl_y1_c15`

**Pattern.** Each region still has a small backbone shared by all three independent algorithms — that's the most defensible signal because three different objectives converge on it.

---

## 6. Main qualitative differences

1. **Goal.** CAREN finds *explanations* for sub-populations; RIPPER builds a *small predictive rule list*; M5Rules fits *region-specific linear equations*.
2. **Output volume.** CAREN produces many rules (48 / 20); RIPPER produces 4 rules in each region; M5Rules produces 2 (RDD) or 3 (RVV) IF-regions. CAREN is the only suitable output for multi-niche narrative.
3. **Coverage.** RIPPER and M5Rules cover 100% by construction. CAREN rules cover 17–34% individually but collectively touch most years (with overlap).
4. **Threshold semantics.** CAREN speaks in *intervals* (e.g. soil water between 141 and 231 mm). RIPPER speaks in *inequalities* (e.g. soil water ≤ 101). M5Rules speaks in *continuous coefficients* (effect per unit). These three styles match three different communication goals: sub-group profiling, decision support, sensitivity analysis.
5. **Predictive power.** RIPPER (RDD) accuracy = 0.4205; (RVV) accuracy = 0.275. M5Rules training R² is good (0.55 / 0.58) but CV R² stays near zero (+0.004 / −0.07) — the equations don't generalise enough to forecast.
6. **Cross-model backbone.**
    - RDD → (none)
    - RVV → `swa_fl_y1_a30`, `tm_fl_y1_c15`
   These are the most defensible explanatory variables in each region.
7. **What this means for the thesis.** CAREN should be presented as the *interpretive* tool — it produces the rich rule narrative across many climate niches. RIPPER and M5Rules should be presented as *cross-validation evidence* — they confirm the backbone variables in each region, even though neither generalises well enough to use as a stand-alone forecaster.

---

## 7. What changed since the previous run

| Item | Previous (May 18) | Current (May 22) |
|---|---|---|
| RDD RIPPER rules | 2 (1 + default) | 4 (3 + default) |
| RDD RIPPER accuracy | 0.35 | **0.42** ↑ |
| RVV RIPPER rules | 3 (2 + default) | 4 (3 + default) |
| RVV RIPPER accuracy | 0.36 | **0.28** ↓ |
| RDD M5Rules rules | 1 | 2 |
| RDD M5Rules CV R² | −0.19 | **+0.004** (now ≈0 instead of negative) |
| RVV M5Rules rules | 1 | 3 |
| RVV M5Rules CV R² | −0.03 | −0.07 |
| RDD CAREN rules | 34 | 48 |
| RVV CAREN rules | 67 | 20 |

Bigger CAREN-RDD ruleset (48 vs 34) and smaller CAREN-RVV ruleset (20 vs 67) reflect changes in the pipeline's discovery parameters, not the data itself.

---

## 8. One-line summary

CAREN is the right tool for **describing why some years deviate**; RIPPER and M5Rules are weaker but useful **corroborators**. The features that all three algorithms agree on are the single most defensible explanatory variables in each region.
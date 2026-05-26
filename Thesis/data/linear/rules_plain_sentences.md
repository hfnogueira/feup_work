# Wine production rules — plain-language translation

All current rules in `data/linear/` (pipeline run 2026-05-22) translated to one-sentence summaries.

Target: wine production residuals after a linear trend, in mhl. Training years: 88 (RDD), 80 (RVV).

Sources:
- `ripper_interpreted_*.txt` — RIPPER (JRip) classification rules
- `m5rules_rules_*.txt` — M5Rules regression equations
- `rules_interpreted_*.txt` — CarenR distribution rules

---

## Feature decoder

`metric_stage_yX_window`

| Code | Meaning |
|---|---|
| `tm` | Mean temperature |
| `tn / ti` | Minimum temperature |
| `tx` | Maximum temperature |
| `tm.days.less.15` | Count of days with mean temp < 15 °C |
| `days.minTemp.less.0` | Count of frost days (min temp < 0 °C) |
| `days.maxTemp.above.35 / days.tempMax.above.35` | Count of hot days (max temp > 35 °C) |
| `rf` | Rainfall (sum, mm) |
| `days.rf.above.1mm` | Count of wet days (>1 mm rain) |
| `iaf` | Leaf Area Index (Índice de Área Foliar) |
| `swa` | Available soil water (average) |
| `sw.less.15wp` | Count of dry-stress occurrences (soil water < 1.5×Wp) |
| `sw.above.09fc` | Count of waterlogging occurrences (soil water > 0.9×Fc) |

Stage: `bb`=Bud Break, `fl`=Flowering, `sm`=Start of Maturity, `hv`=Harvest. Year: `y0`=previous, `y1`=current. Window: `bN`=N days before, `aN`=N days after, `cN`=N-day window centred on the stage.

---

## RIPPER classification rules

### RDD (Douro) — accuracy 0.42, macro-F1 0.40 — 4 rules (ordered list, first match wins)

**Rule 1** → wine production is **MEDIUM** (support 21.3%, precision 73.7%, covers 19 yrs, 5 wrong)

In years when:
- mean temperature in a 10-day window centred on Harvest (current year) is at least 21.168182
- mean temperature in a 10-day window centred on Harvest (current year) is at most 22.804545

**Rule 2** → wine production is **HIGH** (support 19.1%, precision 76.5%, covers 17 yrs, 4 wrong)

In years when:
- Leaf Area Index in a 10-day window centred on Bud Break (current year) is at least 0.001565
- mean temperature in the 20 days before Harvest (current year) is at least 23.1025

**Rule 3** → wine production is **HIGH** (support 6.7%, precision 100.0%, covers 6 yrs, 0 wrong)

In years when:
- count of days with mean temperature below 15 °C in the 15 days before Flowering (current year) is at most 3
- mean temperature in the 20 days before Harvest (current year) is at most 21.2975

**Rule 4** (default): wine production is **LOW** for all years not matched above.
_Covers 46 years; 20 misclassified._


### RVV (Vinho Verde) — accuracy 0.28, macro-F1 0.26 — 4 rules (ordered list, first match wins)

**Rule 1** → wine production is **MEDIUM** (support 16.2%, precision 92.3%, covers 13 yrs, 1 wrong)

In years when:
- Leaf Area Index in the 10 days after Flowering (previous year) is at least 2.40493
- mean temperature in the 30 days after Bud Break (previous year) is at least 13.341667
- rainfall (mm) in the 10 days after Flowering (current year) is at most 15.9

**Rule 2** → wine production is **MEDIUM** (support 11.2%, precision 88.9%, covers 9 yrs, 1 wrong)

In years when:
- mean temperature in the 10 days before Flowering (current year) is at least 19.26
- Leaf Area Index in the 10 days before Flowering (current year) is at least 2.203642

**Rule 3** → wine production is **HIGH** (support 26.2%, precision 85.7%, covers 21 yrs, 3 wrong)

In years when:
- available soil water (mm) in the 30 days after Flowering (current year) is at most 123.564982
- mean temperature in a 15-day window centred on Flowering (current year) is at least 16.606667

**Rule 4** (default): wine production is **LOW** for all years not matched above.
_Covers 37 years; 12 misclassified._

---

## M5Rules regression

Each rule applies to a region of feature space (an IF clause) and then predicts the residual with a linear equation; the *sign* of each coefficient tells you the direction of effect. M5 reports both training R² and 10-fold CV R²; **CV R² is the honest out-of-sample metric**.

### RDD (Douro)

Training MAE: **125 mhl**, RMSE: 160.19, R²: **0.552**; 10-fold CV R²: **0.004**

**Rule 1** — applies when:
- mean temperature in the 10 days before Flowering (previous year) > 16.03
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) ≤ 3.5

Covers 19 years (~44.82% of training).

`Wine_mhl = -1023.6146`
- **+23.4341 × tm_fl_y0_b10** — higher mean temperature in the 10 days before Flowering (previous year) → *more* wine
- **-52.1672 × tm_bb_y0_a30** — higher mean temperature in the 30 days after Bud Break (previous year) → *less* wine
- **-9.1835 × tm_sm_y0_a20** — higher mean temperature in the 20 days after Start of Maturity (previous year) → *less* wine
- **+8.8029 × tm_hv_y1_c10** — higher mean temperature in a 10-day window centred on Harvest (current year) → *more* wine
- **+7.3148 × tn_hv_y0_a30** — higher minimum temperature in the 30 days after Harvest (previous year) → *more* wine
- **-3.4465 × tm.days.less.15_fl_y1_b15** — higher count of days with mean temperature below 15 °C in the 15 days before Flowering (current year) → *less* wine
- **+0.8769 × rf_hv_y0_a10** — higher rainfall (mm) in the 10 days after Harvest (previous year) → *more* wine
- **+7.6834 × days.rf.above.1mm_sm_y1_b20** — higher count of wet days (>1 mm rain) in the 20 days before Start of Maturity (current year) → *more* wine
- **-32.6483 × days.rf.above.1mm_hv_y0_a10** — higher count of wet days (>1 mm rain) in the 10 days after Harvest (previous year) → *less* wine
- **+675.752 × iaf_fl_y1_b10** — higher Leaf Area Index in the 10 days before Flowering (current year) → *more* wine
- **+285.8541 × iaf_fl_y1_b30** — higher Leaf Area Index in the 30 days before Flowering (current year) → *more* wine
- **+2319.9926 × iaf_fl_y0_b20** — higher Leaf Area Index in the 20 days before Flowering (previous year) → *more* wine
- **-2474.5516 × iaf_fl_y0_b30** — higher Leaf Area Index in the 30 days before Flowering (previous year) → *less* wine
- **-0.4354 × swa_hv_y1_b20** — higher available soil water (mm) in the 20 days before Harvest (current year) → *less* wine

**Rule 2** (default — applies to all remaining years)

Covers 69 years (~72.204% of training).

`Wine_mhl = -408.0874`
- **-97.628 × tm_bb_y0_a30** — higher mean temperature in the 30 days after Bud Break (previous year) → *less* wine
- **-38.5402 × tm_sm_y0_a20** — higher mean temperature in the 20 days after Start of Maturity (previous year) → *less* wine
- **+39.6208 × tn_hv_y0_a30** — higher minimum temperature in the 30 days after Harvest (previous year) → *more* wine
- **+1394.7275 × iaf_fl_y1_b20** — higher Leaf Area Index in the 20 days before Flowering (current year) → *more* wine
- **+4138.6018 × iaf_fl_y0_b10** — higher Leaf Area Index in the 10 days before Flowering (previous year) → *more* wine
- **-4199.4292 × iaf_fl_y0_b20** — higher Leaf Area Index in the 20 days before Flowering (previous year) → *less* wine
- **-2.3042 × swa_hv_y1_b20** — higher available soil water (mm) in the 20 days before Harvest (current year) → *less* wine


### RVV (Vinho Verde)

Training MAE: **269.23 mhl**, RMSE: 354.8, R²: **0.575**; 10-fold CV R²: **-0.07**

**Rule 1** — applies when:
- available soil water (mm) in the 15 days after Flowering (current year) > 131.364

Covers 41 years (~67.641% of training).

`Wine_mhl = +2419.7711`
- **-13.2577 × tm_fl_y0_b10** — higher mean temperature in the 10 days before Flowering (previous year) → *less* wine
- **-40.7199 × tm_fl_y0_a10** — higher mean temperature in the 10 days after Flowering (previous year) → *less* wine
- **+17.1571 × tm_sm_y0_a20** — higher mean temperature in the 20 days after Start of Maturity (previous year) → *more* wine
- **-24.8711 × tm_hv_y1_b20** — higher mean temperature in the 20 days before Harvest (current year) → *less* wine
- **-2063.7854 × iaf_bb_y1_c10** — higher Leaf Area Index in a 10-day window centred on Bud Break (current year) → *less* wine
- **-8.1184 × swa_fl_y1_a30** — higher available soil water (mm) in the 30 days after Flowering (current year) → *less* wine

**Rule 2** — applies when:
- mean temperature in the 20 days before Harvest (current year) > 16.764

Covers 31 years (~73.261% of training).

`Wine_mhl = +227.8692`
- **-24.0915 × tm_fl_y0_b10** — higher mean temperature in the 10 days before Flowering (previous year) → *less* wine
- **+46.6995 × tm_sm_y0_a20** — higher mean temperature in the 20 days after Start of Maturity (previous year) → *more* wine
- **-49.5548 × tm_hv_y1_b20** — higher mean temperature in the 20 days before Harvest (current year) → *less* wine
- **+5.1021 × rf_hv_y0_a10** — higher rainfall (mm) in the 10 days after Harvest (previous year) → *more* wine
- **-1.7369 × swa_hv_y1_b20** — higher available soil water (mm) in the 20 days before Harvest (current year) → *less* wine

**Rule 3** (default — applies to all remaining years)

Covers 8 years (~59.917% of training).

`Wine_mhl = +3455.2925`
- **-151.6016 × tm_fl_y0_b10** — higher mean temperature in the 10 days before Flowering (previous year) → *less* wine

---

## CAREN distribution rules — plain sentences

*Sorted by importance score = support × −log₁₀(p-value). Higher score → more important rule (larger sub-population and stronger significance).*

### RDD (Douro) — 48 significant rules

Rules sorted by importance score = `support × −log₁₀(p-value)` (higher = more important — bigger sub-population *and* lower p-value).

**Rule 1** (was Rule 1) ↑ +121.23 mhl — support 28.1% (≈25 yrs), p = 0.0043, score = 66.50

In years when:
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 0 and 3.33
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **121.23 mhl above the long-term trend**.

**Rule 2** (was Rule 7) ↓ -131.85 mhl — support 32.6% (≈29 yrs), p = 0.0134, score = 61.06

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **131.85 mhl below the long-term trend**.

**Rule 3** (was Rule 8) ↓ -131.85 mhl — support 32.6% (≈29 yrs), p = 0.0134, score = 61.06

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29

…wine production tends to be **131.85 mhl below the long-term trend**.

**Rule 4** (was Rule 13) ↑ +99.0 mhl — support 33.7% (≈30 yrs), p = 0.0174, score = 59.29

In years when:
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 0 and 3.33

…wine production tends to be **99.0 mhl above the long-term trend**.

**Rule 5** (was Rule 23) ↓ -116.75 mhl — support 33.7% (≈30 yrs), p = 0.0244, score = 54.34

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231

…wine production tends to be **116.75 mhl below the long-term trend**.

**Rule 6** (was Rule 3) ↑ +112.78 mhl — support 25.8% (≈23 yrs), p = 0.0089, score = 52.91

In years when:
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 0 and 3.33
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **112.78 mhl above the long-term trend**.

**Rule 7** (was Rule 14) ↑ +99.47 mhl — support 30.3% (≈27 yrs), p = 0.0199, score = 51.54

In years when:
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 0 and 3.33
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **99.47 mhl above the long-term trend**.

**Rule 8** (was Rule 11) ↑ +118.8 mhl — support 28.1% (≈25 yrs), p = 0.0158, score = 50.62

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **118.8 mhl above the long-term trend**.

**Rule 9** (was Rule 16) ↓ -120.27 mhl — support 30.3% (≈27 yrs), p = 0.0225, score = 49.93

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **120.27 mhl below the long-term trend**.

**Rule 10** (was Rule 17) ↓ -120.27 mhl — support 30.3% (≈27 yrs), p = 0.0225, score = 49.93

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29

…wine production tends to be **120.27 mhl below the long-term trend**.

**Rule 11** (was Rule 5) ↑ +114.57 mhl — support 24.7% (≈22 yrs), p = 0.011, score = 48.38

In years when:
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 0 and 3.33
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66

…wine production tends to be **114.57 mhl above the long-term trend**.

**Rule 12** (was Rule 32) ↑ +120.7 mhl — support 32.6% (≈29 yrs), p = 0.0354, score = 47.30

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **120.7 mhl above the long-term trend**.

**Rule 13** (was Rule 36) ↓ -104.51 mhl — support 31.5% (≈28 yrs), p = 0.0406, score = 43.83

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66

…wine production tends to be **104.51 mhl below the long-term trend**.

**Rule 14** (was Rule 4) ↑ +192.38 mhl — support 21.3% (≈19 yrs), p = 0.0095, score = 43.07

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of wet days (>1 mm rain) in the 10 days after Harvest (previous year) is between 0 and 2.27
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **192.38 mhl above the long-term trend**.

**Rule 15** (was Rule 18) ↓ -144.17 mhl — support 25.8% (≈23 yrs), p = 0.0227, score = 42.41

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- Leaf Area Index in a 10-day window centred on Bud Break (current year) is between 0 and 0.00167
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **144.17 mhl below the long-term trend**.

**Rule 16** (was Rule 19) ↓ -144.17 mhl — support 25.8% (≈23 yrs), p = 0.0227, score = 42.41

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- Leaf Area Index in a 10-day window centred on Bud Break (current year) is between 0 and 0.00167
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29

…wine production tends to be **144.17 mhl below the long-term trend**.

**Rule 17** (was Rule 2) ↓ -131.27 mhl — support 20.2% (≈18 yrs), p = 0.0084, score = 41.93

In years when:
- Leaf Area Index in the 10 days after Flowering (current year) is between 0.739 and 0.878
- available soil water (mm) in the 30 days after Flowering (current year) is between 390 and 404

…wine production tends to be **131.27 mhl below the long-term trend**.

**Rule 18** (was Rule 6) ↑ +118.92 mhl — support 21.3% (≈19 yrs), p = 0.0114, score = 41.39

In years when:
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 0 and 3.33
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **118.92 mhl above the long-term trend**.

**Rule 19** (was Rule 10) ↑ +123.16 mhl — support 22.5% (≈20 yrs), p = 0.0157, score = 40.59

In years when:
- mean temperature in the 10 days before Flowering (previous year) is between 16 and 18
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **123.16 mhl above the long-term trend**.

**Rule 20** (was Rule 25) ↑ +142.16 mhl — support 25.8% (≈23 yrs), p = 0.029, score = 39.67

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29

…wine production tends to be **142.16 mhl above the long-term trend**.

**Rule 21** (was Rule 26) ↑ +98.26 mhl — support 25.8% (≈23 yrs), p = 0.029, score = 39.67

In years when:
- mean temperature in the 10 days before Flowering (previous year) is between 16 and 18
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **98.26 mhl above the long-term trend**.

**Rule 22** (was Rule 12) ↑ +175.13 mhl — support 22.5% (≈20 yrs), p = 0.0173, score = 39.64

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of wet days (>1 mm rain) in the 10 days after Harvest (previous year) is between 0 and 2.27

…wine production tends to be **175.13 mhl above the long-term trend**.

**Rule 23** (was Rule 20) ↑ +128.04 mhl — support 23.6% (≈21 yrs), p = 0.0228, score = 38.75

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **128.04 mhl above the long-term trend**.

**Rule 24** (was Rule 22) ↑ +104.19 mhl — support 22.5% (≈20 yrs), p = 0.0233, score = 36.73

In years when:
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 0 and 3.33
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **104.19 mhl above the long-term trend**.

**Rule 25** (was Rule 9) ↑ +136.76 mhl — support 20.2% (≈18 yrs), p = 0.0156, score = 36.50

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of wet days (>1 mm rain) in a 10-day window centred on Harvest (current year) is between 0 and 2.6
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **136.76 mhl above the long-term trend**.

**Rule 26** (was Rule 30) ↓ -110.06 mhl — support 24.7% (≈22 yrs), p = 0.0335, score = 36.43

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **110.06 mhl below the long-term trend**.

**Rule 27** (was Rule 31) ↓ -110.06 mhl — support 24.7% (≈22 yrs), p = 0.0335, score = 36.43

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29

…wine production tends to be **110.06 mhl below the long-term trend**.

**Rule 28** (was Rule 27) ↑ +115.03 mhl — support 23.6% (≈21 yrs), p = 0.0316, score = 35.41

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66

…wine production tends to be **115.03 mhl above the long-term trend**.

**Rule 29** (was Rule 21) ↑ +102.09 mhl — support 21.3% (≈19 yrs), p = 0.0231, score = 34.86

In years when:
- available soil water (mm) in the 30 days after Flowering (current year) is between 355 and 376
- count of wet days (>1 mm rain) in a 10-day window centred on Harvest (current year) is between 0 and 2.6
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **102.09 mhl above the long-term trend**.

**Rule 30** (was Rule 15) ↑ +114.23 mhl — support 20.2% (≈18 yrs), p = 0.0199, score = 34.36

In years when:
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 0 and 3.33
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **114.23 mhl above the long-term trend**.

**Rule 31** (was Rule 42) ↑ +77.76 mhl — support 25.8% (≈23 yrs), p = 0.0479, score = 34.05

In years when:
- mean temperature in the 10 days before Flowering (previous year) is between 16 and 18
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **77.76 mhl above the long-term trend**.

**Rule 32** (was Rule 24) ↑ +125.25 mhl — support 21.3% (≈19 yrs), p = 0.0254, score = 33.98

In years when:
- mean temperature in the 10 days before Flowering (previous year) is between 16 and 18
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643
- count of frost days (min temp < 0 °C) in a 20-day window centred on Bud Break (current year) is between 0 and 0.5

…wine production tends to be **125.25 mhl above the long-term trend**.

**Rule 33** (was Rule 39) ↓ -116.47 mhl — support 24.7% (≈22 yrs), p = 0.043, score = 33.75

In years when:
- mean temperature in the 10 days before Flowering (previous year) is between 14.3 and 16
- Leaf Area Index in a 10-day window centred on Bud Break (current year) is between 0 and 0.00167

…wine production tends to be **116.47 mhl below the long-term trend**.

**Rule 34** (was Rule 40) ↑ +97.64 mhl — support 24.7% (≈22 yrs), p = 0.044, score = 33.51

In years when:
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 0 and 3.33
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29

…wine production tends to be **97.64 mhl above the long-term trend**.

**Rule 35** (was Rule 41) ↑ +98.93 mhl — support 24.7% (≈22 yrs), p = 0.044, score = 33.51

In years when:
- mean temperature in the 10 days before Flowering (previous year) is between 16 and 18
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643
- count of frost days (min temp < 0 °C) in a 20-day window centred on Bud Break (current year) is between 0 and 0.5

…wine production tends to be **98.93 mhl above the long-term trend**.

**Rule 36** (was Rule 37) ↑ +109.39 mhl — support 23.6% (≈21 yrs), p = 0.0407, score = 32.81

In years when:
- mean temperature in the 10 days before Flowering (previous year) is between 16 and 18
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **109.39 mhl above the long-term trend**.

**Rule 37** (was Rule 34) ↑ +135.26 mhl — support 22.5% (≈20 yrs), p = 0.0392, score = 31.65

In years when:
- count of wet days (>1 mm rain) in the 20 days before Start of Maturity (current year) is between 0 and 2
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **135.26 mhl above the long-term trend**.

**Rule 38** (was Rule 35) ↑ +146.03 mhl — support 22.5% (≈20 yrs), p = 0.0392, score = 31.65

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66

…wine production tends to be **146.03 mhl above the long-term trend**.

**Rule 39** (was Rule 33) ↑ +121.53 mhl — support 21.3% (≈19 yrs), p = 0.0354, score = 30.91

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of wet days (>1 mm rain) in a 10-day window centred on Harvest (current year) is between 0 and 2.6

…wine production tends to be **121.53 mhl above the long-term trend**.

**Rule 40** (was Rule 44) ↓ -131.03 mhl — support 23.6% (≈21 yrs), p = 0.0492, score = 30.87

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of wet days (>1 mm rain) in the 20 days after Flowering (current year) is between 7 and 14

…wine production tends to be **131.03 mhl below the long-term trend**.

**Rule 41** (was Rule 45) ↓ -95.91 mhl — support 23.6% (≈21 yrs), p = 0.0492, score = 30.87

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66

…wine production tends to be **95.91 mhl below the long-term trend**.

**Rule 42** (was Rule 46) ↓ -95.91 mhl — support 23.6% (≈21 yrs), p = 0.0492, score = 30.87

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66

…wine production tends to be **95.91 mhl below the long-term trend**.

**Rule 43** (was Rule 47) ↓ -130.45 mhl — support 23.6% (≈21 yrs), p = 0.0492, score = 30.87

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- Leaf Area Index in a 10-day window centred on Bud Break (current year) is between 0 and 0.00167
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29

…wine production tends to be **130.45 mhl below the long-term trend**.

**Rule 44** (was Rule 48) ↓ -130.45 mhl — support 23.6% (≈21 yrs), p = 0.0492, score = 30.87

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 141 and 231
- Leaf Area Index in a 10-day window centred on Bud Break (current year) is between 0 and 0.00167
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **130.45 mhl below the long-term trend**.

**Rule 45** (was Rule 28) ↑ +130.0 mhl — support 20.2% (≈18 yrs), p = 0.0323, score = 30.11

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 74.2 and 101
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 3.66
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 0.643

…wine production tends to be **130.0 mhl above the long-term trend**.

**Rule 46** (was Rule 29) ↑ +113.86 mhl — support 20.2% (≈18 yrs), p = 0.0332, score = 29.87

In years when:
- available soil water (mm) in the 15 days after Flowering (current year) is between 360 and 380
- count of wet days (>1 mm rain) in a 10-day window centred on Harvest (current year) is between 0 and 2.6
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **113.86 mhl above the long-term trend**.

**Rule 47** (was Rule 43) ↑ +104.69 mhl — support 21.3% (≈19 yrs), p = 0.0487, score = 27.96

In years when:
- available soil water (mm) in the 30 days after Flowering (current year) is between 355 and 376
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.29
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **104.69 mhl above the long-term trend**.

**Rule 48** (was Rule 38) ↑ +108.23 mhl — support 20.2% (≈18 yrs), p = 0.042, score = 27.81

In years when:
- Leaf Area Index in the 20 days before Flowering (previous year) is between 0.474 and 0.517
- Leaf Area Index in the 30 days before Flowering (previous year) is between 0.356 and 0.412
- count of wet days (>1 mm rain) in the 20 days after Start of Maturity (current year) is between 0 and 3.46

…wine production tends to be **108.23 mhl above the long-term trend**.


### RVV (Vinho Verde) — 20 significant rules

Rules sorted by importance score = `support × −log₁₀(p-value)` (higher = more important — bigger sub-population *and* lower p-value).

**Rule 1** (was Rule 3) ↓ -365.44 mhl — support 27.5% (≈22 yrs), p = 0.0294, score = 42.12

In years when:
- available soil water (mm) in the 15 days after Flowering (current year) is between 143 and 234
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37

…wine production tends to be **365.44 mhl below the long-term trend**.

**Rule 2** (was Rule 4) ↑ +310.84 mhl — support 25.0% (≈20 yrs), p = 0.0298, score = 38.14

In years when:
- mean temperature in the 30 days after Bud Break (previous year) is between 11.2 and 13.5
- count of days with mean temperature below 15 °C in a 15-day window centred on Flowering (current year) is between 0 and 3.4
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.14

…wine production tends to be **310.84 mhl above the long-term trend**.

**Rule 3** (was Rule 6) ↓ -386.66 mhl — support 26.2% (≈21 yrs), p = 0.0356, score = 37.95

In years when:
- rainfall (mm) in the 10 days after Flowering (current year) is between 20.7 and 121
- Leaf Area Index in a 10-day window centred on Harvest (current year) is between 1.29 and 2.52
- count of hot days (max temp > 35 °C) in the 10 days after Flowering (current year) is between 0 and 1.26

…wine production tends to be **386.66 mhl below the long-term trend**.

**Rule 4** (was Rule 7) ↓ -386.66 mhl — support 26.2% (≈21 yrs), p = 0.0356, score = 37.95

In years when:
- rainfall (mm) in the 10 days after Flowering (current year) is between 20.7 and 121
- Leaf Area Index in a 10-day window centred on Harvest (current year) is between 1.29 and 2.52
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.14

…wine production tends to be **386.66 mhl below the long-term trend**.

**Rule 5** (was Rule 5) ↓ -292.43 mhl — support 23.8% (≈19 yrs), p = 0.0299, score = 36.28

In years when:
- mean temperature in the 10 days after Flowering (current year) is between 12.6 and 17.5
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37
- count of waterlogging occurrences (soil water > 0.9×Fc) in the 20 days after Flowering (previous year) is between 17.5 and 20

…wine production tends to be **292.43 mhl below the long-term trend**.

**Rule 6** (was Rule 1) ↓ -337.5 mhl — support 21.2% (≈17 yrs), p = 0.0198, score = 36.11

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 61.5 and 97
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 1.91
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.14

…wine production tends to be **337.5 mhl below the long-term trend**.

**Rule 7** (was Rule 2) ↓ -267.62 mhl — support 21.2% (≈17 yrs), p = 0.0216, score = 35.31

In years when:
- mean temperature in a 15-day window centred on Flowering (current year) is between 13.8 and 17.1
- mean temperature in the 10 days after Flowering (current year) is between 12.6 and 17.5
- count of waterlogging occurrences (soil water > 0.9×Fc) in the 20 days after Flowering (previous year) is between 17.5 and 20

…wine production tends to be **267.62 mhl below the long-term trend**.

**Rule 8** (was Rule 10) ↓ -245.5 mhl — support 25.0% (≈20 yrs), p = 0.0397, score = 35.03

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 61.5 and 97
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 1.91
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.14

…wine production tends to be **245.5 mhl below the long-term trend**.

**Rule 9** (was Rule 9) ↓ -370.86 mhl — support 23.8% (≈19 yrs), p = 0.0394, score = 33.43

In years when:
- available soil water (mm) in the 15 days after Flowering (current year) is between 143 and 234
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 1.91
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37

…wine production tends to be **370.86 mhl below the long-term trend**.

**Rule 10** (was Rule 16) ↑ +281.09 mhl — support 23.8% (≈19 yrs), p = 0.0411, score = 32.99

In years when:
- available soil water (mm) in the 15 days after Flowering (current year) is between 81.4 and 115
- count of days with mean temperature below 15 °C in a 15-day window centred on Flowering (current year) is between 0 and 3.4
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.14

…wine production tends to be **281.09 mhl above the long-term trend**.

**Rule 11** (was Rule 17) ↑ +279.11 mhl — support 23.8% (≈19 yrs), p = 0.0411, score = 32.99

In years when:
- available soil water (mm) in the 30 days after Flowering (current year) is between 77.5 and 113
- count of days with mean temperature below 15 °C in a 15-day window centred on Flowering (current year) is between 0 and 3.4
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.14

…wine production tends to be **279.11 mhl above the long-term trend**.

**Rule 12** (was Rule 8) ↓ -332.03 mhl — support 22.5% (≈18 yrs), p = 0.0357, score = 32.56

In years when:
- Leaf Area Index in the 10 days before Flowering (previous year) is between 2.16 and 2.21
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 1.91
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37

…wine production tends to be **332.03 mhl below the long-term trend**.

**Rule 13** (was Rule 19) ↓ -296.09 mhl — support 22.5% (≈18 yrs), p = 0.0464, score = 30.00

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 61.5 and 97
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 1.91
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37

…wine production tends to be **296.09 mhl below the long-term trend**.

**Rule 14** (was Rule 20) ↓ -250.05 mhl — support 22.5% (≈18 yrs), p = 0.0464, score = 30.00

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 61.5 and 97
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 1.91
- count of frost days (min temp < 0 °C) in a 20-day window centred on Bud Break (current year) is between 0 and 1.53
- count of hot days (max temp > 35 °C) in the 20 days after Flowering (current year) is between 0 and 1.14

…wine production tends to be **250.05 mhl below the long-term trend**.

**Rule 15** (was Rule 18) ↓ -336.92 mhl — support 21.2% (≈17 yrs), p = 0.0415, score = 29.30

In years when:
- Leaf Area Index in the 10 days before Flowering (previous year) is between 2.16 and 2.21
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 1.91
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37
- count of frost days (min temp < 0 °C) in a 20-day window centred on Bud Break (current year) is between 0 and 1.53

…wine production tends to be **336.92 mhl below the long-term trend**.

**Rule 16** (was Rule 11) ↑ +283.76 mhl — support 20.0% (≈16 yrs), p = 0.0403, score = 27.89

In years when:
- mean temperature in the 15 days after Bud Break (previous year) is between 9.16 and 12
- count of days with mean temperature below 15 °C in a 15-day window centred on Flowering (current year) is between 0 and 3.4
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37
- count of waterlogging occurrences (soil water > 0.9×Fc) in the 20 days after Flowering (previous year) is between 17.5 and 20
- count of frost days (min temp < 0 °C) in a 20-day window centred on Bud Break (current year) is between 0 and 1.53

…wine production tends to be **283.76 mhl above the long-term trend**.

**Rule 17** (was Rule 12) ↓ -319.37 mhl — support 20.0% (≈16 yrs), p = 0.0403, score = 27.89

In years when:
- count of wet days (>1 mm rain) in a 15-day window centred on Flowering (current year) is between 5.67 and 12
- available soil water (mm) in the 30 days after Flowering (current year) is between 140 and 227

…wine production tends to be **319.37 mhl below the long-term trend**.

**Rule 18** (was Rule 13) ↓ -389.94 mhl — support 20.0% (≈16 yrs), p = 0.0403, score = 27.89

In years when:
- available soil water (mm) in the 15 days after Flowering (current year) is between 143 and 234
- count of days with mean temperature below 15 °C in a 15-day window centred on Flowering (current year) is between 0 and 3.4
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37

…wine production tends to be **389.94 mhl below the long-term trend**.

**Rule 19** (was Rule 14) ↓ -307.54 mhl — support 20.0% (≈16 yrs), p = 0.0403, score = 27.89

In years when:
- available soil water (mm) in the 20 days before Harvest (current year) is between 61.5 and 97
- count of hot days (max temp > 35 °C) in the 20 days before Start of Maturity (current year) is between 0 and 1.91
- count of hot days (max temp > 35 °C) in the 20 days after Start of Maturity (current year) is between 0 and 2.37
- count of frost days (min temp < 0 °C) in a 20-day window centred on Bud Break (current year) is between 0 and 1.53

…wine production tends to be **307.54 mhl below the long-term trend**.

**Rule 20** (was Rule 15) ↓ -268.79 mhl — support 20.0% (≈16 yrs), p = 0.0403, score = 27.89

In years when:
- mean temperature in a 15-day window centred on Flowering (current year) is between 13.8 and 17.1
- mean temperature in the 10 days after Flowering (current year) is between 12.6 and 17.5
- count of waterlogging occurrences (soil water > 0.9×Fc) in the 20 days after Flowering (previous year) is between 17.5 and 20
- count of frost days (min temp < 0 °C) in a 20-day window centred on Bud Break (current year) is between 0 and 1.53

…wine production tends to be **268.79 mhl below the long-term trend**.

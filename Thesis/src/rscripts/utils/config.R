# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       SHARED PIPELINE CONFIGURATION
#                       by: hugonogueira
#
#  Single source of truth for parameters that must stay consistent across
#  scripts 3, 4, 5, 6, 9, run_all_pipelines, and detrend_diagnostics.
#
#  Source this near the top of each pipeline script:
#    source("src/rscripts/utils/config.R")
#
#  HARD DEFAULTS: sourcing this file always overwrites any local values.
#  To change the global detrend method (e.g. linear → lowess), edit it here
#  in ONE place and re-run.
#
#  Per-script tuning lives in each script, not here:
#    - script 4:  jrip_N, jrip_O, jrip_F, n_classes
#    - script 5:  m5_unsmoothed
#    - script 6:  min_train, step_years, carenr_min_sup, carenr_min_conf,
#                 carenr_strategy, carenr_min_subgroup, dt_cp, dt_minsplit,
#                 m5_unsmoothed, horizon_scenario
#    - script 9:  min_sup_filter, max_pval_filter
#
#  Per-RUN state (which region — variable `file`) is also NOT set here;
#  it stays in each script so you can flip RDD/RVV without editing this file.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ── Detrending ────────────────────────────────────────────────────────────
# Set to TRUE to remove the long-term production trend before modelling.
# When TRUE, rules describe DEVIATIONS from the trend (climate signal),
# not raw production levels.
detrend        <- TRUE

# Detrending method (only used when detrend = TRUE):
#   "linear"  → remove a fitted straight line
#   "lowess"  → remove a locally-weighted smooth trend (loess)
detrend_method <- "linear"

# LOWESS smoothing span (fraction of points used in each local fit).
# Only used when detrend_method = "lowess". Default 0.75 = smooth trend.
loess_span     <- 0.75


# ── Cross-validation / RNG ────────────────────────────────────────────────
# Used by RIPPER (script 4), M5Rules (script 5), run_all_pipelines.R,
# and (seed only) scenario validation (script 6).
n_folds <- 10
seed    <- 42

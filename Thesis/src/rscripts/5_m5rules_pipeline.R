# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       M5Rules Pipeline
#                       by:  hugonogueira
#
#  Applies M5Rules (WEKA's regression rule learner) to predict wine
#  production (Wine_mhl) as a continuous variable.
#
#  Unlike RIPPER (classifier), M5Rules works directly in the regression
#  space — no target discretisation needed. Each rule has a linear model
#  at its leaf, making predictions continuous and directly comparable
#  to CarenR subgroup means.
#
#  Pipeline:
#    1. Load continuous feature dataset (output of features creation_v2.R)
#    2. Train M5Rules on full dataset — compare rules with CarenR
#    3. Evaluate with k-fold cross-validation (MAE, RMSE, R²)
#    4. Compare against naive baseline (predict training mean)
#    5. Print and save discovered rules
#
#  Requires: RWeka, tidyverse
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# libs --------------------------------------------------------------------------------

library(tidyverse)

if (!requireNamespace("RWeka", quietly = TRUE)) install.packages("RWeka")
library(RWeka)


# config ------------------------------------------------------------------------------
# Shared params (detrend, detrend_method, loess_span, n_folds, seed) come from
# utils/config.R. Per-run state and per-script tuning stay below.
#
# CAVEAT on detrend: M5Rules CV is handled internally by Weka
# (evaluate_Weka_classifier), so fold-correct detrending is not possible here —
# the trend is fitted once on the full dataset. Walk-forward validation
# (script 6) does fold-correct detrending and should be used as the primary
# evaluation.

source("src/rscripts/utils/config.R")
source("src/rscripts/utils/detrend_utils.R")

# Per-run state — change to switch region
file <- 1    # 1 = RDD   |   other = RVV

# Per-script tuning
# M5Rules control options:
#   -N  use unsmoothed predictions (default: smoothed)
#   -U  use unsmoothed linear models in leaves
# Default settings (smoothed) are generally better for small datasets.
m5_unsmoothed <- FALSE   # set TRUE to disable smoothing


# load data ---------------------------------------------------------------------------

if (file == 1) {
   df     <- read.csv(file = 'data/dataPrep_cont_dataset_rdd.csv')
   region <- 'RDD'
} else {
   df     <- read.csv(file = 'data/dataPrep_cont_dataset_rvv.csv')
   region <- 'RVV'
}

cat('------------------------------------------------------------\n')
cat('Region:', region, '| Observations:', nrow(df), '| Features:', ncol(df) - 2, '\n')
cat('Global Wine_mhl — Mean:', round(mean(df$Wine_mhl), 2),
    '| SD:', round(sd(df$Wine_mhl), 2),
    '| Min:', round(min(df$Wine_mhl), 2),
    '| Max:', round(max(df$Wine_mhl), 2), '\n')
cat('Detrend:', detrend, if (detrend) paste0('(', detrend_method, ')') else '', '\n')
cat('------------------------------------------------------------\n')


# detrend (optional) ------------------------------------------------------------------
# Full-dataset detrend — see CAVEAT in config above regarding Weka CV leakage.

if (detrend) {

  cat('\n--- Detrending enabled (method:', detrend_method, ') ---\n')
  tr             <- get_trend_residuals(df, method = detrend_method,
                                        span = loess_span, verbose = TRUE)
  df$Wine_mhl_original <- df$Wine_mhl    # keep raw for reference
  df$Wine_mhl          <- tr$residuals   # replace with residuals

  cat('Wine_mhl now contains RESIDUALS (deviation from', detrend_method, 'trend).\n')
  cat('Residual mean:', round(mean(df$Wine_mhl), 2),
      '| SD:', round(sd(df$Wine_mhl), 2), '\n\n')

} else {

  cat('\n--- Detrending disabled — using raw Wine_mhl ---\n\n')

}


# prepare feature matrix --------------------------------------------------------------
# Wine_mhl stays CONTINUOUS — no discretisation needed

df_m5 <- df %>% select(-year, -any_of("Wine_mhl_original"))  # exclude raw target — data leakage

cat('\nDimensions going into M5Rules:', nrow(df_m5), 'rows x', ncol(df_m5) - 1, 'features\n')


# naive baseline ----------------------------------------------------------------------
# Always predicting the training mean — the floor any model must beat

naive_pred   <- mean(df_m5$Wine_mhl)
naive_errors <- df_m5$Wine_mhl - naive_pred
naive_mae    <- mean(abs(naive_errors))
naive_rmse   <- sqrt(mean(naive_errors^2))
naive_r2     <- 0   # by definition, predicting the mean gives R² = 0

cat('\n=== Naive Baseline (always predict training mean) ===\n')
cat('Predicted value:', round(naive_pred, 2), 'mhl\n')
cat('MAE :', round(naive_mae,  2), 'mhl\n')
cat('RMSE:', round(naive_rmse, 2), 'mhl\n')
cat('R²  : 0.000 (by definition)\n')


# train M5Rules -----------------------------------------------------------------------

cat('\n============================================================\n')
cat(' Training M5Rules on full dataset ...\n')
cat('============================================================\n')

set.seed(seed)

ctrl <- if (m5_unsmoothed) Weka_control(N = TRUE) else Weka_control()

model <- M5Rules(Wine_mhl ~ ., data = df_m5, control = ctrl)

cat('\n=== Discovered Rules ===\n')
print(model)


# training set performance ------------------------------------------------------------

train_pred   <- predict(model, df_m5)
train_errors <- df_m5$Wine_mhl - train_pred
train_mae    <- mean(abs(train_errors))
train_rmse   <- sqrt(mean(train_errors^2))
ss_res       <- sum(train_errors^2)
ss_tot       <- sum((df_m5$Wine_mhl - mean(df_m5$Wine_mhl))^2)
train_r2     <- 1 - ss_res / ss_tot

cat('\n=== Training Set Performance ===\n')
cat('MAE :', round(train_mae,  2), 'mhl\n')
cat('RMSE:', round(train_rmse, 2), 'mhl\n')
cat('R²  :', round(train_r2,   3), '\n')


# cross-validation evaluation ---------------------------------------------------------

cat('\n============================================================\n')
cat(' ', n_folds, '-fold Cross Validation\n')
cat('============================================================\n')

eval_cv <- evaluate_Weka_classifier(
   model,
   numFolds   = n_folds,
   complexity = FALSE,
   seed       = seed
)

print(eval_cv)

# extract CV metrics manually
cv_mae  <- eval_cv$details['meanAbsoluteError']
cv_rmse <- eval_cv$details['rootMeanSquaredError']
cv_rae  <- eval_cv$details['relativeAbsoluteError']
cv_rse  <- eval_cv$details['rootRelativeSquaredError']

# R² from relative squared error: R² = 1 - (RRSE²)
cv_r2 <- 1 - (cv_rse / 100)^2

cat('\n=== Cross-Validation Summary ===\n')
cat('MAE :', round(cv_mae,  2), 'mhl\n')
cat('RMSE:', round(cv_rmse, 2), 'mhl\n')
cat('R²  :', round(cv_r2,   3), '\n')

cat('\n=== vs Naive Baseline ===\n')
cat('MAE  improvement over baseline:', round((1 - cv_mae  / naive_mae)  * 100, 1), '%\n')
cat('RMSE improvement over baseline:', round((1 - cv_rmse / naive_rmse) * 100, 1), '%\n')


# per-rule coverage -------------------------------------------------------------------

cat('\n=== Rule Coverage on Training Data ===\n')
cat('(shows how many years each rule fires on the full dataset)\n\n')

# get predictions and residuals per observation
df_m5$predicted  <- train_pred
df_m5$residual   <- train_errors
df_m5$abs_error  <- abs(train_errors)

cat('Overall training predictions:\n')
cat('Mean predicted:', round(mean(train_pred), 2), 'mhl\n')
cat('Mean actual:   ', round(mean(df_m5$Wine_mhl), 2), 'mhl\n')
cat('Correlation (predicted vs actual):', round(cor(df_m5$Wine_mhl, train_pred), 3), '\n')


# save rules + metrics ----------------------------------------------------------------
# Output goes to data/{detrend_method}/ so both runs coexist without overwriting.
# Note: CV metrics are in RESIDUAL units when detrend=TRUE (not absolute mhl).
# Use walk-forward results (script 6) for mhl-unit comparisons.

out_dir <- file.path('data', if (detrend) detrend_method else 'no_detrend')
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

rules_file <- file.path(out_dir, paste0('m5rules_rules_', tolower(region), '.txt'))

sink(rules_file)
cat('M5Rules —', region, '\n')
cat('Generated     :', format(Sys.time(), '%Y-%m-%d %H:%M'), '\n')
cat('Observations  :', nrow(df_m5), '\n')
cat('Detrend method:', if (detrend) detrend_method else 'none', '\n')
cat('Target        : Wine_mhl', if (detrend) '(residuals)' else '(raw, mhl)', '\n\n')

cat('=== Naive Baseline ===\n')
cat('MAE:', round(naive_mae, 2), '| RMSE:', round(naive_rmse, 2), '\n\n')

cat('=== Discovered Rules ===\n')
print(model)

cat('\n=== Training Performance ===\n')
cat('MAE:', round(train_mae, 2), '| RMSE:', round(train_rmse, 2), '| R2:', round(train_r2, 3), '\n')

cat('\n=== Cross-Validation (', n_folds, 'folds) ===\n')
print(eval_cv)
cat('R2 (CV):', round(cv_r2, 3), '\n')
sink()

# Save numeric metrics as CSV for comparison in script 8
metrics_csv <- data.frame(
  region         = region,
  detrend_method = if (detrend) detrend_method else 'none',
  model          = 'M5Rules',
  cv_mae         = round(cv_mae,  4),
  cv_rmse        = round(cv_rmse, 4),
  cv_r2          = round(cv_r2,   4),
  note           = if (detrend) 'metrics in residual units (not mhl)' else 'metrics in mhl'
)
write.csv(metrics_csv,
          file.path(out_dir, paste0('m5rules_metrics_', tolower(region), '.csv')),
          row.names = FALSE)

cat('\nOutput saved to:', out_dir, '\n')

# --- Structured rules CSV (read by script 9 rule interpreter) ----------------
# Parses the M5Rules print output into one row per rule.
# Block structure: "LM num: N" header, then condition lines, then "Wine_mhl = ..."
# Coverage summary: "LM1 (n_instances/error%)" appears in the preamble.

m5_raw      <- capture.output(print(model))
block_idx   <- grep("^(LM num:|Rule:)\\s*[0-9]+", m5_raw)

if (length(block_idx) > 0) {

  structured_m5 <- bind_rows(lapply(seq_along(block_idx), function(i) {

    blk_start <- block_idx[i] + 1
    blk_end   <- if (i < length(block_idx)) block_idx[i + 1] - 1 else length(m5_raw)
    block     <- m5_raw[blk_start:blk_end]
    block     <- block[nchar(trimws(block)) > 0]    # drop blank lines

    rule_idx  <- as.integer(gsub("[^0-9]", "", m5_raw[block_idx[i]]))

    # Locate "Wine_mhl =" — separates conditions (above) from linear model (below)
    target_line <- grep("^\\s*Wine_mhl\\s*=", block)[1]

    if (!is.na(target_line)) {
      cond_lines <- if (target_line > 1) trimws(block[1:(target_line - 1)]) else character(0)
      cond_lines <- cond_lines[nchar(cond_lines) > 0]
      conditions_raw   <- if (length(cond_lines) > 0) paste(cond_lines, collapse = " AND ") else "(default)"
      linear_model_raw <- paste(trimws(block[target_line:length(block)]), collapse = " ")
    } else {
      conditions_raw   <- "(default)"
      linear_model_raw <- paste(trimws(block), collapse = " ")
    }

    # Coverage: "LM{N} (n_instances/err%)" in the preamble block at the top
    lm_header <- grep(paste0("^LM", rule_idx, "\\s*\\("), m5_raw, value = TRUE)
    n_inst <- NA_real_; err_pct <- NA_real_
    if (length(lm_header) > 0) {
      hm <- regmatches(lm_header[1],
              regexec("\\(([0-9]+)/([0-9.]+)%?\\)", lm_header[1]))[[1]]
      if (length(hm) >= 3) {
        n_inst  <- as.numeric(hm[2])
        err_pct <- as.numeric(hm[3])
      }
    }

    data.frame(
      rule_index       = rule_idx,
      conditions_raw   = conditions_raw,
      linear_model_raw = linear_model_raw,
      n_instances      = n_inst,
      error_pct        = err_pct,
      target_detrended = detrend,
      region           = region,
      detrend_method   = if (detrend) detrend_method else 'none',
      stringsAsFactors = FALSE
    )
  }))

  struct_file <- file.path(out_dir, paste0('m5rules_rules_structured_', tolower(region), '.csv'))
  write.csv(structured_m5, struct_file, row.names = FALSE)
  cat('Saved structured M5Rules (for script 9):', struct_file, '\n')

} else {
  cat('[!] No LM num blocks found in M5Rules output — structured CSV not saved.\n')
}

cat('Done.\n')

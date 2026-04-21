# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       carenR Classification Pipeline
#                       by:  hugonogueira
#
#  Applies carenR in classification/prediction mode (prm=TRUE) to predict
#  wine production class (Low / Medium / High).
#
#  This pipeline is designed to be directly comparable with RIPPER
#  (4_ripper_pipeline.R). Keep config flags identical between scripts:
#    - Same detrend flag  → same target definition
#    - Same n_classes, n_folds, seed, class_labels  → identical CV scheme
#    - Same evaluation metrics: confusion matrix, per-class P/R/F1, accuracy
#
#  Detrending note:
#    When detrend = TRUE, Wine_mhl residuals (deviation from linear trend)
#    are classified instead of raw values.  This makes Low/Medium/High mean
#    "below / around / above what was expected for that era", which is
#    consistent with the descriptive carenR analysis (3_carenR__application.R)
#    and removes era-level bias (early decades are not structurally "Low").
#
#    IMPORTANT — CV-correct detrending:
#    The trend is estimated only from the training fold and applied to both
#    train and test, avoiding data leakage.  The full-dataset model uses a
#    trend fitted on all data (same as the descriptive script).
#
#  Pipeline:
#    1. Load datasets (continuous for trend/breaks, discretized for features)
#    2. Optional detrend → discretize Wine_mhl residuals into Wine_class
#    3. Train carenR on full dataset (prm=FALSE) → readable rules
#    4. k-fold stratified CV with fold-level detrending when detrend=TRUE
#    5. Report confusion matrix, per-class P / R / F1, macro averages
#
#  Requires: carenR, tidyverse
#
#  NOTE: carenR writes temp files (RulesTemp.prm, caren_temp.bas,
#        caren_temp.bas.pre) to the working directory.  Run from rscripts/.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# libs --------------------------------------------------------------------------------

library(carenR)
library(tidyverse)


# config  ── keep identical to 4_ripper_pipeline.R -----------------------------------

file      <- 1     # 1 = RDD   |   other = RVV
n_classes <- 3     # Low / Medium / High
n_folds   <- 10    # cross-validation folds
seed      <- 42

# Set TRUE to classify residuals from the linear production trend instead of
# raw Wine_mhl.  Must match the detrend flag in 4_ripper_pipeline.R so that
# both models operate on the same target definition.
detrend <- TRUE

# Detrending method — only used when detrend = TRUE.
#   "linear"  : remove a fitted straight line  (original behaviour)
#   "lowess"  : remove a locally-weighted smooth trend (loess)
# Must match detrend_method in scripts 3, 4, and 6.
detrend_method <- "linear"   # ← switch to "lowess" for LOWESS comparison

# LOWESS smoothing span (fraction of points used in each local fit).
# Only used when detrend_method = "lowess". Keep fixed across all scripts.
loess_span <- 0.75

source("src/rscripts/utils/detrend_utils.R")

# carenR tuning parameters
# min.sup  = 0.2 : rule must cover ≥ 20% of training data
# min.conf = 0.5 : rule must be correct ≥ 50% of the time
# imp      = 0.001 : minimum improvement on confidence for rule refinement
caren_min_sup  <- 0.2
caren_min_conf <- 0.5
caren_imp      <- 0.001

class_labels <- c('Low', 'Medium', 'High')[1:n_classes]


# load data ---------------------------------------------------------------------------
# df_cont : continuous features  → trend fitting + quantile break computation
# df_disc : pre-discretized features → carenR feature matrix

if (file == 1) {
  df_cont <- read.csv('data/dataPrep_cont_dataset_rdd.csv')
  df_disc <- read.csv('data/dataPrep_dis_rule_dataset_rdd.csv')
  region  <- 'RDD'
} else {
  df_cont <- read.csv('data/dataPrep_cont_dataset_rvv.csv')
  df_disc <- read.csv('data/dataPrep_dis_rule_dataset_rvv.csv')
  region  <- 'RVV'
}

# safety: rows must be aligned on year in both files
stopifnot(
  "Row mismatch between cont and disc datasets — check data prep" =
    all(df_cont$year == df_disc$year)
)

cat('------------------------------------------------------------\n')
cat('Region:', region, '| Observations:', nrow(df_disc),
    '| Features:', ncol(df_disc) - 2, '\n')
cat('Detrend:', detrend, '\n')
cat('------------------------------------------------------------\n')


# Note: detrend_target() has been replaced by get_trend_residuals() from
# detrend_utils.R (sourced in config above). This supports both "linear" and
# "lowess" methods controlled by the detrend_method config variable.


# build target variable ---------------------------------------------------------------
# Compute Wine_mhl (or its residuals) for the full dataset, fit quantile breaks,
# assign Wine_class.  Breaks are computed once here; the same numeric thresholds
# are reused per fold so the class meaning is stable across the experiment.

year_col <- df_cont$year   # keep for fold-level trend fitting

if (detrend) {

  cat('\n--- Detrending enabled (method:', detrend_method,
      '| full-dataset trend for global class boundaries) ---\n')
  tr_full     <- get_trend_residuals(df_cont, method = detrend_method,
                                     span = loess_span, verbose = TRUE)
  wine_values <- tr_full$residuals
  cat('Residual mean:', round(mean(wine_values), 2),
      '| SD:', round(sd(wine_values), 2), '\n')

} else {

  cat('\n--- Detrending disabled — classifying raw Wine_mhl ---\n')
  wine_values <- df_cont$Wine_mhl

}

# Equal-frequency (quantile) breaks — same logic as RIPPER
breaks <- quantile(wine_values,
                   probs = seq(0, 1, length.out = n_classes + 1),
                   na.rm = TRUE)
breaks[1]              <- -Inf
breaks[length(breaks)] <- Inf
real_breaks            <- quantile(wine_values,
                                   probs = seq(0, 1, length.out = n_classes + 1),
                                   na.rm = TRUE)

df_disc$Wine_class <- cut(wine_values,
                          breaks         = breaks,
                          labels         = class_labels,
                          include.lowest = TRUE,
                          ordered_result = TRUE)

cat('\nTarget class distribution:\n')
tbl <- table(df_disc$Wine_class)
print(tbl)
cat('\nBalance check — each class should have roughly',
    round(nrow(df_disc) / n_classes), 'instances\n')
cat('Min:', min(tbl), '| Max:', max(tbl),
    '| Imbalance ratio:', round(max(tbl) / min(tbl), 2), '\n')
cat('\nQuantile thresholds',
    if (detrend) '(residuals, mhl)' else '(raw Wine_mhl, mhl)', ':\n')
print(round(real_breaks, 2))


# prepare feature matrix --------------------------------------------------------------
# Drop : year, Wine_mhl  (year is kept separately for fold-level trend fitting)
# Keep : all discretized climate features + Wine_class (factor)

df_caren <- df_disc %>%
  select(-year, -Wine_mhl) %>%
  mutate(Wine_class = factor(Wine_class, levels = class_labels, ordered = FALSE))

# carenR expects factors for all discretized feature columns
feat_cols <- setdiff(names(df_caren), 'Wine_class')
df_caren[feat_cols] <- lapply(df_caren[feat_cols], as.factor)

# attach year as auxiliary column (used only for fold-level detrending; dropped
# from the carenR input inside the CV loop)
df_caren$.year <- year_col

cat('\nDimensions going into carenR:',
    nrow(df_caren), 'rows x', ncol(df_caren) - 1, 'feature cols\n')


# train on full dataset (readable rules) ----------------------------------------------
# prm=FALSE → returns a human-readable data.frame of rules.
# This is the interpretable output, equivalent to print(model) in RIPPER.

cat('\n============================================================\n')
cat(' Training carenR on full dataset (readable rules) ...\n')
cat('============================================================\n')

set.seed(seed)

# Build full-dataset training frame (drop the auxiliary year column)
df_train_full <- df_caren %>% select(-.year)

# If detrending, the Wine_class is already based on full-dataset residuals above.
# No further action needed for the full-dataset model.

model_full <- caren(
  df_train_full,
  class    = 'Wine_class',
  min.sup  = caren_min_sup,
  min.conf = caren_min_conf,
  imp      = caren_imp,
  prm      = FALSE
)

cat('\n=== Discovered Rules (full dataset) ===\n')
if (is.data.frame(model_full) && nrow(model_full) > 0) {
  ar.pp(model_full)
  cat('\nTotal rules found:', nrow(model_full), '\n')
} else {
  cat('No rules found. Consider lowering caren_min_sup or caren_min_conf.\n')
}


# k-fold stratified cross-validation -------------------------------------------------
#
# Mirrors evaluate_Weka_classifier(model, numFolds=n_folds) in RIPPER.
# Stratification preserves class proportions in every fold.
#
# When detrend = TRUE:
#   - Trend is fitted on the training fold's continuous Wine_mhl (from df_cont)
#   - Residuals are computed for both train and test folds
#   - Quantile breaks are derived from training-fold residuals only
#   - Test fold receives Wine_class labels based on those training-fold breaks
#   → No information from the test set leaks into target construction.

cat('\n============================================================\n')
cat('', n_folds, '-fold Stratified Cross-Validation\n')
cat('============================================================\n')

set.seed(seed)

# Stratified fold assignment
fold_ids <- integer(nrow(df_caren))
for (cls in class_labels) {
  idx      <- which(df_caren$Wine_class == cls)
  idx_shuf <- sample(idx)
  assigned <- (seq_along(idx_shuf) - 1) %% n_folds + 1
  fold_ids[idx_shuf] <- assigned
}

# Containers
all_preds  <- rep(NA_character_, nrow(df_caren))
all_actual <- as.character(df_caren$Wine_class)   # global labels (for non-detrend)
fold_acc   <- numeric(n_folds)

for (k in seq_len(n_folds)) {

  idx_test  <- which(fold_ids == k)
  idx_train <- which(fold_ids != k)

  cat('\n--- Fold', k,
      '| train:', length(idx_train),
      '| test:',  length(idx_test), '---\n')

  # --- prepare fold data frames ------------------------------------------------
  # Start from df_caren (discretized features + .year auxiliary)
  train_fold_raw <- df_caren[idx_train, ]
  test_fold_raw  <- df_caren[idx_test,  ]

  if (detrend) {
    # Rebuild Wine_class inside this fold using only training-fold trend.
    # df_cont rows correspond 1-to-1 with df_caren rows (verified above).
    cont_train <- df_cont[idx_train, c('year', 'Wine_mhl')]
    cont_test  <- df_cont[idx_test,  c('year', 'Wine_mhl')]

    tr_fold     <- get_trend_residuals(cont_train, cont_train,
                                       method = detrend_method,
                                       span = loess_span, verbose = FALSE)
    resid_train <- tr_fold$residuals
    resid_test  <- get_trend_residuals(cont_train, cont_test,
                                       method = detrend_method,
                                       span = loess_span, verbose = FALSE)$residuals

    fold_breaks        <- quantile(resid_train,
                                   probs = seq(0, 1, length.out = n_classes + 1),
                                   na.rm = TRUE)
    fold_breaks[1]              <- -Inf
    fold_breaks[length(fold_breaks)] <- Inf

    train_fold_raw$Wine_class <- factor(
      cut(resid_train, breaks = fold_breaks, labels = class_labels,
          include.lowest = TRUE, ordered_result = TRUE),
      levels = class_labels)

    test_fold_raw$Wine_class  <- factor(
      cut(resid_test,  breaks = fold_breaks, labels = class_labels,
          include.lowest = TRUE, ordered_result = TRUE),
      levels = class_labels)

    # Update all_actual for these test rows (fold-specific labels)
    all_actual[idx_test] <- as.character(test_fold_raw$Wine_class)
  }

  # Drop auxiliary year column before passing to carenR
  train_fold <- train_fold_raw %>% select(-.year)
  test_fold  <- test_fold_raw  %>% select(-.year)

  # --- train -------------------------------------------------------------------
  # prm=TRUE writes RulesTemp.prm; returns handle "RulesTemp"
  prm_handle <- caren(
    train_fold,
    class    = 'Wine_class',
    min.sup  = caren_min_sup,
    min.conf = caren_min_conf,
    imp      = caren_imp,
    prm      = TRUE
  )

  # --- predict -----------------------------------------------------------------
  pred_result <- tryCatch(
    predict.caren(prm_handle, test_fold, class = 'Wine_class'),
    error = function(e) {
      cat('   [!] predict.caren error on fold', k, ':', conditionMessage(e), '\n')
      NULL
    }
  )

  if (is.null(pred_result)) {
    # No rules fired: fall back to majority class in training fold
    majority   <- names(sort(table(train_fold$Wine_class), decreasing = TRUE))[1]
    fold_preds <- rep(majority, length(idx_test))
    cat('   [!] Falling back to majority class:', majority, '\n')
  } else {
    fold_preds <- as.character(pred_result$Prediction)
    # carenR sometimes returns "Wine_class=High" — strip prefix
    fold_preds <- sub('^Wine_class=', '', fold_preds)
    fold_preds <- sub('^class=',      '', fold_preds)
  }

  all_preds[idx_test] <- fold_preds
  fold_acc[k] <- mean(fold_preds == as.character(test_fold$Wine_class),
                      na.rm = TRUE)
  cat('   Fold', k, 'accuracy:', round(fold_acc[k], 3), '\n')
}


# aggregate CV results ----------------------------------------------------------------

cat('\n============================================================\n')
cat(' Cross-Validation Summary\n')
cat('============================================================\n')

cat('\nPer-fold accuracy:\n')
for (k in seq_len(n_folds)) {
  cat('  Fold', k, ':', round(fold_acc[k], 3), '\n')
}
cat('\nMean CV Accuracy :', round(mean(fold_acc), 3))
cat('\nSD   CV Accuracy :', round(sd(fold_acc),   3), '\n')

# Confusion matrix
cat('\n=== Confusion Matrix ===\n')
conf_mat <- table(Actual    = factor(all_actual, levels = class_labels),
                  Predicted = factor(all_preds,  levels = class_labels))
print(conf_mat)

# Per-class metrics
cat('\n=== Per-class Precision / Recall / F1 ===\n')

precision <- diag(conf_mat) / colSums(conf_mat)
recall    <- diag(conf_mat) / rowSums(conf_mat)
f1        <- 2 * precision * recall / (precision + recall)

metrics <- data.frame(
  Class     = class_labels,
  Precision = round(precision, 3),
  Recall    = round(recall,    3),
  F1        = round(f1,        3),
  row.names = NULL
)
print(metrics)

cat('\nMacro-avg Precision :', round(mean(precision, na.rm = TRUE), 3))
cat('\nMacro-avg Recall    :', round(mean(recall,    na.rm = TRUE), 3))
cat('\nMacro-avg F1        :', round(mean(f1,        na.rm = TRUE), 3), '\n')

overall_acc <- sum(diag(conf_mat)) / sum(conf_mat)
cat('\nOverall CV Accuracy :', round(overall_acc, 3), '\n')


# save results to file ----------------------------------------------------------------
# Output goes to data/{detrend_method}/ so both runs coexist without overwriting.

out_dir      <- file.path('data', if (detrend) detrend_method else 'no_detrend')
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

results_file <- file.path(out_dir, paste0('caren_class_results_', tolower(region), '.txt'))

sink(results_file)
cat('carenR Classification Results -', region, '\n')
cat('Generated       :', format(Sys.time(), '%Y-%m-%d %H:%M'), '\n')
cat('Observations    :', nrow(df_caren), '\n')
cat('Detrend         :', detrend, '\n')
cat('Target classes  :', paste(class_labels, collapse = ' / '), '\n')
cat('Thresholds      :', paste(round(real_breaks, 2), collapse = ' | '), '\n')
cat('carenR params   : min.sup =', caren_min_sup,
    '| min.conf =', caren_min_conf,
    '| imp =', caren_imp, '\n\n')

cat('=== Full-dataset Rules ===\n')
if (is.data.frame(model_full) && nrow(model_full) > 0) {
  ar.pp(model_full)
  cat('\nTotal rules:', nrow(model_full), '\n')
} else {
  cat('No rules found.\n')
}

cat('\n=== Cross-Validation (', n_folds, 'folds) ===\n')
cat('Mean Accuracy :', round(mean(fold_acc), 3), '\n')
cat('SD Accuracy   :', round(sd(fold_acc),   3), '\n\n')

cat('Confusion Matrix:\n')
print(conf_mat)

cat('\nPer-class Metrics:\n')
print(metrics)

cat('\nMacro-avg Precision :', round(mean(precision, na.rm = TRUE), 3))
cat('\nMacro-avg Recall    :', round(mean(recall,    na.rm = TRUE), 3))
cat('\nMacro-avg F1        :', round(mean(f1,        na.rm = TRUE), 3), '\n')
cat('\nOverall CV Accuracy :', round(overall_acc, 3), '\n')
sink()

# Also save numeric metrics as CSV for easy comparison in script 8
metrics_csv <- data.frame(
  region          = region,
  detrend_method  = if (detrend) detrend_method else 'none',
  model           = 'carenR',
  accuracy        = round(overall_acc, 4),
  macro_precision = round(mean(precision, na.rm = TRUE), 4),
  macro_recall    = round(mean(recall,    na.rm = TRUE), 4),
  macro_f1        = round(mean(f1,        na.rm = TRUE), 4)
)
write.csv(metrics_csv,
          file.path(out_dir, paste0('caren_class_metrics_', tolower(region), '.csv')),
          row.names = FALSE)

cat('\nResults saved to:', out_dir, '\n')
cat('Done.\n')

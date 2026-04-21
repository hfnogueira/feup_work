# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       RIPPER Pipeline
#                       by:  hugonogueira
#
#  Applies JRip (WEKA's RIPPER implementation) to discover classification
#  rules for wine production (Wine_mhl).
#
#  Pipeline:
#    1. Load continuous feature dataset (output of features creation_v2.R)
#    2. Discretize target variable Wine_mhl into ordered classes (Low/Med/High)
#    3. Train JRip (RIPPER) — features stay continuous, RIPPER finds own thresholds
#    4. Evaluate with k-fold cross-validation
#    5. Print discovered rules
#
#  Requires: RWeka (wraps WEKA's Java JRip), tidyverse
#  Install:  install.packages("RWeka")
#            RWeka::WPM("install-package", "all")  # if needed
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# libs --------------------------------------------------------------------------------

library(tidyverse)

# Install RWeka if not available
if (!requireNamespace("RWeka", quietly = TRUE)) {
   install.packages("RWeka")
}
library(RWeka)


# config ------------------------------------------------------------------------------

file      <- 1   # 1 = RDD   |   other = RVV
n_classes <- 3   # number of target classes: 3 = Low / Medium / High
n_folds   <- 10  # cross-validation folds
seed      <- 42

# Set TRUE to classify residuals from the linear production trend instead of
# raw Wine_mhl.  Must match the detrend flag in 7_caren_classification_pipeline.R
# so that both models operate on the same target definition.
detrend <- TRUE

# Detrending method — only used when detrend = TRUE.
#   "linear"  : remove a fitted straight line  (original behaviour)
#   "lowess"  : remove a locally-weighted smooth trend (loess)
# Must match detrend_method in scripts 3, 6, and 7.
detrend_method <- "linear"   # ← switch to "lowess" for LOWESS comparison

# LOWESS smoothing span (fraction of points used in each local fit).
# Only used when detrend_method = "lowess". Keep fixed across all scripts.
loess_span <- 0.75

source("src/rscripts/utils/detrend_utils.R")

# JRip tuning parameters
# NOTE: datasets are small (~80-90 obs). Default N=2.0 is too permissive and
# can produce rules that cover only 2 instances (overfitting). With ~30 obs per
# class, N=4.0 means a rule must cover at least ~13% of its class — more robust.
# O=3 (optimisation passes) gives slightly better rule sets at negligible cost.
jrip_N <- 4.0   # min total weight of instances in a rule  (default: 2.0)
jrip_O <- 3     # number of optimisation runs              (default: 2)
jrip_F <- 3     # internal pruning folds                   (default: 3)


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
cat('------------------------------------------------------------\n')


# discretize target variable ----------------------------------------------------------
# Using quantile (equal-frequency) cuts so classes are balanced.
# n_classes = 3 → Low / Medium / High based on tertiles.
#
# When detrend = TRUE, the linear production trend is removed first.
# Low/Medium/High then mean "below / around / above what was expected for that era",
# removing the structural upward trend that would otherwise make early decades
# always appear "Low" regardless of climate conditions.
# Must match the detrend flag in 7_caren_classification_pipeline.R.

class_labels <- c('Low', 'Medium', 'High')[1:n_classes]

if (detrend) {

  cat('\n--- Detrending enabled (method:', detrend_method, ') ---\n')
  tr          <- get_trend_residuals(df, method = detrend_method,
                                     span = loess_span, verbose = TRUE)
  wine_values <- tr$residuals
  cat('Residual mean:', round(mean(wine_values), 2),
      '| SD:', round(sd(wine_values), 2), '\n\n')

} else {

  cat('\n--- Detrending disabled — using raw Wine_mhl ---\n\n')
  wine_values <- df$Wine_mhl

}

breaks <- quantile(wine_values,
                   probs = seq(0, 1, length.out = n_classes + 1),
                   na.rm = TRUE)

# Open boundaries to capture min/max safely
breaks[1]              <- -Inf
breaks[length(breaks)] <- Inf

df$Wine_class <- cut(wine_values,
                     breaks         = breaks,
                     labels         = class_labels,
                     include.lowest = TRUE,
                     ordered_result = TRUE)

real_breaks <- quantile(wine_values,
                        probs = seq(0, 1, length.out = n_classes + 1),
                        na.rm = TRUE)

cat('\nTarget class distribution (Wine_mhl discretized):\n')
tbl <- table(df$Wine_class)
print(tbl)

cat('\nBalance check — each class should have roughly', round(nrow(df) / n_classes), 'instances\n')
cat('Min class size:', min(tbl), '| Max class size:', max(tbl),
    '| Imbalance ratio:', round(max(tbl) / min(tbl), 2), '\n')

cat('\nWine_mhl quantile thresholds used (mhl):\n')
print(round(real_breaks, 2))


# prepare feature matrix --------------------------------------------------------------
# Drop: year (index), Wine_mhl (original continuous target)
# Keep: all 59 climate features (continuous) + Wine_class (factor target)

df_ripper <- df %>%
   select(-year, -Wine_mhl) %>%
   mutate(Wine_class = factor(Wine_class, levels = class_labels, ordered = FALSE))

cat('\nDimensions going into RIPPER:', nrow(df_ripper), 'rows x', ncol(df_ripper), 'cols\n')


# train RIPPER (JRip) -----------------------------------------------------------------
# JRip options (Weka notation):
#   -F  folds for internal pruning (default 3)
#   -N  minimum total weight of instances in a rule (default 2.0)
#   -O  number of optimisation runs (default 2)
#   -S  random seed

cat('\n============================================================\n')
cat(' Training RIPPER (JRip) on full dataset ...\n')
cat('============================================================\n')

set.seed(seed)

model <- JRip(
   Wine_class ~ .,
   data    = df_ripper,
   control = Weka_control(F = jrip_F, N = jrip_N, O = jrip_O, S = seed)
)

cat('\n=== Discovered Rules ===\n')
print(model)

cat('\n=== Model Summary ===\n')
summary(model)


# cross-validation evaluation ---------------------------------------------------------

cat('\n============================================================\n')
cat(' ', n_folds, '-fold Cross Validation\n')
cat('============================================================\n')

eval_cv <- evaluate_Weka_classifier(
   model,
   numFolds   = n_folds,
   complexity = TRUE,
   seed       = seed
)

print(eval_cv)


# per-class metrics -------------------------------------------------------------------

cat('\n=== Per-class Precision / Recall / F1 ===\n')

conf_mat <- eval_cv$confusionMatrix
if (!is.null(conf_mat)) {

   precision <- diag(conf_mat) / colSums(conf_mat)
   recall    <- diag(conf_mat) / rowSums(conf_mat)
   f1        <- 2 * precision * recall / (precision + recall)

   metrics <- data.frame(
      Class     = class_labels,
      Precision = round(precision, 3),
      Recall    = round(recall, 3),
      F1        = round(f1, 3)
   )

   print(metrics)
}


# save rules + metrics ----------------------------------------------------------------
# Output goes to data/{detrend_method}/ so both runs coexist without overwriting.

out_dir <- file.path('data', if (detrend) detrend_method else 'no_detrend')
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

rules_file <- file.path(out_dir, paste0('ripper_rules_', tolower(region), '.txt'))

sink(rules_file)
cat('RIPPER Rules -', region, '\n')
cat('Generated     :', format(Sys.time(), '%Y-%m-%d %H:%M'), '\n')
cat('Observations  :', nrow(df_ripper), '\n')
cat('Detrend method:', if (detrend) detrend_method else 'none', '\n')
cat('Target classes:', paste(class_labels, collapse = ' / '), '\n')
cat('Thresholds (mhl):', paste(round(real_breaks, 2), collapse = ' | '), '\n\n')
cat('=== Rules ===\n')
print(model)
cat('\n=== Cross-Validation (', n_folds, 'folds) ===\n')
print(eval_cv)
sink()

# Also save numeric metrics as CSV for easy comparison in script 8
if (!is.null(eval_cv$confusionMatrix)) {
  conf_mat  <- eval_cv$confusionMatrix
  precision <- diag(conf_mat) / colSums(conf_mat)
  recall    <- diag(conf_mat) / rowSums(conf_mat)
  f1        <- 2 * precision * recall / (precision + recall)
  overall   <- sum(diag(conf_mat)) / sum(conf_mat)

  metrics_csv <- data.frame(
    region         = region,
    detrend_method = if (detrend) detrend_method else 'none',
    model          = 'RIPPER',
    accuracy       = round(overall, 4),
    macro_precision= round(mean(precision, na.rm = TRUE), 4),
    macro_recall   = round(mean(recall,    na.rm = TRUE), 4),
    macro_f1       = round(mean(f1,        na.rm = TRUE), 4)
  )
  write.csv(metrics_csv,
            file.path(out_dir, paste0('ripper_metrics_', tolower(region), '.csv')),
            row.names = FALSE)
}

cat('\nOutput saved to:', out_dir, '\n')
cat('Done.\n')






# This is the rules from the full dataset
print(model)   # ← THIS is what you compare to CarenR


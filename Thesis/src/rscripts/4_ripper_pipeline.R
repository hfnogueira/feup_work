# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       RIPPER Pipeline
#                       by:  hugonogueira
#
#  Applies JRip (WEKA's RIPPER implementation) to discover classification
#  rules for wine production (Wine_mhl).
#
#  Pipeline:
#    1. Load continuous feature dataset (output of features creation_v2.R)
#    2. Discretize target variable Wine_mhl into quartile bins (Q1/Q2/Q3/Q4)
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
# Shared params (detrend, detrend_method, loess_span, n_folds, seed) come from
# utils/config.R. Per-run state and per-script tuning stay below.

source("src/rscripts/utils/config.R")
source("src/rscripts/utils/detrend_utils.R")

# Per-run state — change to switch region
file      <- 1   # 1 = RDD   |   other = RVV

# Per-script tuning
n_classes <- 4   # number of target classes: 4 = Q1 / Q2 / Q3 / Q4 (quartiles)
                 # Must match n_bins in 6_scenario_validation.R for consistency.

# JRip tuning parameters
# NOTE: datasets are small (~80-90 obs). Default N=2.0 is too permissive and
# can produce rules that cover only 2 instances (overfitting). With ~25 obs per
# class, N=4.0 means a rule must cover at least ~16% of its class — more robust.
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
# n_classes = 4 → Q1 / Q2 / Q3 / Q4 based on quartiles.
# Matches n_bins = 4 in 6_scenario_validation.R so rule discovery and predictive
# evaluation use the same class schema (note: script 6 re-fits breaks per fold).
#
# When detrend = TRUE, the production trend is removed first.
# Q1 then means "lowest production relative to era trend", Q4 means "highest",
# removing the structural upward trend that would otherwise make early decades
# always appear as Q1 regardless of climate conditions.

class_labels <- paste0('Q', seq_len(n_classes))   # Q1, Q2, Q3, Q4

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






# --- Structured rules CSV (read by script 9 rule interpreter) ----------------
# Parses the JRip print output into one row per rule.
# JRip condition format: (feature >= val) and (feature2 <= val2) => class=X (n/err)
# Default rule has no conditions: " => class=X (n/err)"

rules_raw  <- capture.output(print(model))
rule_lines <- grep("=>", rules_raw, value = TRUE)

if (length(rule_lines) > 0) {

  structured_ripper <- bind_rows(lapply(seq_along(rule_lines), function(i) {

    line  <- trimws(rule_lines[i])
    parts <- strsplit(line, "=>", fixed = TRUE)[[1]]

    cond_raw   <- trimws(parts[1])
    conseq_raw <- if (length(parts) >= 2) trimws(parts[2]) else ""

    # Parse consequent: "Wine_class=High (14.0/2.0)"
    m <- regmatches(conseq_raw,
           regexec("=(\\w+)\\s*\\(([0-9.]+)/([0-9.]+)\\)", conseq_raw))[[1]]

    n_cov <- if (length(m) >= 3) as.numeric(m[3]) else NA_real_
    n_err <- if (length(m) >= 4) as.numeric(m[4]) else NA_real_

    # Build one threshold column per internal break (n_classes - 1 of them)
    thresh_df <- as.data.frame(t(setNames(
      round(real_breaks[2:(length(real_breaks) - 1)], 3),
      paste0('threshold_Q', seq_len(n_classes - 1), '_', seq_len(n_classes - 1) + 1)
    )))

    cbind(data.frame(
      rule_index      = i,
      is_default      = nchar(cond_raw) == 0,
      conditions_raw  = if (nchar(cond_raw) == 0) "(default)" else cond_raw,
      predicted_class = if (length(m) >= 2) m[2] else NA_character_,
      n_covered       = n_cov,
      n_errors        = n_err,
      support_pct     = if (!is.na(n_cov)) round(n_cov / nrow(df_ripper) * 100, 1) else NA_real_,
      precision_pct   = if (!is.na(n_cov) && n_cov > 0) round((n_cov - n_err) / n_cov * 100, 1) else NA_real_,
      target_detrended= detrend,
      region          = region,
      detrend_method  = if (detrend) detrend_method else 'none',
      stringsAsFactors= FALSE
    ), thresh_df)
  }))

  struct_file <- file.path(out_dir, paste0('ripper_rules_structured_', tolower(region), '.csv'))
  write.csv(structured_ripper, struct_file, row.names = FALSE)
  cat('Saved structured RIPPER rules (for script 9):', struct_file, '\n')

} else {
  cat('[!] No rule lines found in JRip output — structured CSV not saved.\n')
}

cat('Done.\n')

# Full rules (compare to CarenR) #← THIS is what you compare to CarenR


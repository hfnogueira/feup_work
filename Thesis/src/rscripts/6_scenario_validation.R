# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                   Scenario-Based Validation Pipeline
#                   by:  hugonogueira
#
#  Evaluates three regression models across a series of train/test scenarios.
#  Each scenario uses an expanding training window; all years after the
#  training cutoff form the test set.
#
#  Models compared (1 classification + 4 regression + 3 baselines):
#    1. Naive                  — predict the training mean (MSE-optimal baseline)
#    2. Naive_Median           — predict the training median (MAE-optimal baseline)
#    3. Naive_MA               — predict mean of last N training years (adaptive baseline)
#    4. DT                     — Decision Tree (rpart), regression
#    3. Ripper                 — JRip (RIPPER) on quartile-binned target → bin mean (classification)
#    4. CarenR_Dist            — CAREN distribution rules, all 58 features (regression)
#    5. CarenR_Dist_FS         — CAREN, top-N by |Pearson cor| (continuous features vs target)
#    6. CarenR_Dist_Thresh     — CAREN, all features with |cor| ≥ threshold
#    7. CarenR_Dist_Sup        — CAREN, all 58 features, higher min.sup (broader rules)
#    8. CarenR_Dist_Sup_Thresh — CAREN, |cor| threshold + higher min.sup
#    9. CarenR_Dist_Eta2       — CAREN, top-N by η² (ANOVA on discretized bins vs target)
#   10. M5Rules                — Piecewise linear rule model (regression)
#
#  Key design:
#    - ALL models trained exclusively on the training split (no leakage)
#    - CarenR rules are discovered on the training portion of the discretized
#      dataset; conditions are then checked against continuous feature values
#    - Detrending is fold-correct: trend fitted on training only, applied
#      to test; predictions are converted back to mhl before scoring
#
#  step_years controls scenario granularity:
#    step_years = 1  → one new year added to training each scenario
#                       (many scenarios, see how accuracy evolves gradually)
#    step_years = 10 → 10 years added each scenario
#                       (few scenarios, bigger jumps; cleaner learning curve)
#
#  Two analytical angles:
#    1. Learning curve  — MAE/R² vs n_train per model
#                         (does more data improve accuracy?)
#    2. Forecast horizon — predictions over the test window for a fixed cutoff
#                         (do predictions degrade for years far from training?)
#
#  Output (in data/{detrend_method}/):
#    scenario_metrics_{region}.csv   — one row per (scenario × model)
#    scenario_predictions_{region}.csv — one row per (scenario × year × model)
#    dt_rules_{region}.txt           — DT rules for every scenario
#    plot_learning_curve_{region}.png
#    plot_horizon_{region}.png       — predictions for a selected scenario
#
#  Requires: tidyverse, RWeka, rpart, carenR
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# libs --------------------------------------------------------------------------------

library(tidyverse)
library(carenR)

if (!requireNamespace("RWeka",  quietly = TRUE)) install.packages("RWeka")
if (!requireNamespace("rpart",  quietly = TRUE)) install.packages("rpart")

library(RWeka)
library(rpart)


# config ------------------------------------------------------------------------------
# Shared params (detrend, detrend_method, loess_span, seed) come from
# utils/config.R. Per-run state and per-script tuning stay below.
# Detrending here is fold-correct (trend re-fitted on each training window).

source("src/rscripts/utils/config.R")
source("src/rscripts/utils/detrend_utils.R")

# Per-run state — change to switch region
file       <- 2     # 1 = RDD  |  2 = RVV

# Per-script tuning — walk-forward scenarios
min_train_pct <- 0.80   # minimum training fraction (e.g. 0.80 = 80% of data)
                        # min_train is derived from this after n is known (see below)
step_years    <- 1      # years added to training window each scenario
                        # (1 = one-by-one, 10 = decade steps)

# Classification rule learners (RIPPER + CarenR_Assoc)
n_bins            <- 4      # quantile bins for target discretisation (4 = quartiles)

# CarenR — shared by both approaches
carenr_min_sup    <- 0.2    # rule must cover ≥ 20% of training observations
carenr_min_sup_hi <- 0.3    # higher support threshold — fewer, broader, more generalizable rules
carenr_min_conf   <- 0.1    # minimum confidence
carenr_n_features <- 15     # top-N features selected per fold (used by _FS and _Eta2)
carenr_cor_thresh <- 0.30   # |cor| threshold for CarenR_Dist_Thresh / _Sup_Thresh variants
carenr_jvm_mb     <- "4000M"  # JVM heap for caren() calls

# CarenR_Dist (manual distribution-rule approach)
carenr_strategy     <- 'C'   # 'A' = first rule, 'B' = simple avg, 'C' = support-weighted avg,
                              # 'D' = confidence-weighted (pvalue-based, see CarenR_Dist_Conf)
carenr_min_subgroup <- 3     # min training obs in a subgroup to trust its mean
carenr_agg_fn       <- "median"  # within-subgroup aggregation: "median" (MAE-optimal)
                                  # or "mean" (MSE-optimal). Applied to all CarenR variants.
carenr_lag_max      <- 5     # maximum lag (years) to consider in the ACF analysis
                              # significant lags are selected automatically per fold

# Decision Tree
dt_cp       <- 0.01   # complexity parameter (higher = simpler tree)
dt_minsplit <- 10     # min obs to attempt a split

# M5Rules
m5_unsmoothed <- FALSE   # TRUE = disable leaf-model smoothing

# Naive extended baselines
naive_ma_window <- 10   # moving average: use last N training years
                        # falls back to full mean if n_train < naive_ma_window

# Horizon plot: which scenario index to use for the horizon plot
# NULL = automatically use the middle scenario
horizon_scenario <- NULL


# load data ---------------------------------------------------------------------------

if (file == 1) {
  df_cont <- read.csv('data/dataPrep_cont_dataset_rdd.csv')
  df_disc <- read.csv('data/dataPrep_dis_rule_dataset_rdd.csv')
  region  <- 'RDD'
} else {
  df_cont <- read.csv('data/dataPrep_cont_dataset_rvv.csv')
  df_disc <- read.csv('data/dataPrep_dis_rule_dataset_rvv.csv')
  region  <- 'RVV'
}

# Rows must be aligned by year in both datasets
stopifnot("Row count mismatch between cont and disc datasets" =
            nrow(df_cont) == nrow(df_disc))
stopifnot("Year mismatch between cont and disc datasets" =
            all(df_cont$year == df_disc$year))

df_cont <- df_cont %>% arrange(year)
df_disc <- df_disc %>% arrange(year)

n <- nrow(df_cont)

# Derive min_train from the percentage — must leave at least 1 test observation
min_train <- max(floor(min_train_pct * n), 1)

cat('------------------------------------------------------------\n')
cat('Region:', region, '| Observations:', n, '\n')
cat('Year range:', min(df_cont$year), '-', max(df_cont$year), '\n')
cat('Global mean Wine_mhl:', round(mean(df_cont$Wine_mhl), 2), 'mhl\n')
cat('Detrend:', detrend, if (detrend) paste0('(', detrend_method, ')') else '', '\n')
cat('min_train_pct:', min_train_pct, '→ min_train:', min_train, 'obs\n')
cat('step_years:', step_years, '\n')
cat('------------------------------------------------------------\n\n')


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# --- CarenR_Dist helpers: parse distribution rules and predict manually --------
#
# Used by the CarenR_Dist model (Dist=TRUE approach).
# Discovers distribution rules via KS test; for each test observation finds
# all firing rules and combines subgroup means by strategy A/B/C.
# This is deliberately kept separate from CarenR_Assoc (predict.caren / prm=TRUE)
# so both approaches can be compared in the scenario loop.
#
# Subgroup format:  feature=[lower|upper)  or  feature=[lower|upper]
#   [  always means >=   (left always inclusive)
#   )  means <  (right exclusive)
#   ]  means <= (right inclusive)

v <- function(obs, col) obs[[col]]

parse_one_condition <- function(cond) {
  cond      <- trimws(cond)
  split_pos <- regexpr("=\\[", cond)
  feature   <- substr(cond, 1, split_pos - 1)
  interval  <- substr(cond, split_pos + 1, nchar(cond))

  close_incl <- endsWith(interval, "]")
  right_op   <- if (close_incl) "<=" else "<"

  inner  <- gsub("[\\[\\]()\\ ]", "", interval, perl = TRUE)
  parts  <- strsplit(inner, "\\|")[[1]]
  lower  <- as.numeric(parts[1])
  upper  <- as.numeric(parts[2])

  paste0('v(obs,"', feature, '") >= ', lower,
         ' & v(obs,"', feature, '") ', right_op, ' ', upper)
}

parse_rule_fn <- function(subgroup_str) {
  conditions <- strsplit(subgroup_str, "\\s+&\\s+")[[1]]
  r_parts    <- vapply(trimws(conditions), parse_one_condition, character(1))
  full_expr  <- paste(r_parts, collapse = " & ")
  eval(parse(text = paste0("function(obs) { ", full_expr, " }")))
}

parse_drs <- function(drs) {
  if (is.null(drs) || !is.data.frame(drs) || nrow(drs) == 0)
    return(list(rules_df = NULL, rule_fns = list()))

  rules_df <- drs %>%
    mutate(Ant_sup = as.numeric(Ant_sup),
           pvalue  = as.numeric(pvalue),
           Mean    = as.numeric(Mean)) %>%
    filter(Ant_sup < 1.0) %>%       # drop global rule
    arrange(pvalue)

  if (nrow(rules_df) == 0)
    return(list(rules_df = NULL, rule_fns = list()))

  # caren() returns Subgroup as a list column where each element is a
  # character vector of individual conditions (one string per condition).
  # Join them with " & " so parse_rule_fn can split them back correctly.
  if (is.list(rules_df$Subgroup)) {
    rules_df$Subgroup <- sapply(rules_df$Subgroup,
                                function(x) paste(as.character(x), collapse = " & "),
                                USE.NAMES = FALSE)
  } else {
    rules_df$Subgroup <- as.character(rules_df$Subgroup)
  }

  rule_fns <- lapply(seq_len(nrow(rules_df)), function(i) {
    tryCatch(
      parse_rule_fn(rules_df$Subgroup[i]),
      error = function(e) {
        cat('    [parse_rule_fn error] rule', i, ':', conditionMessage(e), '\n')
        function(obs) FALSE
      }
    )
  })

  # Confidence weights: -log(pvalue).  Higher = rule is more statistically
  # significant (smaller p-value from KS test).  Used by strategy 'D'.
  conf_weights <- -log(pmax(as.numeric(rules_df$pvalue), 1e-300))

  list(rules_df = rules_df, rule_fns = rule_fns, conf_weights = conf_weights)
}

predict_carenr_dist <- function(test_obs, train_data, rule_fns, strategy, min_subgroup,
                                agg_fn       = mean,   # within-subgroup aggregation
                                rule_weights = NULL) { # NULL → use support; numeric vector → custom weights

  if (length(rule_fns) == 0) return(agg_fn(train_data$Wine_mhl))

  preds   <- numeric(0)
  weights <- numeric(0)

  for (k in seq_along(rule_fns)) {
    fn    <- rule_fns[[k]]
    fires <- tryCatch(isTRUE(fn(test_obs)), error = function(e) FALSE)
    if (!fires) next

    train_match <- vapply(seq_len(nrow(train_data)), function(j) {
      tryCatch(isTRUE(fn(train_data[j, , drop = FALSE])),
               error = function(e) FALSE)
    }, logical(1))

    n_match <- sum(train_match)
    if (n_match < min_subgroup) next

    # Within-subgroup prediction (mean or median depending on agg_fn)
    preds <- c(preds, agg_fn(train_data$Wine_mhl[train_match]))

    # Weight: custom (confidence) if supplied, else support fraction
    w <- if (!is.null(rule_weights)) rule_weights[k] else n_match / nrow(train_data)
    weights <- c(weights, w)

    if (strategy == 'A') break
  }

  if (length(preds) == 0) return(agg_fn(train_data$Wine_mhl))

  switch(strategy,
         'A' = preds[1],
         'B' = mean(preds),
         'C' = ,                                        # C and D share the same formula;
         'D' = sum(preds * weights) / sum(weights))     # only the weights differ
}


# --- Classification helpers: map predicted bin labels to numeric predictions --
#
# Used by RIPPER and CarenR_Assoc after they predict a quartile bin label.
# Returns an n×3 matrix with columns pred, lower, upper (all on mhl scale).
# Observations whose predicted bin is NA or unknown fall back to NA.

apply_bin_stats <- function(pred_bins, bin_stats, trend_offsets) {
  pred_bins <- as.character(pred_bins)
  result <- t(sapply(seq_along(pred_bins), function(i) {
    bl <- pred_bins[i]
    if (is.na(bl) || !bl %in% names(bin_stats))
      c(pred = NA_real_, lower = NA_real_, upper = NA_real_)
    else {
      bs <- bin_stats[[bl]]
      c(pred  = bs$mean  + trend_offsets[i],
        lower = bs$lower + trend_offsets[i],
        upper = bs$upper + trend_offsets[i])
    }
  }))
  # Ensure matrix shape even when n_test == 1 (sapply returns a named vector then)
  if (is.null(dim(result))) result <- matrix(result, nrow = 1, dimnames = list(NULL, c('pred','lower','upper')))
  result
}


# --- Eta-squared (η²) for feature pre-selection (CarenR_Dist_Eta2) -----------
#
# Measures how much variance in the continuous target is explained by the
# discrete bins of a single feature (one-way ANOVA).
#
# Why η² instead of Pearson for CAREN?
#   CAREN sees features in their DISCRETIZED form (interval bins, not raw numbers).
#   Pearson measures linear association on continuous values — it misses threshold
#   effects and ignores the information lost during discretization.
#   η² = SS_between / SS_total asks exactly the right question: "do the bins of
#   this feature separate the target distribution?" — which is what CAREN's KS
#   test evaluates internally.
#
# Returns NA for constant or all-missing features (safe to use with na.last=TRUE
# in sort() so NA-features are always ranked last).

compute_eta2 <- function(bins_vec, target) {
  bins_fac <- factor(as.character(bins_vec))
  if (nlevels(bins_fac) < 2 || all(is.na(target))) return(NA_real_)
  fit  <- aov(target ~ bins_fac)
  s    <- summary(fit)[[1]]
  ss_b <- s["bins_fac", "Sum Sq"]
  ss_t <- sum(s[, "Sum Sq"])
  if (ss_t == 0) return(0)
  ss_b / ss_t
}


# --- CAREN-format interval discretization ------------------------------------
# Discretizes a continuous vector into interval strings matching the format
# used by train_disc:  [lower|upper)  for all bins except the last,  [lower|upper]
# for the last bin.  CAREN reads these as categorical levels and generates
# conditions that parse_one_condition can decode back to numeric boundaries.
#
# Arguments:
#   values  — numeric vector to discretize (may contain NAs)
#   breaks  — numeric breakpoints (from quantile() on training data)
# Returns a character vector (same length as values; NA where values is NA).

make_caren_intervals <- function(values, breaks) {
  n_b    <- length(breaks) - 1
  labels <- character(n_b)
  for (i in seq_len(n_b)) {
    lo <- breaks[i]; hi <- breaks[i + 1]
    labels[i] <- sprintf("[%g|%g%s", lo, hi, if (i == n_b) "]" else ")")
  }
  as.character(cut(values, breaks = breaks, labels = labels, include.lowest = TRUE))
}


# --- Supervised cut-point finder ---------------------------------------------
# Finds n_bins-1 interior cut points for a continuous feature x that maximise
# the between-group sum-of-squares of target y (training residuals).
# Uses greedy binary recursive splitting — equivalent to fitting a 1-D
# decision stump chain.  Cut points are derived entirely from training data,
# so applying them inside the scenario loop avoids data leakage.
#
# Arguments:
#   x        — numeric feature vector (training observations)
#   y        — numeric target vector (training residuals, same length as x)
#   n_bins   — number of bins to produce (default 3 → two cut points)
#   min_obs  — minimum observations required on each side of a split
# Returns a numeric vector of n_bins-1 cut points.

find_supervised_cuts <- function(x, y, n_bins = 3, min_obs = 5) {
  x <- as.numeric(x);  y <- as.numeric(y)
  ok <- !is.na(x) & !is.na(y);  x <- x[ok];  y <- y[ok]

  # Fallback: not enough data → equal-frequency quantile cuts
  if (length(x) < 2 * min_obs)
    return(as.numeric(quantile(x, seq_len(n_bins - 1) / n_bins)))

  best_split <- function(xv, yv) {
    cands      <- sort(unique(xv));  cands <- cands[-length(cands)]
    if (length(cands) == 0) return(NA_real_)
    grand_mean <- mean(yv)
    scores <- vapply(cands, function(t) {
      lo <- yv[xv <= t];  hi <- yv[xv > t]
      if (length(lo) < min_obs || length(hi) < min_obs) return(-Inf)
      # Between-group SS (proportional to variance reduction)
      length(lo) * (mean(lo) - grand_mean)^2 +
        length(hi) * (mean(hi) - grand_mean)^2
    }, numeric(1))
    if (all(is.infinite(scores))) return(NA_real_)
    cands[which.max(scores)]
  }

  if (n_bins == 2) {
    t1 <- best_split(x, y)
    return(if (is.na(t1)) as.numeric(quantile(x, 0.5)) else t1)
  }

  # Two cuts for 3 bins: first globally best, then best inside one segment
  t1 <- best_split(x, y)
  if (is.na(t1)) return(as.numeric(quantile(x, c(1/3, 2/3))))

  lo_m <- x <= t1;  hi_m <- !lo_m
  t2_lo <- if (sum(lo_m) >= 2 * min_obs) best_split(x[lo_m], y[lo_m]) else NA_real_
  t2_hi <- if (sum(hi_m) >= 2 * min_obs) best_split(x[hi_m], y[hi_m]) else NA_real_

  score_gain <- function(t, xv, yv) {
    if (is.na(t)) return(-Inf)
    lo <- yv[xv <= t];  hi <- yv[xv > t]
    if (length(lo) < min_obs || length(hi) < min_obs) return(-Inf)
    gm <- mean(yv)
    length(lo) * (mean(lo) - gm)^2 + length(hi) * (mean(hi) - gm)^2
  }

  t2 <- if (score_gain(t2_lo, x[lo_m], y[lo_m]) >=
             score_gain(t2_hi, x[hi_m], y[hi_m])) t2_lo else t2_hi
  if (is.na(t2))
    t2 <- as.numeric(quantile(x, ifelse(mean(lo_m) >= 0.5, 1/3, 2/3)))

  cuts <- sort(unique(c(t1, t2)))   # deduplicate — prevents "breaks not unique" in cut()

  # If deduplication collapsed to a single cut, add a second from equal-frequency
  if (length(cuts) < n_bins - 1) {
    fallback <- as.numeric(quantile(x, seq_len(n_bins - 1) / n_bins))
    cuts <- sort(unique(c(cuts, fallback)))[seq_len(n_bins - 1)]
  }

  cuts
}


# --- Metrics ------------------------------------------------------------------

compute_metrics <- function(actual, predicted) {
  err    <- actual - predicted
  mae    <- mean(abs(err),   na.rm = TRUE)
  rmse   <- sqrt(mean(err^2, na.rm = TRUE))
  ss_res <- sum(err^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  r2     <- 1 - ss_res / ss_tot
  c(mae = mae, rmse = rmse, r2 = r2)
}


# --- DT rule extractor --------------------------------------------------------
# Returns a character vector of human-readable rules from an rpart model.
# Each rule = path from root to leaf + the leaf prediction.

extract_dt_rules <- function(model, round_digits = 3) {
  frame <- model$frame

  # Node indices where var == "<leaf>"
  leaf_nodes <- as.integer(rownames(frame)[frame$var == "<leaf>"])

  rules <- character(length(leaf_nodes))

  for (k in seq_along(leaf_nodes)) {
    node <- leaf_nodes[k]

    # Walk up the tree to collect conditions
    path_conds  <- character(0)
    current     <- node

    while (current > 1) {
      parent    <- current %/% 2
      is_left   <- (current == parent * 2)       # left = condition TRUE
      p_row     <- frame[as.character(parent), ]
      split_var <- as.character(p_row$var)

      # Extract the split point from the splits matrix
      split_info <- model$splits[as.character(parent), ]
      split_pt   <- round(split_info["index"], round_digits)

      if (is_left) {
        cond <- paste0(split_var, " < ", split_pt)
      } else {
        cond <- paste0(split_var, " >= ", split_pt)
      }
      path_conds <- c(cond, path_conds)   # prepend to keep root-to-leaf order
      current    <- parent
    }

    leaf_row <- frame[as.character(node), ]
    pred_val <- round(leaf_row$yval, round_digits)
    n_obs    <- leaf_row$n

    rule_text <- if (length(path_conds) == 0) {
      paste0("Rule ", k, ": [ALL] → predict ", pred_val, " (n=", n_obs, ")")
    } else {
      paste0("Rule ", k, ": IF ", paste(path_conds, collapse = " AND "),
             " → predict ", pred_val, " (n=", n_obs, ")")
    }
    rules[k] <- rule_text
  }

  rules
}


# ==============================================================================
# GENERATE SCENARIOS
# ==============================================================================

# Each scenario is defined by the last training index.
# train indices:  1 .. train_end
# test  indices: (train_end+1) .. n

train_ends <- seq(min_train, n - 1, by = step_years)

# If the last step doesn't reach n-1, add it so we always include a scenario
# that uses the maximum possible training data
if (tail(train_ends, 1) < n - 1) train_ends <- c(train_ends, n - 1)

n_scenarios <- length(train_ends)

cat('Scenarios generated:', n_scenarios, '\n')
cat('Train size range:', min(train_ends), '-', max(train_ends), 'obs\n')
cat('Test size range:', n - max(train_ends), '-', n - min(train_ends), 'obs\n\n')


# ==============================================================================
# MAIN SCENARIO LOOP
# ==============================================================================

# Pre-allocate result containers
all_metrics     <- vector('list', n_scenarios)
all_predictions <- vector('list', n_scenarios)
dt_rules_log    <- character(0)   # accumulate DT rule text across scenarios

set.seed(seed)

cat('Running', n_scenarios, 'scenarios...\n\n')

for (s in seq_along(train_ends)) {

  te         <- train_ends[s]
  train_cont <- df_cont[seq_len(te), ]
  test_cont  <- df_cont[(te + 1):n, ]

  train_disc <- df_disc[seq_len(te), ]
  test_disc  <- df_disc[(te + 1):n, ]   # used only for feature values (not target)

  n_test  <- nrow(test_cont)
  n_train <- nrow(train_cont)

  cat(sprintf('Scenario %3d/%d  |  train: %d obs (%d-%d)  |  test: %d obs (%d-%d)\n',
              s, n_scenarios,
              n_train, min(train_cont$year), max(train_cont$year),
              n_test,  min(test_cont$year),  max(test_cont$year)))

  # --------------------------------------------------------------------------
  # 1. FOLD-CORRECT DETRENDING
  # --------------------------------------------------------------------------

  if (detrend) {
    trend_model      <- fit_trend(train_cont, method = detrend_method, span = loess_span)
    train_residuals  <- train_cont$Wine_mhl - apply_trend(trend_model, train_cont)
    trend_offsets    <- apply_trend(trend_model, test_cont)   # one per test obs
  } else {
    train_residuals <- train_cont$Wine_mhl
    trend_offsets   <- rep(0, n_test)
  }

  # Working copy: train features + residuals (no year)
  train_features <- train_cont %>%
    mutate(Wine_mhl = train_residuals) %>%
    select(-year)

  test_features  <- test_cont %>% select(-year, -Wine_mhl)   # features only

  # --------------------------------------------------------------------------
  # 2. DISCRETIZATION  (shared by RIPPER and CarenR_Assoc)
  # --------------------------------------------------------------------------
  # Quartile-bin the de-trended target on training data only (no leakage).
  # Classification models predict a bin label; apply_bin_stats() maps that
  # label back to the bin's training mean + 95% CI for numeric evaluation.

  bin_breaks <- unique(quantile(train_residuals,
                                probs = seq(0, 1, length.out = n_bins + 1)))
  bin_labels <- paste0("Q", seq_len(length(bin_breaks) - 1))

  train_bins <- cut(train_residuals,
                    breaks         = bin_breaks,
                    labels         = bin_labels,
                    include.lowest = TRUE)

  # Per-bin statistics on training residuals (used for point preds + CI)
  bin_stats <- setNames(lapply(bin_labels, function(bl) {
    vals <- train_residuals[train_bins == bl]
    n_b  <- length(vals)
    m    <- mean(vals)
    se   <- if (n_b > 1) sd(vals) / sqrt(n_b) else 0
    t_q  <- if (n_b > 1) qt(0.975, df = n_b - 1) else 0
    list(mean = m, lower = m - t_q * se, upper = m + t_q * se, n = n_b)
  }), bin_labels)

  # --------------------------------------------------------------------------
  # 3. NAIVE BASELINES
  #    3a. Naive_Mean   — training mean  (minimises MSE, standard baseline)
  #    3b. Naive_Median — training median (minimises MAE — theoretically optimal
  #                       point prediction under our evaluation metric)
  #    3c. Naive_MA     — mean of last naive_ma_window training years
  #                       (adapts to recent production levels; ignores older data)
  # --------------------------------------------------------------------------

  naive_train_mean   <- mean(train_residuals)
  pred_naive         <- naive_train_mean + trend_offsets

  naive_train_median <- median(train_residuals)
  pred_naive_median  <- naive_train_median + trend_offsets

  ma_vals            <- tail(train_residuals, naive_ma_window)   # last N years
  naive_ma_mean      <- mean(ma_vals)
  pred_naive_ma      <- naive_ma_mean + trend_offsets

  # --------------------------------------------------------------------------
  # 4. DECISION TREE  (rpart)
  # --------------------------------------------------------------------------

  pred_dt <- tryCatch({
    dt_model <- rpart(Wine_mhl ~ .,
                      data    = train_features,
                      control = rpart.control(cp = dt_cp, minsplit = dt_minsplit))
    as.numeric(predict(dt_model, test_features)) + trend_offsets
  }, error = function(e) {
    cat('  [!] DT failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --- Extract and log DT rules for this scenario ---------------------------
  dt_rules_header <- paste0(
    "\n", strrep("=", 60), "\n",
    "SCENARIO ", s, " — train: ", n_train, " obs (",
    min(train_cont$year), "-", max(train_cont$year), ")",
    "  test: ", n_test, " obs\n",
    strrep("=", 60), "\n"
  )

  dt_rules_text <- tryCatch({
    tree_structure <- paste(capture.output(print(dt_model)), collapse = "\n")
    rule_lines     <- extract_dt_rules(dt_model)
    paste0(
      dt_rules_header,
      "\nTree structure:\n", tree_structure,
      "\n\nPath-based rules:\n",
      paste(rule_lines, collapse = "\n"), "\n"
    )
  }, error = function(e) {
    paste0(dt_rules_header, "  [could not extract rules: ", conditionMessage(e), "]\n")
  })

  dt_rules_log <- c(dt_rules_log, dt_rules_text)

  # --------------------------------------------------------------------------
  # 5a. RIPPER (JRip) — classification rule learner on continuous features
  #     Target: quartile-binned de-trended Wine_mhl (train_bins)
  #     Prediction: bin mean + 95% CI mapped back to mhl scale
  # --------------------------------------------------------------------------

  pred_ripper <- tryCatch({

    ripper_train <- train_features %>% mutate(Wine_mhl = train_bins)
    ripper_model <- JRip(Wine_mhl ~ ., data = ripper_train)
    pred_bins    <- as.character(predict(ripper_model, test_features))
    bin_result   <- apply_bin_stats(pred_bins, bin_stats, trend_offsets)
    list(pred  = bin_result[, 'pred'],
         lower = bin_result[, 'lower'],
         upper = bin_result[, 'upper'])

  }, error = function(e) {
    cat('  [!] RIPPER failed on scenario', s, ':', conditionMessage(e), '\n')
    list(pred  = rep(NA_real_, n_test),
         lower = rep(NA_real_, n_test),
         upper = rep(NA_real_, n_test))
  })

  # Resolve aggregation function once per scenario from config string
  subgroup_agg <- if (carenr_agg_fn == "median") median else mean

  # --------------------------------------------------------------------------
  # 5b. CARENR_DIST — manual distribution-rule approach with Dist=TRUE
  #     Distribution rules selected by KS test; support-weighted avg of
  #     all firing subgroup aggregates (mean or median per carenr_agg_fn).
  #     Rules are mined for statistical interestingness, repurposed for prediction.
  # --------------------------------------------------------------------------

  pred_caren_dist <- tryCatch({

    feat_cols      <- setdiff(names(train_disc), c('year', 'Wine_mhl'))

    disc_train_tmp <- train_disc
    disc_train_tmp[feat_cols] <- lapply(disc_train_tmp[feat_cols], function(x) {
      as.factor(as.character(x))
    })
    disc_train_tmp$Wine_mhl <- train_residuals
    disc_train_tmp          <- disc_train_tmp %>% select(-year)

    drs <- suppressMessages(suppressWarnings(
      caren(disc_train_tmp,
            Dist     = TRUE,
            POI      = 'Wine_mhl',
            min.sup  = carenr_min_sup,
            min.conf = carenr_min_conf)
    ))

    parsed <- parse_drs(drs)

    # Rules use actual continuous boundaries (e.g. rf_fl_y1_a10=[20.7|121])
    # so rule matching must use the continuous dataset, not disc data.
    train_for_pred <- train_cont %>% mutate(Wine_mhl = train_residuals)

    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0   # dummy — rules don't condition on target
      predict_carenr_dist(test_obs_j,
                          train_for_pred,
                          parsed$rule_fns,
                          carenr_strategy,
                          carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5c. CARENR_DIST_FS — same as CarenR_Dist but with feature pre-selection
  #     Top-N features by |Pearson correlation| with train_residuals are kept;
  #     the rest are dropped before rule mining. Reduces spurious rules from
  #     irrelevant features in high-dimensional small-sample settings.
  # --------------------------------------------------------------------------

  pred_caren_dist_fs <- tryCatch({

    feat_cols <- setdiff(names(train_disc), c('year', 'Wine_mhl'))

    # Rank features by |cor| with continuous residuals on training data.
    # Correlation computed on continuous values (train_cont) — same column
    # names as train_disc, correct numeric values for Pearson.
    feat_cors <- sapply(feat_cols, function(col)
      abs(cor(train_cont[[col]], train_residuals, use = 'complete.obs')))
    top_feats <- names(sort(feat_cors, decreasing = TRUE))[seq_len(carenr_n_features)]

    disc_train_fs <- train_disc
    disc_train_fs[feat_cols] <- lapply(disc_train_fs[feat_cols], function(x)
      as.factor(as.character(x)))
    disc_train_fs$Wine_mhl <- train_residuals
    disc_train_fs <- disc_train_fs %>% select(all_of(top_feats), Wine_mhl)

    drs_fs <- suppressMessages(suppressWarnings(
      caren(disc_train_fs,
            Dist     = TRUE,
            POI      = 'Wine_mhl',
            min.sup  = carenr_min_sup,
            min.conf = carenr_min_conf)
    ))

    parsed_fs <- parse_drs(drs_fs)

    train_for_pred <- train_cont %>% mutate(Wine_mhl = train_residuals)

    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0
      predict_carenr_dist(test_obs_j,
                          train_for_pred,
                          parsed_fs$rule_fns,
                          carenr_strategy,
                          carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist_FS failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5d. CARENR_DIST_THRESH — threshold-based feature selection
  #     Keeps all features where |cor| with train_residuals exceeds a fixed
  #     threshold. Adaptive: the number of selected features varies per fold
  #     depending on how many features pass the bar. Falls back to the single
  #     most correlated feature if none pass the threshold.
  # --------------------------------------------------------------------------

  pred_caren_dist_thresh <- tryCatch({

    feat_cols <- setdiff(names(train_disc), c('year', 'Wine_mhl'))

    feat_cors    <- sapply(feat_cols, function(col)
      abs(cor(train_cont[[col]], train_residuals, use = 'complete.obs')))
    feat_cors_ok <- feat_cors[!is.na(feat_cors)]          # drop zero-variance features
    thresh_feats <- names(feat_cors_ok[feat_cors_ok >= carenr_cor_thresh])

    # Fallback: always keep at least the single best feature
    if (length(thresh_feats) == 0)
      thresh_feats <- names(which.max(feat_cors_ok))

    cat(sprintf('  [dist_thresh] |cor|>=%.2f: %d features selected\n',
                carenr_cor_thresh, length(thresh_feats)))

    disc_train_th <- train_disc
    disc_train_th[feat_cols] <- lapply(disc_train_th[feat_cols], function(x)
      as.factor(as.character(x)))
    disc_train_th$Wine_mhl <- train_residuals
    disc_train_th <- disc_train_th %>% select(all_of(thresh_feats), Wine_mhl)

    drs_th <- suppressMessages(suppressWarnings(
      caren(disc_train_th,
            Dist     = TRUE,
            POI      = 'Wine_mhl',
            min.sup  = carenr_min_sup,
            min.conf = carenr_min_conf)
    ))

    parsed_th <- parse_drs(drs_th)

    train_for_pred <- train_cont %>% mutate(Wine_mhl = train_residuals)

    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0
      predict_carenr_dist(test_obs_j,
                          train_for_pred,
                          parsed_th$rule_fns,
                          carenr_strategy,
                          carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist_Thresh failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5e. CARENR_DIST_SUP — all 58 features, higher min.sup (0.4)
  #     Broader rules that must cover ≥40% of training observations.
  #     Tests whether requiring more generalisable rules (independent of
  #     feature selection) reduces spurious overfitting.
  # --------------------------------------------------------------------------

  pred_caren_dist_sup <- tryCatch({

    feat_cols <- setdiff(names(train_disc), c('year', 'Wine_mhl'))

    disc_train_sup <- train_disc
    disc_train_sup[feat_cols] <- lapply(disc_train_sup[feat_cols], function(x)
      as.factor(as.character(x)))
    disc_train_sup$Wine_mhl <- train_residuals
    disc_train_sup <- disc_train_sup %>% select(-year)

    drs_sup <- suppressMessages(suppressWarnings(
      caren(disc_train_sup,
            Dist     = TRUE,
            POI      = 'Wine_mhl',
            min.sup  = carenr_min_sup_hi,
            min.conf = carenr_min_conf)
    ))

    parsed_sup <- parse_drs(drs_sup)

    train_for_pred <- train_cont %>% mutate(Wine_mhl = train_residuals)

    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0
      predict_carenr_dist(test_obs_j,
                          train_for_pred,
                          parsed_sup$rule_fns,
                          carenr_strategy,
                          carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist_Sup failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5f. CARENR_DIST_SUP_THRESH — |cor| threshold + higher min.sup
  #     Combines both levers: pre-select features by correlation AND require
  #     broader rules. Tests whether the two improvements compound.
  # --------------------------------------------------------------------------

  pred_caren_dist_sup_thresh <- tryCatch({

    feat_cols <- setdiff(names(train_disc), c('year', 'Wine_mhl'))

    feat_cors    <- sapply(feat_cols, function(col)
      abs(cor(train_cont[[col]], train_residuals, use = 'complete.obs')))
    feat_cors_ok <- feat_cors[!is.na(feat_cors)]
    thresh_feats <- names(feat_cors_ok[feat_cors_ok >= carenr_cor_thresh])
    if (length(thresh_feats) == 0)
      thresh_feats <- names(which.max(feat_cors_ok))

    disc_train_st <- train_disc
    disc_train_st[feat_cols] <- lapply(disc_train_st[feat_cols], function(x)
      as.factor(as.character(x)))
    disc_train_st$Wine_mhl <- train_residuals
    disc_train_st <- disc_train_st %>% select(all_of(thresh_feats), Wine_mhl)

    drs_st <- suppressMessages(suppressWarnings(
      caren(disc_train_st,
            Dist     = TRUE,
            POI      = 'Wine_mhl',
            min.sup  = carenr_min_sup_hi,
            min.conf = carenr_min_conf)
    ))

    parsed_st <- parse_drs(drs_st)

    train_for_pred <- train_cont %>% mutate(Wine_mhl = train_residuals)

    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0
      predict_carenr_dist(test_obs_j,
                          train_for_pred,
                          parsed_st$rule_fns,
                          carenr_strategy,
                          carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist_Sup_Thresh failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5g. CARENR_DIST_ETA2 — top-N features by eta-squared (η²) from one-way ANOVA
  #     Features are scored on their DISCRETIZED representation (train_disc),
  #     which is exactly what CAREN sees during rule mining.
  #     η² = SS_between / SS_total: how much variance in the residuals is
  #     explained by the feature's bin partition?
  #     This is more theoretically aligned with CAREN's KS test than Pearson.
  # --------------------------------------------------------------------------

  pred_caren_dist_eta2 <- tryCatch({

    feat_cols <- setdiff(names(train_disc), c('year', 'Wine_mhl'))

    # Score each feature by η² on its discretized bins vs continuous residuals
    feat_eta2     <- sapply(feat_cols, function(col)
      compute_eta2(train_disc[[col]], train_residuals))

    # Pick top-N (NA-valued features — constant bins — ranked last)
    top_feats_e2  <- names(sort(feat_eta2, decreasing = TRUE,
                                na.last = TRUE))[seq_len(carenr_n_features)]

    cat(sprintf('  [dist_eta2] top-%d features by η² (max η²=%.3f, min η²=%.3f)\n',
                carenr_n_features,
                max(feat_eta2[top_feats_e2], na.rm = TRUE),
                min(feat_eta2[top_feats_e2], na.rm = TRUE)))

    disc_train_e2 <- train_disc
    disc_train_e2[feat_cols] <- lapply(disc_train_e2[feat_cols], function(x)
      as.factor(as.character(x)))
    disc_train_e2$Wine_mhl <- train_residuals
    disc_train_e2 <- disc_train_e2 %>% select(all_of(top_feats_e2), Wine_mhl)

    drs_e2 <- suppressMessages(suppressWarnings(
      caren(disc_train_e2,
            Dist     = TRUE,
            POI      = 'Wine_mhl',
            min.sup  = carenr_min_sup,
            min.conf = carenr_min_conf)
    ))

    parsed_e2 <- parse_drs(drs_e2)

    train_for_pred <- train_cont %>% mutate(Wine_mhl = train_residuals)

    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0
      predict_carenr_dist(test_obs_j,
                          train_for_pred,
                          parsed_e2$rule_fns,
                          carenr_strategy,
                          carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist_Eta2 failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5h. CARENR_DIST_LAG — best config (Sup_Thresh) + ACF-selected lag features
  #
  #  Instead of always using lag 1, we:
  #    1. Compute the ACF of training residuals up to carenr_lag_max years
  #    2. Keep only lags whose |ACF| exceeds the 95% significance band
  #       (threshold = 2/sqrt(n_train), the standard white-noise test)
  #    3. Fallback to lag 1 if no lag is significant
  #    4. Discretize each selected lag on training data (CAREN format) and
  #       add it alongside the Sup_Thresh climate features
  #
  #  Lag values are always known at prediction time (historical record).
  # --------------------------------------------------------------------------

  pred_caren_dist_lag <- tryCatch({

    feat_cols <- setdiff(names(train_disc), c('year', 'Wine_mhl'))

    # ── 1. ACF-based lag selection ───────────────────────────────────────────
    acf_vals   <- acf(train_residuals, lag.max = carenr_lag_max,
                      plot = FALSE)$acf[-1]          # drop lag-0 (always 1)
    acf_thresh <- 2 / sqrt(n_train)                  # 95% significance band
    sig_lags   <- which(abs(acf_vals) > acf_thresh)
    if (length(sig_lags) == 0) sig_lags <- 1L        # fallback: always lag 1

    cat(sprintf('  [dist_lag] ACF threshold=%.3f  |  significant lags: %s\n',
                acf_thresh, paste(sig_lags, collapse = ', ')))

    # ── 2. Build year → detrended-residual lookup (full dataset, no leakage) ─
    all_res_lookup <- setNames(
      df_cont$Wine_mhl - apply_trend(trend_model, df_cont),
      df_cont$year
    )

    get_lag_val <- function(year_vec, k)
      sapply(year_vec, function(y) {
        key <- as.character(y - k)
        if (key %in% names(all_res_lookup)) all_res_lookup[[key]] else NA_real_
      })

    # ── 3. Compute continuous lag vectors for train and test ─────────────────
    lag_train_cont <- lapply(sig_lags, get_lag_val, year_vec = train_cont$year)
    lag_test_cont  <- lapply(sig_lags, get_lag_val, year_vec = test_cont$year)
    lag_names      <- paste0('lag', sig_lags, '_prod')
    names(lag_train_cont) <- lag_names
    names(lag_test_cont)  <- lag_names

    # ── 4. Discretize each lag using CAREN interval format ───────────────────
    lag_disc_train <- lapply(lag_names, function(nm) {
      vals   <- lag_train_cont[[nm]]
      valid  <- vals[!is.na(vals)]
      breaks <- unique(quantile(valid, probs = seq(0, 1, length.out = n_bins + 1)))
      make_caren_intervals(vals, breaks)   # [lower|upper) strings
    })
    names(lag_disc_train) <- lag_names

    # ── 5. Build disc_train_lag: Sup_Thresh climate features + lag columns ───
    feat_cors_lag    <- sapply(feat_cols, function(col)
      abs(cor(train_cont[[col]], train_residuals, use = 'complete.obs')))
    feat_cors_ok_lag <- feat_cors_lag[!is.na(feat_cors_lag)]
    thresh_feats_lag <- names(feat_cors_ok_lag[feat_cors_ok_lag >= carenr_cor_thresh])
    if (length(thresh_feats_lag) == 0)
      thresh_feats_lag <- names(which.max(feat_cors_ok_lag))

    disc_train_lag <- train_disc
    disc_train_lag[feat_cols] <- lapply(disc_train_lag[feat_cols], function(x)
      as.factor(as.character(x)))
    disc_train_lag$Wine_mhl <- train_residuals

    for (nm in lag_names)
      disc_train_lag[[nm]] <- as.factor(lag_disc_train[[nm]])

    disc_train_lag <- disc_train_lag %>%
      select(all_of(c(thresh_feats_lag, lag_names)), Wine_mhl)

    # Drop rows where ANY lag is NA (CAREN cannot handle missing values)
    any_lag_na <- Reduce(`|`, lapply(lag_names,
                                     function(nm) is.na(disc_train_lag[[nm]])))
    disc_train_lag <- disc_train_lag[!any_lag_na, , drop = FALSE]

    cat(sprintf('  [dist_lag] training rows after NA-lag drop: %d / %d\n',
                nrow(disc_train_lag), n_train))

    # ── 6. Run CAREN ─────────────────────────────────────────────────────────
    # Use lower min.sup here: adding lag columns increases feature dimensionality
    # and reduces training rows (NA-lag drop), making rules harder to find at 0.3.
    # 0.2 lets CAREN find rules while still filtering the weakest ones.
    drs_lag <- suppressMessages(suppressWarnings(
      caren(disc_train_lag,
            Dist     = TRUE,
            POI      = 'Wine_mhl',
            min.sup  = carenr_min_sup,
            min.conf = carenr_min_conf)
    ))

    parsed_lag <- parse_drs(drs_lag)

    # ── 7. Build continuous prediction datasets ───────────────────────────────
    train_for_pred_lag <- train_cont %>% mutate(Wine_mhl = train_residuals)
    for (nm in lag_names)
      train_for_pred_lag[[nm]] <- lag_train_cont[[nm]]

    # ── 8. Predict ────────────────────────────────────────────────────────────
    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0
      for (nm in lag_names)
        test_obs_j[[nm]] <- lag_test_cont[[nm]][j]
      predict_carenr_dist(test_obs_j,
                          train_for_pred_lag,
                          parsed_lag$rule_fns,
                          carenr_strategy,
                          carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist_Lag failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5i. CARENR_DIST_CONF — confidence-weighted prediction
  #     Same config as Sup_Thresh (best CarenR so far): |cor| threshold +
  #     min.sup = 0.3. Reuses parsed_st rules — no extra CAREN call needed.
  #     Weight for each firing rule = -log(pvalue): rules that are more
  #     statistically significant (lower KS p-value) contribute more to the
  #     final prediction. Contrast with strategy C (support-weighted).
  # --------------------------------------------------------------------------

  pred_caren_dist_conf <- tryCatch({

    # Reuse parsed_st from step 5f (same CAREN run, different weighting).
    # If Sup_Thresh produced no rules for this scenario, predict_carenr_dist
    # already falls back to subgroup_agg(train_data$Wine_mhl) — no special
    # handling needed here.
    train_for_pred <- train_cont %>% mutate(Wine_mhl = train_residuals)

    rule_fns_conf  <- if (!is.null(parsed_st$rule_fns)) parsed_st$rule_fns else list()
    conf_w         <- if (!is.null(parsed_st$conf_weights)) parsed_st$conf_weights else numeric(0)

    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0
      predict_carenr_dist(test_obs_j,
                          train_for_pred,
                          rule_fns_conf,
                          strategy       = 'D',          # weighted sum, custom weights
                          min_subgroup   = carenr_min_subgroup,
                          agg_fn         = subgroup_agg,
                          rule_weights   = conf_w)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist_Conf failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5j. CARENR_DIST_STACK — two-stage stacked model
  #
  #  Stage 1 (CarenR):  apply Sup_Thresh rules to get a climate-based
  #                      prediction for each observation (train + test).
  #
  #  Stage 2 (Regression): fit
  #    actual_residual ~ caren_pred + lag_k1 + lag_k2 + ...
  #  on training data, where lags are ACF-selected (same logic as step 5h).
  #  The regression learns the optimal linear combination of the climate signal
  #  (caren_pred) and the production memory (lag values).
  #
  #  At test time: apply the fitted regression to caren_pred + lag values.
  #
  #  Note: caren_pred on training data is in-sample (rules mined from the
  #  same data). This is slightly optimistic for the regression stage, but the
  #  final evaluation is still honest (test set is always held out).
  # --------------------------------------------------------------------------

  pred_caren_dist_stack <- tryCatch({

    # ── 1. ACF-based lag selection (same as step 5h) ──────────────────────────
    acf_vals   <- acf(train_residuals, lag.max = carenr_lag_max,
                      plot = FALSE)$acf[-1]
    acf_thresh <- 2 / sqrt(n_train)
    sig_lags   <- which(abs(acf_vals) > acf_thresh)
    if (length(sig_lags) == 0) sig_lags <- 1L

    # ── 2. Build year → residual lookup (full dataset, training trend) ─────────
    all_res_lookup <- setNames(
      df_cont$Wine_mhl - apply_trend(trend_model, df_cont),
      df_cont$year
    )
    get_lag_val <- function(year_vec, k)
      sapply(year_vec, function(y) {
        key <- as.character(y - k)
        if (key %in% names(all_res_lookup)) all_res_lookup[[key]] else NA_real_
      })

    # ── 3. Compute lag matrices ────────────────────────────────────────────────
    # Use do.call(cbind) so that n_test=1 (last scenario) always yields a
    # proper matrix (sapply collapses to a vector when n_test=1).
    lag_train_mat <- do.call(cbind, lapply(sig_lags, get_lag_val,
                                           year_vec = train_cont$year))
    lag_test_mat  <- do.call(cbind, lapply(sig_lags, get_lag_val,
                                           year_vec = test_cont$year))
    if (is.null(dim(lag_train_mat)))
      lag_train_mat <- matrix(lag_train_mat, nrow = n_train)
    if (is.null(dim(lag_test_mat)))
      lag_test_mat  <- matrix(lag_test_mat,  nrow = n_test)
    colnames(lag_train_mat) <- paste0('lag', sig_lags)
    colnames(lag_test_mat)  <- paste0('lag', sig_lags)

    # ── 4. In-sample CarenR predictions on training data ──────────────────────
    # Apply Sup_Thresh rules to each training observation.
    rule_fns_st   <- if (!is.null(parsed_st$rule_fns)) parsed_st$rule_fns else list()
    train_for_st  <- train_cont %>% mutate(Wine_mhl = train_residuals)

    caren_train_pred <- sapply(seq_len(n_train), function(i) {
      obs_i          <- train_cont[i, , drop = FALSE]
      obs_i$Wine_mhl <- 0
      predict_carenr_dist(obs_i, train_for_st, rule_fns_st,
                          carenr_strategy, carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    })

    # ── 5. Fit stacking regression on training data ────────────────────────────
    stack_train <- data.frame(y = train_residuals,
                              caren = caren_train_pred,
                              lag_train_mat)
    stack_train <- stack_train[complete.cases(stack_train), ]   # drop NA-lag rows

    # Drop caren from formula if it is constant (no rules fired on training data
    # → all values equal median → singular matrix → NA coefficient)
    caren_has_signal <- sd(stack_train$caren, na.rm = TRUE) > 1e-6
    stack_formula    <- if (caren_has_signal) y ~ . else y ~ . - caren
    stack_lm         <- lm(stack_formula, data = stack_train)

    cat(sprintf('  [dist_stack] lags: %s  |  caren signal: %s  |  rows: %d/%d\n',
                paste(sig_lags, collapse = ', '),
                if (caren_has_signal) 'yes' else 'no (dropped)',
                nrow(stack_train), n_train))
    cat('  [dist_stack] coef:', paste(names(coef(stack_lm)),
                                       round(coef(stack_lm), 3),
                                       sep = '=', collapse = '  '), '\n')

    # ── 6. CarenR predictions on test data ────────────────────────────────────
    caren_test_pred <- sapply(seq_len(n_test), function(j) {
      obs_j          <- test_cont[j, , drop = FALSE]
      obs_j$Wine_mhl <- 0
      predict_carenr_dist(obs_j, train_for_st, rule_fns_st,
                          carenr_strategy, carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    })

    # ── 7. Apply stacking regression to test ──────────────────────────────────
    stack_test <- data.frame(caren = caren_test_pred, lag_test_mat)
    stack_pred <- predict(stack_lm, newdata = stack_test)

    # Fallback for any NA predictions (missing lag) → use CarenR prediction
    na_idx <- is.na(stack_pred)
    if (any(na_idx)) stack_pred[na_idx] <- caren_test_pred[na_idx]

    as.numeric(stack_pred) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist_Stack failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5k. CARENR_DIST_SUP_FS — top-N feature selection + higher min.sup
  #     Designed as the LOESS-compatible equivalent of Sup_Thresh:
  #     avoids threshold-selection collapse (where |r|>0.30 leaves too few
  #     features) by using fixed top-N=carenr_n_features instead.
  #     Uses min.sup = carenr_min_sup_hi (0.3) for broader rules.
  # --------------------------------------------------------------------------

  pred_caren_dist_sup_fs <- tryCatch({

    feat_cols <- setdiff(names(train_disc), c('year', 'Wine_mhl'))

    feat_cors   <- sapply(feat_cols, function(col)
      abs(cor(train_cont[[col]], train_residuals, use = 'complete.obs')))
    top_feats   <- names(sort(feat_cors, decreasing = TRUE))[seq_len(carenr_n_features)]

    disc_train_sf <- train_disc
    disc_train_sf[feat_cols] <- lapply(disc_train_sf[feat_cols], function(x)
      as.factor(as.character(x)))
    disc_train_sf$Wine_mhl <- train_residuals
    disc_train_sf <- disc_train_sf %>% select(all_of(top_feats), Wine_mhl)

    drs_sf <- suppressMessages(suppressWarnings(
      caren(disc_train_sf,
            Dist     = TRUE,
            POI      = 'Wine_mhl',
            min.sup  = carenr_min_sup_hi,   # 0.3 — broader rules
            min.conf = carenr_min_conf)
    ))

    parsed_sf <- parse_drs(drs_sf)

    train_for_pred <- train_cont %>% mutate(Wine_mhl = train_residuals)

    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0
      predict_carenr_dist(test_obs_j,
                          train_for_pred,
                          parsed_sf$rule_fns,
                          carenr_strategy,
                          carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Dist_Sup_FS failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 5l. CARENR_SUPERVISED — per-fold supervised discretization
  #
  #  Motivation: the pre-computed df_disc uses globally fixed equal-frequency
  #  tertile cut points (fitted on all 80 years), causing two problems:
  #    (a) Data leakage — test-year feature values influence the cut points.
  #    (b) Unsupervised — bins ignore the production target entirely.
  #
  #  This variant fixes both:
  #    1. Cut points are computed inside the scenario loop on training data ONLY.
  #    2. Cuts are SUPERVISED: for each feature, greedy binary recursive
  #       splitting maximises between-group SS of training residuals
  #       (find_supervised_cuts above).  This places splits where features
  #       best separate low-production from high-production years.
  #    3. Feature selection: top-N by Pearson |r| (same as Sup_FS).
  #
  #  Prediction path is unchanged: parse_one_condition converts CAREN interval
  #  strings back to numeric bounds, so rules are evaluated against continuous
  #  test feature values — no test-set discretization needed.
  # --------------------------------------------------------------------------

  pred_caren_supervised <- tryCatch({

    feat_cols <- setdiff(names(train_cont), c('year', 'Wine_mhl'))

    # Feature selection: Pearson |r| on continuous training features
    feat_cors <- sapply(feat_cols, function(col)
      abs(cor(train_cont[[col]], train_residuals, use = 'complete.obs')))
    feat_cors <- feat_cors[!is.na(feat_cors)]
    top_feats <- names(sort(feat_cors, decreasing = TRUE))[seq_len(carenr_n_features)]

    # Per-fold supervised discretization: cut points from training data + residuals
    disc_train_sv <- data.frame(Wine_mhl = train_residuals)
    for (feat in top_feats) {
      cuts    <- find_supervised_cuts(train_cont[[feat]], train_residuals, n_bins = 3)
      breaks  <- unique(sort(c(-Inf, cuts, Inf)))   # unique() guards against degenerate data
      if (length(breaks) < 3) next                  # skip feature if still degenerate
      disc_train_sv[[feat]] <- as.factor(
        make_caren_intervals(train_cont[[feat]], breaks))
    }

    drs_sv <- suppressMessages(suppressWarnings(
      caren(disc_train_sv,
            Dist     = TRUE,
            POI      = 'Wine_mhl',
            min.sup  = carenr_min_sup,
            min.conf = carenr_min_conf)
    ))

    parsed_sv <- parse_drs(drs_sv)

    n_rules_sv <- if (!is.null(parsed_sv$rules_df)) nrow(parsed_sv$rules_df) else 0
    cat(sprintf('  [supervised] rules found: %d\n', n_rules_sv))

    train_for_pred <- train_cont %>% mutate(Wine_mhl = train_residuals)

    sapply(seq_len(n_test), function(j) {
      test_obs_j          <- test_cont[j, , drop = FALSE]
      test_obs_j$Wine_mhl <- 0
      predict_carenr_dist(test_obs_j,
                          train_for_pred,
                          parsed_sv$rule_fns,
                          carenr_strategy,
                          carenr_min_subgroup,
                          agg_fn = subgroup_agg)
    }) + trend_offsets

  }, error = function(e) {
    cat('  [!] CarenR_Supervised failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 6. M5RULES
  # --------------------------------------------------------------------------

  pred_m5 <- tryCatch({
    ctrl     <- if (m5_unsmoothed) Weka_control(N = TRUE) else Weka_control()
    m5_model <- M5Rules(Wine_mhl ~ ., data = train_features, control = ctrl)
    as.numeric(predict(m5_model, test_features)) + trend_offsets
  }, error = function(e) {
    cat('  [!] M5Rules failed on scenario', s, ':', conditionMessage(e), '\n')
    rep(NA_real_, n_test)
  })

  # --------------------------------------------------------------------------
  # 7. METRICS for this scenario
  # --------------------------------------------------------------------------

  actual_mhl <- test_cont$Wine_mhl

  # Ripper returns a list (pred/lower/upper); extract pred for metrics
  models_preds <- list(
    Naive                  = pred_naive,
    Naive_Median           = pred_naive_median,
    Naive_MA               = pred_naive_ma,
    DT                     = pred_dt,
    Ripper                 = pred_ripper$pred,
    CarenR_Dist            = pred_caren_dist,
    CarenR_Dist_FS         = pred_caren_dist_fs,
    CarenR_Dist_Thresh     = pred_caren_dist_thresh,
    CarenR_Dist_Sup        = pred_caren_dist_sup,
    CarenR_Dist_Sup_Thresh = pred_caren_dist_sup_thresh,
    CarenR_Dist_Eta2       = pred_caren_dist_eta2,
    CarenR_Dist_Lag        = pred_caren_dist_lag,
    CarenR_Dist_Conf       = pred_caren_dist_conf,
    CarenR_Dist_Stack      = pred_caren_dist_stack,
    CarenR_Dist_Sup_FS     = pred_caren_dist_sup_fs,
    CarenR_Supervised      = pred_caren_supervised,
    M5Rules                = pred_m5
  )

  metrics_s <- lapply(names(models_preds), function(m) {
    p <- models_preds[[m]]
    mtr <- compute_metrics(actual_mhl, p)
    data.frame(
      scenario       = s,
      n_train        = n_train,
      train_end_year = max(train_cont$year),
      test_start_year= min(test_cont$year),
      n_test         = n_test,
      model          = m,
      mae            = round(mtr['mae'],  3),
      rmse           = round(mtr['rmse'], 3),
      r2             = round(mtr['r2'],   4),
      row.names      = NULL,
      stringsAsFactors = FALSE
    )
  })

  all_metrics[[s]] <- bind_rows(metrics_s)

  # --------------------------------------------------------------------------
  # 8. PREDICTIONS for this scenario
  # --------------------------------------------------------------------------

  # Retrieve CI bounds: Ripper has them, all regression models get NA
  get_bounds <- function(m, col) {
    if (m == 'Ripper') pred_ripper[[col]] else rep(NA_real_, n_test)
  }

  preds_s <- bind_rows(lapply(names(models_preds), function(m) {
    data.frame(
      scenario        = s,
      n_train         = n_train,
      train_end_year  = max(train_cont$year),
      year            = test_cont$year,
      actual          = actual_mhl,
      model           = m,
      predicted       = models_preds[[m]],
      pred_lower      = get_bounds(m, 'lower'),
      pred_upper      = get_bounds(m, 'upper'),
      stringsAsFactors= FALSE
    )
  }))

  all_predictions[[s]] <- preds_s
}

cat('\nAll scenarios complete.\n\n')


# ==============================================================================
# AGGREGATE RESULTS
# ==============================================================================

metrics_all     <- bind_rows(all_metrics)
predictions_all <- bind_rows(all_predictions)

# Print summary table (last scenario = most training data)
last_s <- max(metrics_all$scenario)
cat('============================================================\n')
cat(' Final Scenario (n_train =', max(metrics_all$n_train), ') — Metrics\n')
cat('============================================================\n')
cat(sprintf('%-10s %10s %10s %10s\n', 'Model', 'MAE', 'RMSE', 'R²'))
cat(strrep('-', 44), '\n')
metrics_all %>%
  filter(scenario == last_s) %>%
  arrange(mae) %>%
  { cat(sprintf('%-10s %10.3f %10.3f %10.4f\n',
                .$model, .$mae, .$rmse, .$r2)) }
cat(strrep('-', 44), '\n')

# Best model across all scenarios
# ─ mean_mae    : simple average of per-scenario MAE (each scenario counts once)
# ─ sd_mae      : standard deviation of per-scenario MAE (spread / stability)
# ─ weighted_mae: MAE weighted by test-set size (larger test windows count more)
#                 equivalent to pooling all predictions and computing one global MAE
# ─ mean_r2     : average R² excluding degenerate single-obs scenarios (R²=-Inf)
best_overall <- metrics_all %>%
  group_by(model) %>%
  summarise(
    mean_mae     = mean(mae, na.rm = TRUE),
    sd_mae       = sd(mae,   na.rm = TRUE),
    weighted_mae = weighted.mean(mae, w = n_test, na.rm = TRUE),
    mean_r2      = mean(r2[is.finite(r2)], na.rm = TRUE),
    .groups      = 'drop'
  ) %>%
  arrange(weighted_mae)

cat('\n')
cat(strrep('=', 70), '\n')
cat(' Summary across all scenarios — sorted by weighted MAE\n')
cat(strrep('=', 70), '\n')
cat(sprintf('%-26s %10s %8s %14s %10s\n',
            'Model', 'Mean MAE', 'SD MAE', 'Weighted MAE', 'Mean R²'))
cat(strrep('-', 70), '\n')
for (i in seq_len(nrow(best_overall))) {
  r <- best_overall[i, ]
  cat(sprintf('%-26s %10.1f %8.1f %14.1f %10.4f\n',
              r$model, r$mean_mae, r$sd_mae, r$weighted_mae,
              ifelse(is.finite(r$mean_r2), r$mean_r2, NA_real_)))
}
cat(strrep('-', 70), '\n')
cat('\n  mean_mae    = unweighted average (each scenario counts equally)\n')
cat('  sd_mae      = spread of MAE across scenarios (lower = more stable)\n')
cat('  weighted_mae= test-size weighted average (larger test sets count more)\n')
cat('  mean_r2     = mean R² on finite scenarios only (excl. n_test=1 cases)\n\n')


# ==============================================================================
# SAVE OUTPUTS
# ==============================================================================

out_dir <- file.path('data', if (detrend) detrend_method else 'no_detrend')
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(metrics_all,
          file.path(out_dir, paste0('scenario_metrics_', tolower(region), '.csv')),
          row.names = FALSE)

write.csv(predictions_all,
          file.path(out_dir, paste0('scenario_predictions_', tolower(region), '.csv')),
          row.names = FALSE)

write.csv(best_overall %>% mutate(region = region, detrend_method = if (detrend) detrend_method else 'none'),
          file.path(out_dir, paste0('scenario_summary_', tolower(region), '.csv')),
          row.names = FALSE)

cat('\nSaved: scenario_metrics_', tolower(region), '.csv\n', sep = '')
cat('Saved: scenario_predictions_', tolower(region), '.csv\n', sep = '')
cat('Saved: scenario_summary_', tolower(region), '.csv\n', sep = '')

# DT rules log ----------------------------------------------------------------

dt_rules_file <- file.path(out_dir, paste0('dt_rules_', tolower(region), '.txt'))

header <- paste0(
  "Decision Tree Rules — ", region, "\n",
  "Generated : ", format(Sys.time(), '%Y-%m-%d %H:%M'), "\n",
  "Detrend   : ", if (detrend) detrend_method else 'none', "\n",
  "step_years: ", step_years, "\n",
  "dt_cp     : ", dt_cp, "  |  dt_minsplit: ", dt_minsplit, "\n",
  "\nNote: predictions are in RESIDUAL units (mhl deviation from trend).\n",
  "Positive = above-trend  |  Negative = below-trend\n",
  strrep("=", 60), "\n"
)

writeLines(c(header, dt_rules_log), dt_rules_file)
cat('Saved: dt_rules_', tolower(region), '.txt\n', sep = '')


# ==============================================================================
# PLOT 1 — Learning Curves  (MAE vs n_train per model)
# ==============================================================================

model_colours <- c(
  Naive                  = "grey60",
  Naive_Median           = "grey35",
  Naive_MA               = "grey15",
  DT                     = "#E15759",
  Ripper                 = "#B07AA1",
  CarenR_Dist            = "#76B7B2",
  CarenR_Dist_FS         = "#59A14F",
  CarenR_Dist_Thresh     = "#EDC948",
  CarenR_Dist_Sup        = "#4E79A7",
  CarenR_Dist_Sup_Thresh = "#F28E2B",
  CarenR_Dist_Eta2       = "#D62728",
  CarenR_Dist_Lag        = "#17BECF",  # lag production feature
  CarenR_Dist_Conf       = "#8C564B",  # confidence-weighted (pvalue)
  CarenR_Dist_Stack      = "#FF7F0E",  # stacked: CarenR + lag regression
  M5Rules                = "#9C755F"
)

# Smooth with a loess line when there are many scenarios
p_lc <- metrics_all %>%
  filter(model != 'Naive') %>%
  ggplot(aes(x = n_train, y = mae, colour = model)) +
  { if (n_scenarios > 10)
      geom_smooth(method = 'loess', span = 0.4, se = FALSE, linewidth = 1.0)
    else
      geom_line(linewidth = 0.9)
  } +
  geom_point(size = 1.6, alpha = 0.55) +
  geom_hline(data = metrics_all %>%
               filter(model == 'Naive') %>%
               group_by(n_train) %>%
               summarise(mae = mean(mae)),
             aes(yintercept = mae), colour = 'grey60',
             linetype = 'dashed', linewidth = 0.7) +
  scale_colour_manual(values = model_colours, name = 'Model') +
  labs(
    title    = paste('Learning Curve — MAE vs Training Size —', region),
    subtitle = paste0('Dashed = Naive baseline  |  Detrend: ', detrend_method,
                      '  |  min_train: ', round(min_train_pct*100), '%  |  step_years: ', step_years),
    x = 'Training observations (n)',
    y = 'MAE (mhl)'
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = 'bottom',
        plot.subtitle    = element_text(colour = 'grey50'))

ggsave(file.path(out_dir, paste0('plot_learning_curve_', tolower(region), '.png')),
       p_lc, width = 10, height = 6, dpi = 150)
cat('Saved: plot_learning_curve_', tolower(region), '.png\n', sep = '')


# Same for R²
p_lc_r2 <- metrics_all %>%
  filter(model != 'Naive') %>%
  ggplot(aes(x = n_train, y = r2, colour = model)) +
  { if (n_scenarios > 10)
      geom_smooth(method = 'loess', span = 0.4, se = FALSE, linewidth = 1.0)
    else
      geom_line(linewidth = 0.9)
  } +
  geom_point(size = 1.6, alpha = 0.55) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey60', linewidth = 0.7) +
  scale_colour_manual(values = model_colours, name = 'Model') +
  labs(
    title    = paste('Learning Curve — R² vs Training Size —', region),
    subtitle = paste0('Dashed = 0  |  Detrend: ', detrend_method,
                      '  |  min_train: ', round(min_train_pct*100), '%  |  step_years: ', step_years),
    x = 'Training observations (n)',
    y = 'R²'
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = 'bottom',
        plot.subtitle    = element_text(colour = 'grey50'))

ggsave(file.path(out_dir, paste0('plot_learning_curve_r2_', tolower(region), '.png')),
       p_lc_r2, width = 10, height = 6, dpi = 150)
cat('Saved: plot_learning_curve_r2_', tolower(region), '.png\n', sep = '')


# ==============================================================================
# PLOT 2 — Horizon Plot  (predictions over test window for one scenario)
# Answers: "how do predictions behave over longer forecast horizons?"
# ==============================================================================

# Choose scenario to display
hz_idx <- if (!is.null(horizon_scenario)) {
  horizon_scenario
} else {
  # Default: scenario closest to the middle of the training range
  which.min(abs(train_ends - median(train_ends)))
}
hz_idx <- max(1, min(hz_idx, n_scenarios))   # clamp

hz_data <- predictions_all %>% filter(scenario == hz_idx)

hz_train_end <- unique(hz_data$train_end_year)
hz_n_train   <- unique(hz_data$n_train)

p_hz <- ggplot(hz_data, aes(x = year)) +
  geom_line(aes(y = actual), colour = 'black', linewidth = 1.0, alpha = 0.6) +
  geom_point(aes(y = actual), colour = 'black', size = 1.5, alpha = 0.6) +
  geom_line(aes(y = predicted, colour = model), linewidth = 0.75) +
  geom_point(aes(y = predicted, colour = model), size = 1.5, alpha = 0.8) +
  geom_vline(xintercept = hz_train_end + 0.5,
             linetype = 'dotted', colour = 'grey40', linewidth = 0.8) +
  annotate('text', x = hz_train_end + 1, y = Inf, vjust = 1.5, hjust = 0,
           label = paste('← train', hz_n_train, 'obs  |  test →'),
           size = 3, colour = 'grey40') +
  scale_colour_manual(values = model_colours, name = 'Model') +
  facet_wrap(~ model, ncol = 2) +
  labs(
    title    = paste0('Forecast Horizon — Scenario ', hz_idx,
                      ' (train up to ', hz_train_end, ') — ', region),
    subtitle = paste0('Black = actual  |  Coloured = predicted  |  Detrend: ',
                      detrend_method),
    x = 'Year', y = 'Wine production (mhl)'
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = 'none',
        strip.text       = element_text(face = 'bold'),
        plot.subtitle    = element_text(colour = 'grey50'))

ggsave(file.path(out_dir, paste0('plot_horizon_', tolower(region), '.png')),
       p_hz, width = 12, height = 8, dpi = 150)
cat('Saved: plot_horizon_', tolower(region), '.png\n', sep = '')


# ==============================================================================
# Also update scenario_metrics to include detrend_method for script 8
# ==============================================================================

metrics_all$detrend_method <- if (detrend) detrend_method else 'none'

write.csv(metrics_all,
          file.path(out_dir, paste0('scenario_metrics_', tolower(region), '.csv')),
          row.names = FALSE)

cat('\nDone.\n')

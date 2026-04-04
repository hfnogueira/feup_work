# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                   Walk-Forward Validation Pipeline
#                   by:  hugonogueira
#
#  Evaluates predictive performance of 5 methods using expanding walk-forward
#  validation: train on all years up to T-1, predict year T.
#
#  Methods compared:
#    1. Naive    — always predict the training mean
#    2. CarenR   — rules loaded from CSV; prediction by chosen strategy
#    3. RIPPER   — retrain JRip each fold, predict class → class training mean
#    4. M5Rules  — retrain M5Rules each fold, predict continuously
#    5. Dtree    — retrain rpart each fold, predict leaf mean
#
#  CarenR prediction strategies (set carenr_strategy below):
#    'A' — First matching rule only      (mimics RIPPER behaviour)
#    'B' — Average of ALL matching rules (more robust)
#    'C' — Support-weighted average      (strongest academically)
#           weight = fraction of training obs that match each rule
#           prediction = sum(pred_i * w_i) / sum(w_i)
#
#  Note on CarenR:
#    Rule CONDITIONS are pre-discovered on the full dataset (minor lookahead).
#    Subgroup MEANS and support weights are re-estimated from training data
#    only at each fold, preserving temporal integrity for the prediction step.
#
#  Rules files (place in distribution_rules/ folder):
#    RulesTemp_rvv.csv  — CarenR output for RVV
#    RulesTemp_rdd.csv  — CarenR output for RDD
#
#  Output:
#    ../data/walkforward_results_{region}.csv
#    ../data/walkforward_metrics_{region}.csv
#    ../data/walkforward_predictions_{region}.png
#    ../data/walkforward_errors_{region}.png
#
#  Requires: tidyverse, RWeka, rpart
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# libs --------------------------------------------------------------------------------

library(tidyverse)

if (!requireNamespace("RWeka",  quietly = TRUE)) install.packages("RWeka")
if (!requireNamespace("rpart",  quietly = TRUE)) install.packages("rpart")

library(RWeka)
library(rpart)


# config ------------------------------------------------------------------------------

file             <- 2      # 1 = RDD  |  2 = RVV
min_train        <- 25     # minimum training observations before first prediction
seed             <- 42

# CarenR prediction strategy:
#   'A' = first matching rule
#   'B' = average of all matching rules
#   'C' = support-weighted average of all matching rules
carenr_strategy     <- 'C'
carenr_min_subgroup <- 3   # min training obs matching a rule to use it

# RIPPER (same params as 4_ripper_pipeline.R)
n_classes <- 3             # Low / Medium / High
jrip_N    <- 4.0
jrip_O    <- 3
jrip_F    <- 3

# Decision Tree
dt_cp       <- 0.01
dt_minsplit <- 10


# load data ---------------------------------------------------------------------------

if (file == 1) {
   df          <- read.csv('../data/dataPrep_cont_dataset_rdd.csv')
   region      <- 'RDD'
   rules_file  <- '../RulesTemp_rdd.csv'
} else {
   df          <- read.csv('../data/dataPrep_cont_dataset_rvv.csv')
   region      <- 'RVV'
   rules_file  <- '../RulesTemp_rvv.csv'
}

df <- df %>% arrange(year)

cat('------------------------------------------------------------\n')
cat('Region:', region, '| Observations:', nrow(df), '\n')
cat('Year range:', min(df$year), '-', max(df$year), '\n')
cat('Global mean Wine_mhl:', round(mean(df$Wine_mhl), 2), 'mhl\n')
cat('Walk-forward folds:', nrow(df) - min_train, '\n')
cat('CarenR strategy:', carenr_strategy, '\n')
cat('------------------------------------------------------------\n')


# ==============================================================================
# CARENR RULE PARSER
# Loads rules from a CarenR CSV output file and converts the Subgroup column
# into evaluable R functions — no hard-coded thresholds needed.
#
# CSV format (semicolon-separated):
#   Ant_sup ; pvalue ; Kurt ; Skew ; Mean ; Mode ; Median ; Stdev ; Dist ; Subgroup
#
# Subgroup example:
#   iaf_hv_y1_c10=[0|0.915)  &  swa_hv_y0_a20=[25.4|88.4)
#
# Each condition: feature=[lower|upper)  or  feature=[lower|upper]
#   [   always means >= (left always inclusive in CarenR output)
#   )   means <  (right exclusive)
#   ]   means <= (right inclusive)
# ==============================================================================

# Safe column accessor — handles dots in column names
v <- function(obs, col) obs[[col]]

# Parse one condition string → R code string
parse_one_condition <- function(cond) {
   cond <- trimws(cond)

   # Split feature name from interval at the first "=["
   split_pos  <- regexpr("=\\[", cond)
   feature    <- substr(cond, 1, split_pos - 1)
   interval   <- substr(cond, split_pos + 1, nchar(cond))   # e.g. "[0.915|2.52]"

   close_incl <- endsWith(interval, "]")                     # ] → <=, ) → <
   right_op   <- if (close_incl) "<=" else "<"

   inner      <- gsub("[\\[\\]()\\ ]", "", interval)        # strip brackets/spaces
   parts      <- strsplit(inner, "\\|")[[1]]
   lower      <- as.numeric(parts[1])
   upper      <- as.numeric(parts[2])

   paste0('v(obs,"', feature, '") >= ', lower,
          ' & v(obs,"', feature, '") ', right_op, ' ', upper)
}

# Parse a full Subgroup string → function(obs) → TRUE/FALSE
parse_rule_fn <- function(subgroup_str) {
   conditions <- strsplit(subgroup_str, "\\s+&\\s+")[[1]]
   r_parts    <- vapply(trimws(conditions), parse_one_condition, character(1))
   full_expr  <- paste(r_parts, collapse = " & ")
   eval(parse(text = paste0("function(obs) { ", full_expr, " }")))
}

# Load rules CSV → list(rules_df, rule_fns)
load_carenr_rules <- function(path) {
   if (!file.exists(path)) {
      warning("Rules file not found: ", path,
              "\n  CarenR will fall back to training mean (same as Naive).")
      return(list(rules_df = NULL, rule_fns = list()))
   }

   rules_df <- read.csv(path, sep = ";", dec = ".", stringsAsFactors = FALSE)

   # Remove global rule (Ant_sup == 1) and sort by p-value ascending
   rules_df <- rules_df %>%
      mutate(Ant_sup = as.numeric(Ant_sup),
             pvalue  = as.numeric(pvalue),
             Mean    = as.numeric(Mean)) %>%
      filter(Ant_sup < 1.0) %>%
      arrange(pvalue)

   # Parse each Subgroup string into an R function
   rule_fns <- lapply(seq_len(nrow(rules_df)), function(i) {
      tryCatch(
         parse_rule_fn(rules_df$Subgroup[i]),
         error = function(e) {
            warning("Could not parse rule ", i, ": ", rules_df$Subgroup[i])
            function(obs) FALSE
         }
      )
   })

   cat('Loaded', nrow(rules_df), 'CarenR rules from', path, '\n')
   list(rules_df = rules_df, rule_fns = rule_fns)
}

carenr <- load_carenr_rules(rules_file)


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# CarenR prediction -----------------------------------------------------------
# For all rules that fire on the test observation:
#   - Compute the training subgroup mean  (no lookahead in the value)
#   - Compute the training support weight (no lookahead in the weight)
# Then combine according to the chosen strategy.

predict_carenr <- function(test_obs, train_data,
                           rule_fns, strategy, min_subgroup) {

   if (length(rule_fns) == 0) return(mean(train_data$Wine_mhl))

   preds   <- numeric(0)
   weights <- numeric(0)

   for (fn in rule_fns) {

      fires <- tryCatch(isTRUE(fn(test_obs)), error = function(e) FALSE)
      if (!fires) next

      # Which training observations also satisfy this rule?
      train_match <- vapply(seq_len(nrow(train_data)), function(j) {
         tryCatch(isTRUE(fn(train_data[j, , drop = FALSE])),
                  error = function(e) FALSE)
      }, logical(1))

      n_match <- sum(train_match)
      if (n_match < min_subgroup) next

      preds   <- c(preds,   mean(train_data$Wine_mhl[train_match]))
      weights <- c(weights, n_match / nrow(train_data))   # training support

      if (strategy == 'A') break   # first match only → stop here
   }

   if (length(preds) == 0) return(mean(train_data$Wine_mhl))

   switch(strategy,
          'A' = preds[1],
          'B' = mean(preds),
          'C' = sum(preds * weights) / sum(weights)
   )
}


# RIPPER prediction -----------------------------------------------------------
# Retrain JRip at every fold; discretise target using training quantiles only.
# Map predicted class → class mean from training data.

predict_ripper <- function(test_obs, train_data,
                           n_classes, jrip_N, jrip_O, jrip_F) {
   tryCatch({
      breaks <- quantile(train_data$Wine_mhl,
                         probs = seq(0, 1, length.out = n_classes + 1))
      breaks[1]             <- -Inf
      breaks[n_classes + 1] <-  Inf
      cls_labels  <- c('Low', 'Medium', 'High')[seq_len(n_classes)]

      train_cls   <- cut(train_data$Wine_mhl, breaks = breaks,
                         labels = cls_labels, include.lowest = TRUE)
      cls_means   <- tapply(train_data$Wine_mhl, train_cls, mean, na.rm = TRUE)

      train_df          <- train_data %>% select(-year)
      train_df$Wine_mhl <- factor(train_cls, levels = cls_labels)

      ctrl      <- Weka_control(N = jrip_N, O = jrip_O, F = jrip_F)
      model_rip <- JRip(Wine_mhl ~ ., data = train_df, control = ctrl)

      pred_cls  <- as.character(predict(model_rip,
                                        test_obs %>% select(-year, -Wine_mhl)))

      if (pred_cls %in% names(cls_means)) as.numeric(cls_means[pred_cls])
      else mean(train_data$Wine_mhl)

   }, error = function(e) mean(train_data$Wine_mhl))
}


# ==============================================================================
# WALK-FORWARD LOOP
# ==============================================================================

n            <- nrow(df)
test_indices <- (min_train + 1):n
n_test       <- length(test_indices)

cat('\nStarting walk-forward validation...\n')
cat('Test points:', n_test,
    '| Years:', df$year[test_indices[1]], '-', df$year[tail(test_indices, 1)], '\n\n')

results <- data.frame(
   year    = df$year[test_indices],
   actual  = df$Wine_mhl[test_indices],
   naive   = NA_real_,
   carenr  = NA_real_,
   ripper  = NA_real_,
   m5rules = NA_real_,
   dtree   = NA_real_
)

set.seed(seed)
pb_step <- max(1, floor(n_test / 10))

for (i in seq_along(test_indices)) {

   t_idx      <- test_indices[i]
   train_data <- df[seq_len(t_idx - 1), ]
   test_obs   <- df[t_idx, , drop = FALSE]

   if (i %% pb_step == 0 || i == 1 || i == n_test) {
      cat(sprintf('  Fold %3d / %d  (year %d  |  training n = %d)\n',
                  i, n_test, df$year[t_idx], nrow(train_data)))
   }

   # 1. Naive ---------------------------------------------------------------
   results$naive[i] <- mean(train_data$Wine_mhl)

   # 2. CarenR — strategy A / B / C -----------------------------------------
   results$carenr[i] <- predict_carenr(
      test_obs, train_data,
      carenr$rule_fns, carenr_strategy, carenr_min_subgroup
   )

   # 3. RIPPER — retrain → class → class mean --------------------------------
   results$ripper[i] <- predict_ripper(
      test_obs, train_data, n_classes, jrip_N, jrip_O, jrip_F
   )

   # 4. M5Rules — retrain → predict continuously -----------------------------
   tryCatch({
      train_m5        <- train_data %>% select(-year)
      model_m5        <- M5Rules(Wine_mhl ~ ., data = train_m5)
      results$m5rules[i] <- as.numeric(predict(model_m5,
                                               test_obs %>% select(-year, -Wine_mhl)))
   }, error = function(e) {
      results$m5rules[i] <<- mean(train_data$Wine_mhl)
   })

   # 5. Decision Tree — retrain → predict leaf mean --------------------------
   tryCatch({
      train_dt  <- train_data %>% select(-year)
      model_dt  <- rpart(Wine_mhl ~ ., data = train_dt,
                         control = rpart.control(cp = dt_cp, minsplit = dt_minsplit))
      results$dtree[i] <- as.numeric(predict(model_dt,
                                             test_obs %>% select(-year, -Wine_mhl)))
   }, error = function(e) {
      results$dtree[i] <<- mean(train_data$Wine_mhl)
   })
}

cat('\nWalk-forward complete!\n\n')


# ==============================================================================
# METRICS
# ==============================================================================

compute_metrics <- function(actual, predicted) {
   err    <- actual - predicted
   mae    <- mean(abs(err),   na.rm = TRUE)
   rmse   <- sqrt(mean(err^2, na.rm = TRUE))
   ss_res <- sum(err^2, na.rm = TRUE)
   ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
   r2     <- 1 - ss_res / ss_tot
   list(mae = mae, rmse = rmse, r2 = r2)
}

methods <- c('naive', 'carenr', 'ripper', 'm5rules', 'dtree')
labels  <- c('Naive', paste0('CarenR-', carenr_strategy), 'RIPPER', 'M5Rules', 'Dtree')

metrics_table <- data.frame(method = labels,
                             mae    = NA_real_,
                             rmse   = NA_real_,
                             r2     = NA_real_)

cat('============================================================\n')
cat(' Walk-Forward Validation Metrics —', region,
    '| Strategy:', carenr_strategy, '\n')
cat('============================================================\n')
cat(sprintf('%-16s %10s %10s %10s\n', 'Method', 'MAE', 'RMSE', 'R²'))
cat(strrep('-', 50), '\n')

for (i in seq_along(methods)) {
   m <- compute_metrics(results$actual, results[[methods[i]]])
   metrics_table$mae[i]  <- m$mae
   metrics_table$rmse[i] <- m$rmse
   metrics_table$r2[i]   <- m$r2
   cat(sprintf('%-16s %10.1f %10.1f %10.3f\n',
               labels[i], m$mae, m$rmse, m$r2))
}

cat(strrep('-', 50), '\n')
cat('\nBest MAE : ', labels[which.min(metrics_table$mae)],  '\n')
cat('Best RMSE: ', labels[which.min(metrics_table$rmse)], '\n')
cat('Best R²  : ', labels[which.max(metrics_table$r2)],   '\n')


# ==============================================================================
# PLOT 1 — Predictions vs Actuals (faceted)
# ==============================================================================

results_long <- results %>%
   pivot_longer(cols      = all_of(methods),
                names_to  = 'model',
                values_to = 'predicted') %>%
   mutate(model = factor(model, levels = methods, labels = labels))

p_preds <- ggplot(results_long, aes(x = year)) +
   geom_line(aes(y = actual),
             colour = 'black', linewidth = 0.9, alpha = 0.55) +
   geom_line(aes(y = predicted, colour = model), linewidth = 0.6) +
   geom_point(aes(y = predicted, colour = model), size = 1.4, alpha = 0.75) +
   facet_wrap(~model, ncol = 2) +
   scale_colour_brewer(palette = 'Set1') +
   labs(
      title    = paste('Walk-Forward Validation —', region),
      subtitle = paste0('Black = actual  |  Coloured = predicted',
                        '  |  CarenR strategy: ', carenr_strategy),
      x = 'Year', y = 'Wine production (mhl)'
   ) +
   theme_minimal(base_size = 11) +
   theme(legend.position = 'none',
         strip.text    = element_text(face = 'bold'),
         plot.subtitle = element_text(colour = 'grey50'))

ggsave(paste0('../data/walkforward_predictions_', tolower(region), '.png'),
       p_preds, width = 12, height = 9, dpi = 150)

cat('\nSaved: walkforward_predictions_', tolower(region), '.png\n', sep = '')


# ==============================================================================
# PLOT 2 — Absolute Error Boxplots
# ==============================================================================

errors_long <- results_long %>%
   mutate(abs_error = abs(actual - predicted))

naive_mae <- metrics_table$mae[1]   # Naive is always first

p_errors <- ggplot(errors_long, aes(x = model, y = abs_error, fill = model)) +
   geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 1.5) +
   geom_hline(yintercept = naive_mae,
              linetype = 'dashed', colour = 'grey40', linewidth = 0.8) +
   annotate('text', x = 0.55, y = naive_mae * 1.04,
            label = 'Naive MAE', hjust = 0, size = 3.2, colour = 'grey40') +
   scale_fill_brewer(palette = 'Set1') +
   labs(
      title    = paste('Absolute Error Distribution —', region),
      subtitle = 'Lower = better  |  Dashed = Naive MAE baseline',
      x = 'Model', y = '|Error| (mhl)'
   ) +
   theme_minimal(base_size = 11) +
   theme(legend.position = 'none',
         plot.subtitle = element_text(colour = 'grey50'))

ggsave(paste0('../data/walkforward_errors_', tolower(region), '.png'),
       p_errors, width = 9, height = 6, dpi = 150)

cat('Saved: walkforward_errors_', tolower(region), '.png\n', sep = '')


# ==============================================================================
# SAVE RESULTS
# ==============================================================================

write.csv(results,
          paste0('../data/walkforward_results_', tolower(region), '.csv'),
          row.names = FALSE)

write.csv(metrics_table,
          paste0('../data/walkforward_metrics_', tolower(region), '.csv'),
          row.names = FALSE)

cat('\nResults saved:\n')
cat('  ../data/walkforward_results_',  tolower(region), '.csv\n', sep = '')
cat('  ../data/walkforward_metrics_',  tolower(region), '.csv\n', sep = '')
cat('\nDone.\n')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                     MASTER PIPELINE RUNNER
#                     by: hugonogueira
#
#  Runs CarenR, RIPPER, and M5Rules for BOTH regions (RDD + RVV)
#  non-interactively. All outputs are saved to data/{detrend_method}/.
#
#  After this script completes, run script 6 (scenario validation) for
#  each region to get the comparative evaluation results.
#
#  Run from the project root (Thesis/):
#    setwd("/Users/hugonogueira/git_hugo/feup/Thesis")
#    source("src/rscripts/run_all_pipelines.R")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ─── SHARED CONFIG ────────────────────────────────────────────────────────
# All shared params (detrend, detrend_method, loess_span, n_folds, seed)
# live in utils/config.R — single source of truth across every script.

source("src/rscripts/utils/config.R")
source("src/rscripts/utils/detrend_utils.R")


# ─── LIBS ─────────────────────────────────────────────────────────────────

library(carenR)
library(tidyverse)
if (!requireNamespace("RWeka", quietly = TRUE)) install.packages("RWeka")
library(RWeka)


# ─── HELPER: output directory ─────────────────────────────────────────────

out_dir <- file.path("data", if (detrend) detrend_method else "no_detrend")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# CarenR rules are saved alongside RIPPER / M5Rules outputs (same out_dir)
# so every model's artefacts live in one place per detrend method.
carenr_rules_dir <- out_dir


# ═══════════════════════════════════════════════════════════════════════════
#  LOOP OVER REGIONS
# ═══════════════════════════════════════════════════════════════════════════

regions <- list(
  list(id = "RDD", file_cont = "data/dataPrep_cont_dataset_rdd.csv",
                   file_disc = "data/dataPrep_dis_rule_dataset_rdd.csv"),
  list(id = "RVV", file_cont = "data/dataPrep_cont_dataset_rvv.csv",
                   file_disc = "data/dataPrep_dis_rule_dataset_rvv.csv")
)

all_summary <- list()

for (reg in regions) {

  region <- reg$id
  cat("\n", strrep("═", 65), "\n", sep = "")
  cat("  REGION:", region, "\n")
  cat(strrep("═", 65), "\n\n")


  # ─────────────────────────────────────────────────────────────────────────
  # 1. CARENR  (distribution rules)
  # ─────────────────────────────────────────────────────────────────────────

  cat("── CarenR ───────────────────────────────────────────────────\n")

  df_disc <- read.csv(reg$file_disc)

  # Cast all feature columns to factor, Wine_mhl stays numeric
  for (col in names(df_disc)) {
    if (col == "Wine_mhl") {
      df_disc[[col]] <- as.numeric(df_disc[[col]])
    } else {
      df_disc[[col]] <- as.factor(df_disc[[col]])
    }
  }

  cat("Obs:", nrow(df_disc), "| Features:", ncol(df_disc) - 2,
      "(excl. year & Wine_mhl)\n")

  # Detrend
  if (detrend) {
    df_for_trend <- data.frame(
      year     = as.numeric(as.character(df_disc$year)),
      Wine_mhl = df_disc$Wine_mhl
    )
    tr <- get_trend_residuals(df_for_trend, method = detrend_method,
                              span = loess_span, verbose = TRUE)
    df_disc$Wine_mhl_original <- df_disc$Wine_mhl
    df_disc$Wine_mhl          <- tr$residuals
    cat("Residual mean:", round(mean(df_disc$Wine_mhl), 2),
        "| SD:", round(sd(df_disc$Wine_mhl), 2), "\n")
  }

  df_caren <- df_disc %>% select(-year)
  if ("Wine_mhl_original" %in% names(df_caren))
    df_caren <- df_caren %>% select(-Wine_mhl_original)

  # Build distribution rule set
  drs <- caren(df_caren, Dist = TRUE, POI = "Wine_mhl",
               min.sup = 0.2, min.conf = 0.1)

  n_rules <- max(0, nrow(drs) - 1)
  cat("Rules found:", n_rules, "\n")
  cat("Rules with support ≥ 20%:",
      nrow(drs %>% filter(Ant_sup >= 0.2, Ant_sup < 1)), "\n")

  # Save rules CSV (used by script 6)
  # Drop the 'Dist' list column — write.table can't serialize list columns.
  # Script 6 only needs: Subgroup, Ant_sup, pvalue, Mean, Stdev, etc.
  rules_file <- file.path(carenr_rules_dir,
                           paste0("RulesTemp_", tolower(region),
                                  "_", detrend_method, ".csv"))
  
  # write.table cannot serialize list-type columns. Convert any list column
  # to a character string so the CSV is readable by script 6.
  drs_save <- drs
  for (col in names(drs_save)) {
    if (is.list(drs_save[[col]])) {
      drs_save[[col]] <- sapply(drs_save[[col]],
                                function(x) paste(as.character(x), collapse = "|"),
                                USE.NAMES = FALSE)
    }
  }
  write.table(drs_save, rules_file, sep = ";", dec = ".",
              row.names = FALSE, quote = FALSE)
  cat("CarenR rules saved →", rules_file, "\n\n")

  all_summary[[paste0("CarenR_", region)]] <- list(
    region = region, model = "CarenR", n_rules = n_rules
  )


  # ─────────────────────────────────────────────────────────────────────────
  # 2. RIPPER
  # ─────────────────────────────────────────────────────────────────────────

  cat("── RIPPER ───────────────────────────────────────────────────\n")

  df_cont <- read.csv(reg$file_cont)

  n_classes   <- 3
  class_labels <- c("Low", "Medium", "High")
  jrip_N      <- 4.0
  jrip_O      <- 3
  jrip_F      <- 3

  if (detrend) {
    tr_r       <- get_trend_residuals(df_cont, method = detrend_method,
                                      span = loess_span, verbose = FALSE)
    wine_vals  <- tr_r$residuals
  } else {
    wine_vals  <- df_cont$Wine_mhl
  }

  breaks          <- quantile(wine_vals, probs = seq(0, 1, length.out = n_classes + 1), na.rm = TRUE)
  real_breaks     <- breaks
  breaks[1]       <- -Inf
  breaks[length(breaks)] <- Inf

  df_cont$Wine_class <- cut(wine_vals, breaks = breaks,
                            labels = class_labels,
                            include.lowest = TRUE, ordered_result = TRUE)

  cat("Class distribution:\n")
  print(table(df_cont$Wine_class))

  df_ripper <- df_cont %>%
    select(-year, -Wine_mhl) %>%
    mutate(Wine_class = factor(Wine_class, levels = class_labels, ordered = FALSE))

  set.seed(seed)
  model_ripper <- JRip(
    Wine_class ~ .,
    data    = df_ripper,
    control = Weka_control(F = jrip_F, N = jrip_N, O = jrip_O, S = seed)
  )

  cat("\n=== RIPPER Rules ===\n")
  print(model_ripper)

  eval_ripper <- evaluate_Weka_classifier(model_ripper, numFolds = n_folds,
                                          complexity = TRUE, seed = seed)
  cat("\n=== RIPPER CV Results ===\n")
  print(eval_ripper)

  # Per-class metrics
  conf_mat  <- eval_ripper$confusionMatrix
  precision <- diag(conf_mat) / colSums(conf_mat)
  recall    <- diag(conf_mat) / rowSums(conf_mat)
  f1        <- 2 * precision * recall / (precision + recall)
  overall   <- sum(diag(conf_mat)) / sum(conf_mat)

  metrics_ripper <- data.frame(
    Class     = class_labels,
    Precision = round(precision, 3),
    Recall    = round(recall, 3),
    F1        = round(f1, 3)
  )
  cat("\n=== Per-class Precision / Recall / F1 ===\n")
  print(metrics_ripper)

  # Save rules text
  rules_file_r <- file.path(out_dir, paste0("ripper_rules_", tolower(region), ".txt"))
  sink(rules_file_r)
  cat("RIPPER Rules —", region, "\n")
  cat("Generated     :", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("Detrend method:", if (detrend) detrend_method else "none", "\n")
  cat("Target classes:", paste(class_labels, collapse = " / "), "\n")
  cat("Thresholds (mhl):", paste(round(real_breaks, 2), collapse = " | "), "\n\n")
  cat("=== Rules ===\n"); print(model_ripper)
  cat("\n=== Cross-Validation (", n_folds, "folds) ===\n"); print(eval_ripper)
  sink()

  # Save metrics CSV
  metrics_csv_r <- data.frame(
    region = region, detrend_method = if (detrend) detrend_method else "none",
    model = "RIPPER", accuracy = round(overall, 4),
    macro_precision = round(mean(precision, na.rm = TRUE), 4),
    macro_recall    = round(mean(recall,    na.rm = TRUE), 4),
    macro_f1        = round(mean(f1,        na.rm = TRUE), 4)
  )
  write.csv(metrics_csv_r,
            file.path(out_dir, paste0("ripper_metrics_", tolower(region), ".csv")),
            row.names = FALSE)

  # Structured rules CSV (for script 9)
  rules_raw  <- capture.output(print(model_ripper))
  rule_lines <- grep("=>", rules_raw, value = TRUE)
  if (length(rule_lines) > 0) {
    structured_ripper <- bind_rows(lapply(seq_along(rule_lines), function(i) {
      line  <- trimws(rule_lines[i])
      parts <- strsplit(line, "=>", fixed = TRUE)[[1]]
      cond_raw   <- trimws(parts[1])
      conseq_raw <- if (length(parts) >= 2) trimws(parts[2]) else ""
      m <- regmatches(conseq_raw,
             regexec("=(\\w+)\\s*\\(([0-9.]+)/([0-9.]+)\\)", conseq_raw))[[1]]
      n_cov <- if (length(m) >= 3) as.numeric(m[3]) else NA_real_
      n_err <- if (length(m) >= 4) as.numeric(m[4]) else NA_real_
      data.frame(
        rule_index = i, is_default = nchar(cond_raw) == 0,
        conditions_raw = if (nchar(cond_raw) == 0) "(default)" else cond_raw,
        predicted_class = if (length(m) >= 2) m[2] else NA_character_,
        n_covered = n_cov, n_errors = n_err,
        support_pct   = if (!is.na(n_cov)) round(n_cov / nrow(df_ripper) * 100, 1) else NA_real_,
        precision_pct = if (!is.na(n_cov) && n_cov > 0) round((n_cov - n_err) / n_cov * 100, 1) else NA_real_,
        threshold_low_med  = round(real_breaks[2], 3),
        threshold_med_high = round(real_breaks[3], 3),
        target_detrended = detrend, region = region,
        detrend_method = if (detrend) detrend_method else "none",
        stringsAsFactors = FALSE
      )
    }))
    write.csv(structured_ripper,
              file.path(out_dir, paste0("ripper_rules_structured_", tolower(region), ".csv")),
              row.names = FALSE)
  }

  cat("RIPPER outputs saved →", out_dir, "\n\n")
  all_summary[[paste0("RIPPER_", region)]] <- list(
    region = region, model = "RIPPER", accuracy = round(overall, 4),
    macro_f1 = round(mean(f1, na.rm = TRUE), 4)
  )


  # ─────────────────────────────────────────────────────────────────────────
  # 3. M5RULES
  # ─────────────────────────────────────────────────────────────────────────

  cat("── M5Rules ──────────────────────────────────────────────────\n")

  df_m5 <- read.csv(reg$file_cont)

  if (detrend) {
    tr_m <- get_trend_residuals(df_m5, method = detrend_method,
                                span = loess_span, verbose = TRUE)
    df_m5$Wine_mhl_original <- df_m5$Wine_mhl
    df_m5$Wine_mhl          <- tr_m$residuals
    cat("Residual mean:", round(mean(df_m5$Wine_mhl), 2),
        "| SD:", round(sd(df_m5$Wine_mhl), 2), "\n")
  }

  naive_pred  <- mean(df_m5$Wine_mhl)
  naive_err   <- df_m5$Wine_mhl - naive_pred
  naive_mae   <- mean(abs(naive_err))
  naive_rmse  <- sqrt(mean(naive_err^2))

  df_m5_feat <- df_m5 %>% select(-year)
  if ("Wine_mhl_original" %in% names(df_m5_feat))
    df_m5_feat <- df_m5_feat %>% select(-Wine_mhl_original)

  set.seed(seed)
  model_m5 <- M5Rules(Wine_mhl ~ ., data = df_m5_feat, control = Weka_control())

  cat("\n=== M5Rules Rules ===\n")
  print(model_m5)

  # Training performance
  train_pred   <- predict(model_m5, df_m5_feat)
  train_errors <- df_m5_feat$Wine_mhl - train_pred
  train_mae    <- mean(abs(train_errors))
  train_rmse   <- sqrt(mean(train_errors^2))
  ss_res       <- sum(train_errors^2)
  ss_tot       <- sum((df_m5_feat$Wine_mhl - mean(df_m5_feat$Wine_mhl))^2)
  train_r2     <- 1 - ss_res / ss_tot

  cat("\n=== Training Performance ===\n")
  cat("MAE :", round(train_mae,  2), "\n")
  cat("RMSE:", round(train_rmse, 2), "\n")
  cat("R²  :", round(train_r2,   3), "\n")

  eval_m5 <- evaluate_Weka_classifier(model_m5, numFolds = n_folds,
                                      complexity = FALSE, seed = seed)
  cat("\n=== M5Rules CV Results ===\n")
  print(eval_m5)

  cv_mae  <- eval_m5$details["meanAbsoluteError"]
  cv_rmse <- eval_m5$details["rootMeanSquaredError"]
  cv_rse  <- eval_m5$details["rootRelativeSquaredError"]
  cv_r2   <- 1 - (cv_rse / 100)^2

  cat("\n=== Cross-Validation Summary ===\n")
  cat("MAE :", round(cv_mae,  2), "\n")
  cat("RMSE:", round(cv_rmse, 2), "\n")
  cat("R²  :", round(cv_r2,   3), "\n")
  cat("MAE improvement over naive:",
      round((1 - cv_mae / naive_mae) * 100, 1), "%\n")
  cat("RMSE improvement over naive:",
      round((1 - cv_rmse / naive_rmse) * 100, 1), "%\n")

  # Save rules text
  rules_file_m <- file.path(out_dir, paste0("m5rules_rules_", tolower(region), ".txt"))
  sink(rules_file_m)
  cat("M5Rules —", region, "\n")
  cat("Generated     :", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
  cat("Detrend method:", if (detrend) detrend_method else "none", "\n")
  cat("Target        : Wine_mhl",
      if (detrend) "(residuals)" else "(raw, mhl)", "\n\n")
  cat("=== Naive Baseline ===\n")
  cat("MAE:", round(naive_mae, 2), "| RMSE:", round(naive_rmse, 2), "\n\n")
  cat("=== Discovered Rules ===\n"); print(model_m5)
  cat("\n=== Training Performance ===\n")
  cat("MAE:", round(train_mae, 2), "| RMSE:", round(train_rmse, 2),
      "| R2:", round(train_r2, 3), "\n")
  cat("\n=== Cross-Validation (", n_folds, "folds) ===\n")
  print(eval_m5)
  cat("R2 (CV):", round(cv_r2, 3), "\n")
  sink()

  # Save metrics CSV
  metrics_csv_m <- data.frame(
    region = region, detrend_method = if (detrend) detrend_method else "none",
    model = "M5Rules",
    cv_mae  = round(cv_mae,  4),
    cv_rmse = round(cv_rmse, 4),
    cv_r2   = round(cv_r2,   4),
    note = if (detrend) "metrics in residual units" else "metrics in mhl"
  )
  write.csv(metrics_csv_m,
            file.path(out_dir, paste0("m5rules_metrics_", tolower(region), ".csv")),
            row.names = FALSE)

  # Structured rules CSV (for script 9)
  m5_raw    <- capture.output(print(model_m5))
  block_idx <- grep("^LM num:\\s*[0-9]+", m5_raw)
  if (length(block_idx) > 0) {
    structured_m5 <- bind_rows(lapply(seq_along(block_idx), function(i) {
      blk_start <- block_idx[i] + 1
      blk_end   <- if (i < length(block_idx)) block_idx[i + 1] - 1 else length(m5_raw)
      block     <- m5_raw[blk_start:blk_end]
      block     <- block[nchar(trimws(block)) > 0]
      rule_idx  <- as.integer(gsub("[^0-9]", "", m5_raw[block_idx[i]]))
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
      lm_header <- grep(paste0("^LM", rule_idx, "\\s*\\("), m5_raw, value = TRUE)
      n_inst <- NA_real_; err_pct <- NA_real_
      if (length(lm_header) > 0) {
        hm <- regmatches(lm_header[1],
                regexec("\\(([0-9]+)/([0-9.]+)%?\\)", lm_header[1]))[[1]]
        if (length(hm) >= 3) { n_inst <- as.numeric(hm[2]); err_pct <- as.numeric(hm[3]) }
      }
      data.frame(
        rule_index = rule_idx, conditions_raw = conditions_raw,
        linear_model_raw = linear_model_raw,
        n_instances = n_inst, error_pct = err_pct,
        target_detrended = detrend, region = region,
        detrend_method = if (detrend) detrend_method else "none",
        stringsAsFactors = FALSE
      )
    }))
    write.csv(structured_m5,
              file.path(out_dir, paste0("m5rules_rules_structured_", tolower(region), ".csv")),
              row.names = FALSE)
  }

  cat("M5Rules outputs saved →", out_dir, "\n\n")
  all_summary[[paste0("M5Rules_", region)]] <- list(
    region = region, model = "M5Rules",
    cv_mae = round(cv_mae, 4), cv_r2 = round(cv_r2, 4)
  )

}   # end region loop


# ═══════════════════════════════════════════════════════════════════════════
#  FINAL SUMMARY
# ═══════════════════════════════════════════════════════════════════════════

cat("\n", strrep("═", 65), "\n", sep = "")
cat("  ALL PIPELINES COMPLETE\n")
cat(strrep("═", 65), "\n\n")
cat(sprintf("%-20s %-8s %-30s\n", "Model × Region", "Type", "Key metrics"))
cat(strrep("─", 60), "\n")

for (key in names(all_summary)) {
  s <- all_summary[[key]]
  if (s$model == "CarenR") {
    cat(sprintf("%-20s %-8s rules=%d\n", key, "Distrib", s$n_rules))
  } else if (s$model == "RIPPER") {
    cat(sprintf("%-20s %-8s Acc=%.3f  Macro-F1=%.3f\n",
                key, "Class", s$accuracy, s$macro_f1))
  } else {
    cat(sprintf("%-20s %-8s CV-MAE=%.3f  CV-R²=%.3f\n",
                key, "Regress", s$cv_mae, s$cv_r2))
  }
}

cat("\n")
cat("─── Outputs saved to ────────────────────────────────────────\n")
cat(" Rules text    :", out_dir, "\n")
cat(" Metrics CSV   :", out_dir, "\n")
cat(" CarenR rules  :", carenr_rules_dir, "\n")
cat(" (all three models now share the same output folder)\n\n")
cat("─── Next step ───────────────────────────────────────────────\n")
cat(" Run script 6 (6_scenario_validation.R) for each region\n")
cat(" to get the comparative walk-forward evaluation.\n")
cat(strrep("═", 65), "\n")

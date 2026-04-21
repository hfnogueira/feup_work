# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                   Detrend Method Comparison
#                   by:  hugonogueira
#
#  Loads results from both detrending methods ("linear" and "lowess")
#  and produces side-by-side comparison tables and plots.
#
#  Prerequisites — run both pipelines first:
#    1. Set detrend_method = "linear" in scripts 3, 4, 6, 7  → run all four
#    2. Set detrend_method = "lowess" in scripts 3, 4, 6, 7  → run all four
#
#  Reads from:
#    data/linear/walkforward_metrics_{region}.csv
#    data/lowess/walkforward_metrics_{region}.csv
#    data/linear/ripper_metrics_{region}.csv          (from script 4)
#    data/lowess/ripper_metrics_{region}.csv
#    data/linear/caren_class_metrics_{region}.csv     (from script 7)
#    data/lowess/caren_class_metrics_{region}.csv
#
#  Produces:
#    data/comparison/detrend_comparison_walkforward.csv
#    data/comparison/detrend_comparison_classification.csv
#    data/comparison/detrend_comparison_walkforward.png
#    data/comparison/detrend_comparison_classification.png
#
#  Requires: tidyverse
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library(tidyverse)


# config ------------------------------------------------------------------------------

methods <- c("linear", "lowess")
regions <- c("rdd", "rvv")

out_dir <- "data/comparison"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


# ==============================================================================
# HELPER: safe CSV loader — returns NULL if file is missing
# ==============================================================================

safe_read <- function(path) {
  if (!file.exists(path)) {
    message("  [skip] not found: ", path)
    return(NULL)
  }
  read.csv(path)
}


# ==============================================================================
# 1. WALK-FORWARD METRICS (regression)
# ==============================================================================

cat("============================================================\n")
cat(" Walk-Forward Validation — Detrend Method Comparison\n")
cat("============================================================\n\n")

wf_list <- list()

for (m in methods) {
  for (r in regions) {
    path <- file.path("data", m,
                      paste0("walkforward_metrics_", r, ".csv"))
    df   <- safe_read(path)
    if (!is.null(df)) wf_list[[paste(m, r)]] <- df
  }
}

if (length(wf_list) == 0) {

  cat("No walk-forward metrics found. Run scripts 6 with both methods first.\n\n")

} else {

  wf_all <- bind_rows(wf_list) %>%
    select(method, region, detrend_method, mae, rmse, r2) %>%
    arrange(region, detrend_method, mae)

  cat("Walk-forward metrics (MAE / RMSE / R²) — lower MAE/RMSE and higher R² = better\n\n")
  print(wf_all, digits = 3, row.names = FALSE)

  write.csv(wf_all,
            file.path(out_dir, "detrend_comparison_walkforward.csv"),
            row.names = FALSE)
  cat("\nSaved: data/comparison/detrend_comparison_walkforward.csv\n")


  # --- Plot: MAE by method and detrend -----------------------------------------
  p_mae <- wf_all %>%
    mutate(label = paste0(method, "\n(", detrend_method, ")")) %>%
    ggplot(aes(x = label, y = mae, fill = detrend_method)) +
    geom_col(position = "dodge", alpha = 0.85, width = 0.65) +
    geom_text(aes(label = round(mae, 1)), vjust = -0.4, size = 3.2) +
    facet_wrap(~ region, scales = "free_y") +
    scale_fill_manual(values = c(linear = "#4E79A7", lowess = "#F28E2B"),
                      name = "Detrend method") +
    labs(
      title    = "Walk-Forward MAE: Linear vs LOWESS Detrending",
      subtitle = "Lower = better  |  Faceted by region",
      x        = NULL, y = "MAE (mhl)"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x  = element_text(size = 8),
          strip.text    = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey50"))

  ggsave(file.path(out_dir, "detrend_comparison_walkforward.png"),
         p_mae, width = 11, height = 6, dpi = 150)
  cat("Saved: data/comparison/detrend_comparison_walkforward.png\n")


  # --- Plot: R² by method and detrend ------------------------------------------
  p_r2 <- wf_all %>%
    mutate(label = paste0(method, "\n(", detrend_method, ")")) %>%
    ggplot(aes(x = label, y = r2, fill = detrend_method)) +
    geom_col(position = "dodge", alpha = 0.85, width = 0.65) +
    geom_text(aes(label = round(r2, 3)), vjust = -0.4, size = 3.2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ region, scales = "free_y") +
    scale_fill_manual(values = c(linear = "#4E79A7", lowess = "#F28E2B"),
                      name = "Detrend method") +
    labs(
      title    = "Walk-Forward R²: Linear vs LOWESS Detrending",
      subtitle = "Higher = better  |  Dashed = 0 baseline",
      x        = NULL, y = "R²"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x  = element_text(size = 8),
          strip.text    = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey50"))

  ggsave(file.path(out_dir, "detrend_comparison_r2.png"),
         p_r2, width = 11, height = 6, dpi = 150)
  cat("Saved: data/comparison/detrend_comparison_r2.png\n")
}


# ==============================================================================
# 2. CLASSIFICATION METRICS (RIPPER + carenR)
# ==============================================================================

cat("\n============================================================\n")
cat(" Classification — Detrend Method Comparison\n")
cat("============================================================\n\n")

cls_list <- list()

for (m in methods) {
  for (r in regions) {
    for (script in c("ripper", "caren_class")) {
      path <- file.path("data", m,
                        paste0(script, "_metrics_", r, ".csv"))
      df   <- safe_read(path)
      if (!is.null(df)) cls_list[[paste(m, r, script)]] <- df
    }
  }
}

if (length(cls_list) == 0) {

  cat("No classification metrics found. Run scripts 4 and 7 with both methods first.\n\n")

} else {

  cls_all <- bind_rows(cls_list) %>%
    select(model, region, detrend_method, accuracy, macro_f1,
           macro_precision, macro_recall) %>%
    arrange(region, model, detrend_method)

  cat("Classification metrics (Accuracy / Macro-F1) — higher = better\n\n")
  print(cls_all, digits = 3, row.names = FALSE)

  write.csv(cls_all,
            file.path(out_dir, "detrend_comparison_classification.csv"),
            row.names = FALSE)
  cat("\nSaved: data/comparison/detrend_comparison_classification.csv\n")


  # --- Plot: Accuracy by model and detrend -------------------------------------
  p_acc <- cls_all %>%
    ggplot(aes(x = model, y = accuracy, fill = detrend_method)) +
    geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
    geom_text(aes(label = round(accuracy, 3)),
              position = position_dodge(0.6), vjust = -0.4, size = 3.2) +
    facet_wrap(~ region) +
    scale_fill_manual(values = c(linear = "#4E79A7", lowess = "#F28E2B"),
                      name = "Detrend method") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title    = "Classification Accuracy: Linear vs LOWESS Detrending",
      subtitle = "Higher = better  |  Faceted by region",
      x        = NULL, y = "CV Accuracy"
    ) +
    theme_minimal(base_size = 11) +
    theme(strip.text    = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey50"))

  ggsave(file.path(out_dir, "detrend_comparison_classification.png"),
         p_acc, width = 9, height = 5, dpi = 150)
  cat("Saved: data/comparison/detrend_comparison_classification.png\n")
}


# ==============================================================================
# 3. SUMMARY TABLE — easy to copy into thesis
# ==============================================================================

cat("\n============================================================\n")
cat(" Summary for thesis\n")
cat("============================================================\n\n")

cat("Interpretation guide:\n")
cat("  - If linear ≈ lowess on all metrics → detrend choice is robust (good!)\n")
cat("  - If lowess noticeably better       → non-linear trend in the data\n")
cat("  - If linear noticeably better       → LOWESS may be overfitting the trend\n\n")

cat("Done.\n")

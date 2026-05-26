# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                   Detrend Method Comparison
#                   by:  hugonogueira
#
#  Compares results from linear vs LOWESS detrending across all pipeline
#  scripts, with a focus on the carenR vs M5Rules regression comparison.
#
#  The scenario validation (script 6) is the PRIMARY comparison source:
#  CarenR and M5Rules are both trained on the training split and evaluated
#  in interpretable mhl units across multiple train/test scenarios.
#
#  The M5Rules standalone CV (script 5) is a SECONDARY source — useful to
#  see rule structure changes between methods, but metrics are in residual
#  units when detrend=TRUE, so they are not directly comparable.
#
#  Prerequisites — run both pipelines first:
#    1. Set detrend_method = "linear"  in scripts 3, 5, 6 → run all three
#    2. Set detrend_method = "lowess"  in scripts 3, 5, 6 → run all three
#
#  Reads from:
#    data/{method}/scenario_metrics_{region}.csv   (script 6)
#    data/{method}/m5rules_metrics_{region}.csv    (script 5)
#
#  Produces:
#    data/comparison/detrend_scenario_mae.png
#    data/comparison/detrend_scenario_r2.png
#    data/comparison/detrend_scenario_table.csv
#    data/comparison/detrend_m5rules_cv_table.csv
#
#  Requires: tidyverse
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)


# config ------------------------------------------------------------------------------

methods <- c("linear", "lowess")
regions <- c("rdd", "rvv")

out_dir <- "data/comparison"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


# helper ------------------------------------------------------------------------------

safe_read <- function(path) {
  if (!file.exists(path)) {
    message("  [skip] not found: ", path)
    return(NULL)
  }
  read.csv(path)
}


# ==============================================================================
# 1. SCENARIO VALIDATION METRICS  (primary — CarenR vs M5Rules, mhl units)
# Aggregates MAE across all scenarios (i.e. all training sizes) per model.
# ==============================================================================

cat("============================================================\n")
cat(" Scenario Validation — Linear vs LOWESS Detrending\n")
cat(" (metrics in mhl units — trend added back before scoring)\n")
cat("============================================================\n\n")

sc_list <- list()

for (m in methods) {
  for (r in regions) {
    df <- safe_read(file.path("data", m,
                              paste0("scenario_metrics_", r, ".csv")))
    if (!is.null(df)) sc_list[[paste(m, r)]] <- df
  }
}

if (length(sc_list) == 0) {

  cat("No scenario results found.\n")
  cat("Run script 6 (6_scenario_validation.R) with both detrend methods first.\n\n")

} else {

  sc_all <- bind_rows(sc_list)

  # Summary: mean MAE/R² across all scenarios, per model × detrend_method × region
  sc_summary <- sc_all %>%
    group_by(region, model, detrend_method) %>%
    summarise(mean_mae  = round(mean(mae,  na.rm = TRUE), 3),
              mean_rmse = round(mean(rmse, na.rm = TRUE), 3),
              mean_r2   = round(mean(r2,   na.rm = TRUE), 4),
              n_scenarios = n(),
              .groups = "drop") %>%
    arrange(region, detrend_method, mean_mae)

  cat("Mean MAE / RMSE / R² across all scenarios:\n\n")
  print(sc_summary, digits = 3, row.names = FALSE)

  write.csv(sc_summary,
            file.path(out_dir, "detrend_scenario_table.csv"),
            row.names = FALSE)
  cat("\nSaved: data/comparison/detrend_scenario_table.csv\n")


  # --- Plot 1: Mean MAE — CarenR vs M5Rules, by detrend method -----------------

  p_mae <- sc_summary %>%
    filter(model %in% c("CarenR", "M5Rules", "DT")) %>%
    ggplot(aes(x = model, y = mean_mae, fill = detrend_method)) +
    geom_col(position = position_dodge(0.7), width = 0.65, alpha = 0.87) +
    geom_text(aes(label = round(mean_mae, 1)),
              position = position_dodge(0.7), vjust = -0.45, size = 3.0) +
    facet_wrap(~ region, scales = "free_y") +
    scale_fill_manual(values = c(linear = "#4E79A7", lowess = "#F28E2B"),
                      name = "Detrend") +
    labs(
      title    = "Mean MAE across Scenarios: Linear vs LOWESS Detrending",
      subtitle = "Lower = better  |  Averaged over all training sizes  |  Faceted by region",
      x = NULL, y = "Mean MAE (mhl)"
    ) +
    theme_minimal(base_size = 11) +
    theme(strip.text      = element_text(face = "bold"),
          plot.subtitle   = element_text(colour = "grey50"),
          legend.position = "bottom")

  ggsave(file.path(out_dir, "detrend_scenario_mae.png"),
         p_mae, width = 11, height = 6, dpi = 150)
  cat("Saved: data/comparison/detrend_scenario_mae.png\n")


  # --- Plot 2: Learning curve overlay — both detrend methods -------------------
  # Shows how MAE evolves with training size for each method × model

  p_lc <- sc_all %>%
    filter(model %in% c("CarenR", "M5Rules", "DT")) %>%
    ggplot(aes(x = n_train, y = mae,
               colour = model, linetype = detrend_method)) +
    geom_smooth(method = "loess", span = 0.5, se = FALSE, linewidth = 0.9) +
    facet_wrap(~ region, scales = "free_y") +
    scale_colour_manual(values = c(DT      = "#E15759",
                                   CarenR  = "#4E79A7",
                                   M5Rules = "#F28E2B"),
                        name = "Model") +
    scale_linetype_manual(values = c(linear = "solid", lowess = "dashed"),
                          name = "Detrend") +
    labs(
      title    = "Learning Curves: MAE vs Training Size by Detrend Method",
      subtitle = "Solid = linear  |  Dashed = lowess  |  Smoothed trend",
      x = "Training observations (n)", y = "MAE (mhl)"
    ) +
    theme_minimal(base_size = 11) +
    theme(strip.text      = element_text(face = "bold"),
          plot.subtitle   = element_text(colour = "grey50"),
          legend.position = "bottom")

  ggsave(file.path(out_dir, "detrend_scenario_r2.png"),
         p_lc, width = 11, height = 6, dpi = 150)
  cat("Saved: data/comparison/detrend_scenario_r2.png\n")
}


# ==============================================================================
# 2. M5RULES STANDALONE CV  (secondary — residual units when detrend=TRUE)
# ==============================================================================

cat("\n============================================================\n")
cat(" M5Rules Standalone CV — Linear vs LOWESS\n")
cat(" (NOTE: metrics in RESIDUAL units when detrend=TRUE —\n")
cat("  use walk-forward results above for mhl-unit comparison)\n")
cat("============================================================\n\n")

m5_list <- list()

for (m in methods) {
  for (r in regions) {
    df <- safe_read(file.path("data", m,
                              paste0("m5rules_metrics_", r, ".csv")))
    if (!is.null(df)) m5_list[[paste(m, r)]] <- df
  }
}

if (length(m5_list) == 0) {
  cat("No M5Rules CV results found.\n")
  cat("Run script 5 with detrend_method = 'linear' and 'lowess' first.\n\n")
} else {
  m5_all <- bind_rows(m5_list) %>%
    select(region, detrend_method, model, cv_mae, cv_rmse, cv_r2, note) %>%
    arrange(region, detrend_method)

  print(m5_all, digits = 3, row.names = FALSE)

  write.csv(m5_all,
            file.path(out_dir, "detrend_m5rules_cv_table.csv"),
            row.names = FALSE)
  cat("\nSaved: data/comparison/detrend_m5rules_cv_table.csv\n")
}


# ==============================================================================
# 3. INTERPRETATION GUIDE
# ==============================================================================

cat("\n============================================================\n")
cat(" How to read these results\n")
cat("============================================================\n\n")

cat("Scenario validation (section 1) is the primary comparison:\n")
cat("  - Metrics are in mhl units → directly interpretable\n")
cat("  - CarenR and M5Rules trained on training split only (no leakage)\n")
cat("  - Learning curves show sample efficiency per model\n")
cat("  - If linear ≈ lowess across all models → detrend choice is robust\n")
cat("  - If lowess consistently lowers MAE → non-linear trend in the data\n\n")

cat("M5Rules CV (section 2) is useful for:\n")
cat("  - Seeing how discovered rules change between detrend methods\n")
cat("  - NOT for comparing absolute error magnitude to section 1\n\n")

cat("Done.\n")

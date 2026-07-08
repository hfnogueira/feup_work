# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  span_selection_diagnostics.R
#  by: hugonogueira
#
#  Tests a range of LOESS spans to help choose the right one for detrending.
#  Three convergent criteria:
#
#  1. STATIONARITY — find the largest span where RVV detrended residuals
#     pass the ADF test (unit root rejected). Want most conservative span
#     that still achieves stationarity.
#
#  2. CORRELATION PRESERVATION — for key features, track |r| with residuals
#     across spans. Genuine features plateau; era-artefact features drop.
#     The "right" span removes artefacts without destroying genuine signal.
#
#  3. NAIVE_MEDIAN MAE — smaller span → tighter fit → lower Naive_Median MAE
#     (less residual variance). When Naive MAE drops sharply, the span is
#     eating into climate signal, not just removing structural drift.
#
#  Outputs (saved to data/span_diagnostics/):
#    span_adf_results.csv          — ADF p-values per span × region
#    span_correlation_results.csv  — feature |r| per span × region
#    span_naive_mae.csv            — Naive_Median MAE per span × region
#    plot_stationarity.png
#    plot_correlations.png
#    plot_naive_mae.png
#
#  Requires: tseries, tidyverse
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for (pkg in c("tseries", "tidyverse")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, " ...")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(tseries)
library(tidyverse)

source("src/rscripts/utils/detrend_utils.R")

out_dir <- "data/span_diagnostics"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ── Config ────────────────────────────────────────────────────────────────

spans <- seq(0.20, 0.90, by = 0.05)   # range of spans to test

# Features to track in correlation analysis (edit as needed)
features_of_interest <- list(
  rdd = c("swa_hv_y1_b20", "days.rf.above.1mm_fl_y1_b20", "tm_hv_y1_c10"),
  rvv = c("iaf_hv_y1_c10", "days.rf.above.1mm_fl_y1_b20", "swa_hv_y1_b20")
)

regions <- list(
  rdd = list(
    prod_file    = "data/dataPrep_production_rdd.csv",
    feature_file = "data/dataPrep_cont_dataset_rdd.csv"
  ),
  rvv = list(
    prod_file    = "data/dataPrep_production_rvv.csv",
    feature_file = "data/dataPrep_cont_dataset_rvv.csv"
  )
)


# ── Helpers ───────────────────────────────────────────────────────────────

load_data <- function(region_name) {
  cfg  <- regions[[region_name]]
  # The continuous feature file already contains Wine_mhl and year
  feat <- read.csv(cfg$feature_file)
  feat$year <- as.numeric(as.character(feat$year))
  feat[order(feat$year), ]
}


# ══════════════════════════════════════════════════════════════════════════
# 1. STATIONARITY TEST
# ══════════════════════════════════════════════════════════════════════════

cat("\n=== 1. Stationarity across spans ===\n\n")
cat(sprintf("%-8s %-8s %-12s %-10s %s\n",
            "Span", "Region", "ADF stat", "ADF p", "Verdict"))
cat(strrep("-", 55), "\n")

adf_results <- list()

for (region_name in names(regions)) {
  df <- load_data(region_name)

  for (sp in spans) {
    resid <- get_trend_residuals(df, method = "lowess",
                                 span = sp, verbose = FALSE)$residuals

    adf_res  <- adf.test(resid)
    stationary <- adf_res$p.value < 0.05

    cat(sprintf("%-8.2f %-8s %-12.3f %-10.4f %s\n",
                sp, toupper(region_name),
                adf_res$statistic, adf_res$p.value,
                if (stationary) "PASS ✓" else "fail"))

    adf_results[[length(adf_results) + 1]] <- data.frame(
      span    = sp,
      region  = toupper(region_name),
      adf_stat = round(adf_res$statistic, 3),
      adf_pval = round(adf_res$p.value, 4),
      stationary = stationary
    )
  }
  cat("\n")
}

adf_df <- bind_rows(adf_results)
write.csv(adf_df, file.path(out_dir, "span_adf_results.csv"), row.names = FALSE)


# ══════════════════════════════════════════════════════════════════════════
# 2. CORRELATION PRESERVATION
# ══════════════════════════════════════════════════════════════════════════

cat("\n=== 2. Feature correlation preservation across spans ===\n\n")

corr_results <- list()

for (region_name in names(regions)) {
  df   <- load_data(region_name)
  feats <- features_of_interest[[region_name]]

  # Only keep features that actually exist in this dataset
  feats <- feats[feats %in% names(df)]
  if (length(feats) == 0) {
    cat("  No matching features found for", toupper(region_name), "\n")
    next
  }

  # Linear baseline correlations
  lin_resid <- get_trend_residuals(df, method = "linear", verbose = FALSE)$residuals
  lin_cors  <- sapply(feats, function(f) abs(cor(df[[f]], lin_resid, use = "complete.obs")))

  cat(sprintf("  %s — linear baseline |r|:\n", toupper(region_name)))
  for (f in feats) cat(sprintf("    %-40s %.3f\n", f, lin_cors[f]))
  cat("\n")

  for (sp in spans) {
    resid <- get_trend_residuals(df, method = "lowess",
                                 span = sp, verbose = FALSE)$residuals

    for (f in feats) {
      r_abs  <- abs(cor(df[[f]], resid, use = "complete.obs"))
      pct    <- round(100 * r_abs / lin_cors[f], 1)

      corr_results[[length(corr_results) + 1]] <- data.frame(
        span          = sp,
        region        = toupper(region_name),
        feature       = f,
        r_abs_loess   = round(r_abs, 4),
        r_abs_linear  = round(lin_cors[f], 4),
        pct_retained  = pct
      )
    }
  }
}

corr_df <- bind_rows(corr_results)
write.csv(corr_df, file.path(out_dir, "span_correlation_results.csv"), row.names = FALSE)

cat("  Correlation results (% of linear |r| retained):\n")
print(
  corr_df %>%
    select(span, region, feature, pct_retained) %>%
    pivot_wider(names_from = feature, values_from = pct_retained) %>%
    arrange(region, span),
  n = Inf
)


# ══════════════════════════════════════════════════════════════════════════
# 3. NAIVE_MEDIAN MAE
# ══════════════════════════════════════════════════════════════════════════

cat("\n=== 3. Naive_Median MAE across spans ===\n\n")
cat(sprintf("%-8s %-8s %-12s %s\n", "Span", "Region", "Naive MAE", "vs Linear"))

naive_results <- list()

for (region_name in names(regions)) {
  df <- load_data(region_name)

  # Linear baseline: naive MAE = MAD of residuals (median absolute deviation)
  lin_resid  <- get_trend_residuals(df, method = "linear", verbose = FALSE)$residuals
  lin_naive  <- median(abs(lin_resid - median(lin_resid)))

  cat(sprintf("  %s linear baseline Naive MAE: %.1f mhl\n",
              toupper(region_name), lin_naive))

  for (sp in spans) {
    resid      <- get_trend_residuals(df, method = "lowess",
                                       span = sp, verbose = FALSE)$residuals
    naive_mae  <- median(abs(resid - median(resid)))
    delta      <- naive_mae - lin_naive

    cat(sprintf("  %-8.2f %-8s %-12.1f %+.1f\n",
                sp, toupper(region_name), naive_mae, delta))

    naive_results[[length(naive_results) + 1]] <- data.frame(
      span        = sp,
      region      = toupper(region_name),
      naive_mae   = round(naive_mae, 2),
      linear_mae  = round(lin_naive, 2),
      delta       = round(delta, 2)
    )
  }
  cat("\n")
}

naive_df <- bind_rows(naive_results)
write.csv(naive_df, file.path(out_dir, "span_naive_mae.csv"), row.names = FALSE)


# ══════════════════════════════════════════════════════════════════════════
# 4. PLOTS
# ══════════════════════════════════════════════════════════════════════════

# Plot 1: ADF p-value vs span
p1 <- ggplot(adf_df, aes(x = span, y = adf_pval, colour = region)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "red") +
  annotate("text", x = max(spans), y = 0.06, label = "p = 0.05 threshold",
           hjust = 1, size = 3, colour = "red") +
  scale_x_reverse() +   # smaller span = more aggressive, put on right
  labs(
    title    = "ADF p-value vs LOESS Span",
    subtitle = "Smaller span = more aggressive detrending | Red line = significance threshold",
    x = "LOESS span (← more aggressive   less aggressive →)",
    y = "ADF p-value",
    colour = "Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "plot_stationarity.png"), p1,
       width = 9, height = 5, dpi = 150)
cat("Saved: data/span_diagnostics/plot_stationarity.png\n")


# Plot 2: Correlation retention vs span
if (nrow(corr_df) > 0) {
  p2 <- ggplot(corr_df, aes(x = span, y = pct_retained,
                              colour = feature, linetype = region)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    geom_hline(yintercept = 85, linetype = "dashed", colour = "grey40") +
    annotate("text", x = max(spans), y = 86, label = "85% retention threshold",
             hjust = 1, size = 3, colour = "grey40") +
    scale_x_reverse() +
    labs(
      title    = "Feature Correlation Retention vs LOESS Span",
      subtitle = "% of linear |r| retained | Drop = era artefact being removed",
      x = "LOESS span (← more aggressive   less aggressive →)",
      y = "% of linear |r| retained",
      colour = "Feature", linetype = "Region"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right")

  ggsave(file.path(out_dir, "plot_correlations.png"), p2,
         width = 11, height = 5, dpi = 150)
  cat("Saved: data/span_diagnostics/plot_correlations.png\n")
}


# Plot 3: Naive MAE vs span
p3 <- ggplot(naive_df, aes(x = span, y = naive_mae, colour = region)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2) +
  geom_hline(data = naive_df %>% distinct(region, linear_mae),
             aes(yintercept = linear_mae, colour = region),
             linetype = "dashed", linewidth = 0.7) +
  scale_x_reverse() +
  labs(
    title    = "Naive_Median MAE vs LOESS Span",
    subtitle = "Dashed = linear baseline | Drop below dashed = span eating into signal",
    x = "LOESS span (← more aggressive   less aggressive →)",
    y = "Naive_Median MAE (mhl)",
    colour = "Region"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "plot_naive_mae.png"), p3,
       width = 9, height = 5, dpi = 150)
cat("Saved: data/span_diagnostics/plot_naive_mae.png\n")


# ══════════════════════════════════════════════════════════════════════════
# 5. SUMMARY RECOMMENDATION
# ══════════════════════════════════════════════════════════════════════════

cat("\n=== Summary ===\n\n")

# Largest span where RVV passes ADF
rvv_pass <- adf_df %>%
  filter(region == "RVV", stationary == TRUE) %>%
  summarise(max_span = max(span)) %>%
  pull(max_span)

if (length(rvv_pass) > 0 && !is.na(rvv_pass)) {
  cat(sprintf("Largest span where RVV passes ADF: %.2f\n", rvv_pass))
} else {
  cat("RVV does not pass ADF at any tested span — check data.\n")
}

# Features still above 85% at each span
if (nrow(corr_df) > 0) {
  retention_summary <- corr_df %>%
    group_by(span, region) %>%
    summarise(n_genuine = sum(pct_retained >= 85), .groups = "drop")

  cat("\nFeatures retaining >= 85% correlation at each span:\n")
  print(retention_summary %>% pivot_wider(names_from = region, values_from = n_genuine),
        n = Inf)
}

cat("\nDone. Check data/span_diagnostics/ for plots and CSV outputs.\n")

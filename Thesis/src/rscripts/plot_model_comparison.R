# ─────────────────────────────────────────────────────────────────────────────
# Plot: Model Comparison vs Naive Baselines
# Reads pre-computed CSVs from the walk-forward validation pipeline
# ─────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(forcats)

# ── paths ──────────────────────────────────────────────────────────────────
summary_csv  <- "data/linear/scenario_summary_rvv.csv"
scenario_csv <- "data/linear/scenario_metrics_rvv.csv"
out_dir      <- "data/linear"

summary_df  <- read.csv(summary_csv,  stringsAsFactors = FALSE)
scenario_df <- read.csv(scenario_csv, stringsAsFactors = FALSE)

# ── colour palette & model grouping ─────────────────────────────────────────
naive_models  <- c("Naive", "Naive_Median", "Naive_MA")
carenr_models <- c("CarenR_Dist", "CarenR_Dist_FS", "CarenR_Dist_Thresh",
                   "CarenR_Dist_Sup", "CarenR_Dist_Sup_Thresh",
                   "CarenR_Dist_Eta2", "CarenR_Dist_Lag",
                   "CarenR_Dist_Conf", "CarenR_Dist_Stack")
ml_models     <- c("DT", "M5Rules", "Ripper")

# nicer display names
label_map <- c(
  "Naive"                  = "Naive (mean)",
  "Naive_Median"           = "Naive (median)",
  "Naive_MA"               = "Naive (MA-10)",
  "CarenR_Dist"            = "CarenR Dist",
  "CarenR_Dist_FS"         = "CarenR FS",
  "CarenR_Dist_Thresh"     = "CarenR Thresh",
  "CarenR_Dist_Sup"        = "CarenR Sup",
  "CarenR_Dist_Sup_Thresh" = "CarenR Sup+Thresh ★",
  "CarenR_Dist_Eta2"       = "CarenR Eta2",
  "CarenR_Dist_Lag"        = "CarenR Lag",
  "CarenR_Dist_Conf"       = "CarenR Conf",
  "CarenR_Dist_Stack"      = "CarenR Stack",
  "DT"                     = "Decision Tree",
  "M5Rules"                = "M5Rules",
  "Ripper"                 = "RIPPER"
)

group_map <- c(
  "Naive"                  = "Naive",
  "Naive_Median"           = "Naive",
  "Naive_MA"               = "Naive",
  "CarenR_Dist"            = "CarenR",
  "CarenR_Dist_FS"         = "CarenR",
  "CarenR_Dist_Thresh"     = "CarenR",
  "CarenR_Dist_Sup"        = "CarenR",
  "CarenR_Dist_Sup_Thresh" = "CarenR (best)",
  "CarenR_Dist_Eta2"       = "CarenR",
  "CarenR_Dist_Lag"        = "CarenR",
  "CarenR_Dist_Conf"       = "CarenR",
  "CarenR_Dist_Stack"      = "CarenR",
  "DT"                     = "Other ML",
  "M5Rules"                = "Other ML",
  "Ripper"                 = "Other ML"
)

group_colours <- c(
  "Naive"        = "#2196F3",   # blue
  "CarenR"       = "#4CAF50",   # green
  "CarenR (best)"= "#1B5E20",   # dark green
  "Other ML"     = "#9E9E9E"    # grey
)

# ── prepare summary data ────────────────────────────────────────────────────
sum_df <- summary_df %>%
  mutate(
    label = label_map[model],
    group = group_map[model],
    label = factor(label, levels = label_map[order(summary_df$weighted_mae[match(names(label_map), summary_df$model)], na.last=TRUE)])
  ) %>%
  arrange(weighted_mae)

# fix factor order by weighted_mae
sum_df$label <- fct_reorder(sum_df$label, sum_df$weighted_mae)

naive_med_wt <- sum_df$weighted_mae[sum_df$model == "Naive_Median"]

# ═══════════════════════════════════════════════════════════════════════════
# PLOT 1: Weighted MAE bar chart (primary ranking)
# ═══════════════════════════════════════════════════════════════════════════
p1 <- ggplot(sum_df, aes(x = label, y = weighted_mae, fill = group)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = naive_med_wt, linetype = "dashed",
             colour = "#1565C0", linewidth = 0.8) +
  annotate("text", x = 1, y = naive_med_wt + 8,
           label = sprintf("Naive (median) = %.0f mhl", naive_med_wt),
           hjust = 0, colour = "#1565C0", size = 3.2, fontface = "italic") +
  geom_text(aes(label = sprintf("%.0f", weighted_mae)),
            hjust = -0.1, size = 3.0, colour = "grey20") +
  coord_flip(ylim = c(0, max(sum_df$weighted_mae) * 1.12)) +
  scale_fill_manual(values = group_colours, name = "Model group") +
  labs(
    title    = "Model Comparison: Weighted MAE across 16 Walk-Forward Scenarios",
    subtitle = "Weighted by test-set size (n_test) — lower is better",
    x        = NULL,
    y        = "Weighted MAE (mhl)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(colour = "grey40", size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(out_dir, "plot_comparison_weighted_mae.png"),
       p1, width = 10, height = 6, dpi = 150)
cat("Saved: plot_comparison_weighted_mae.png\n")

# ═══════════════════════════════════════════════════════════════════════════
# PLOT 2: Per-scenario MAE — key models only (focus plot)
# ═══════════════════════════════════════════════════════════════════════════
key_models <- c("Naive_Median", "Naive", "Naive_MA",
                "CarenR_Dist_Sup_Thresh", "CarenR_Dist_Stack",
                "M5Rules", "DT")

scen_key <- scenario_df %>%
  filter(model %in% key_models) %>%
  mutate(
    label = label_map[model],
    group = group_map[model]
  )

# line type: dashed for naive, solid for rest
lt_map <- c("Naive (median)" = "dashed",
            "Naive (mean)"   = "dashed",
            "Naive (MA-10)"  = "dashed",
            "CarenR Sup+Thresh ★" = "solid",
            "CarenR Stack"   = "solid",
            "M5Rules"        = "dotted",
            "Decision Tree"  = "dotted")

col_map2 <- c(
  "Naive (median)"     = "#1565C0",
  "Naive (mean)"       = "#42A5F5",
  "Naive (MA-10)"      = "#90CAF9",
  "CarenR Sup+Thresh ★"= "#2E7D32",
  "CarenR Stack"       = "#81C784",
  "M5Rules"            = "#757575",
  "Decision Tree"      = "#BDBDBD"
)

p2 <- ggplot(scen_key, aes(x = scenario, y = mae,
                            colour = label, linetype = label, group = label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(values = col_map2, name = NULL) +
  scale_linetype_manual(values = lt_map, name = NULL) +
  scale_x_continuous(breaks = 1:16) +
  labs(
    title    = "Per-Scenario MAE: Key Models vs Naive Baselines",
    subtitle = "16 expanding-window scenarios — earlier scenarios have larger test sets",
    x        = "Scenario (walk-forward split)",
    y        = "MAE (mhl)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(colour = "grey40", size = 10),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "plot_comparison_per_scenario.png"),
       p2, width = 11, height = 5.5, dpi = 150)
cat("Saved: plot_comparison_per_scenario.png\n")

# ═══════════════════════════════════════════════════════════════════════════
# PLOT 3: Relative MAE vs Naive_Median (% gap)
# Shows how much worse each model is compared to the best naive
# ═══════════════════════════════════════════════════════════════════════════
naive_ref <- summary_df$weighted_mae[summary_df$model == "Naive_Median"]

rel_df <- sum_df %>%
  mutate(
    pct_vs_naive = (weighted_mae - naive_ref) / naive_ref * 100,
    worse        = pct_vs_naive > 0
  )

p3 <- ggplot(rel_df, aes(x = label, y = pct_vs_naive, fill = group)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, colour = "#1565C0", linewidth = 1.0) +
  geom_text(aes(label = sprintf("%+.0f%%", pct_vs_naive),
                vjust = ifelse(pct_vs_naive >= 0, -0.3, 1.2)),
            size = 3.0, colour = "grey20") +
  coord_flip() +
  scale_fill_manual(values = group_colours, name = "Model group") +
  labs(
    title    = "Relative Weighted MAE vs Naive (median)",
    subtitle = "0% = matches Naive (median); positive = worse",
    x        = NULL,
    y        = "% difference in Weighted MAE"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title     = element_text(face = "bold", size = 13),
    plot.subtitle  = element_text(colour = "grey40", size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(out_dir, "plot_comparison_relative.png"),
       p3, width = 10, height = 6, dpi = 150)
cat("Saved: plot_comparison_relative.png\n")

# ═══════════════════════════════════════════════════════════════════════════
# PLOT 4: Mean MAE + SD error bars (stability view)
# ═══════════════════════════════════════════════════════════════════════════
p4 <- ggplot(sum_df, aes(x = label, y = mean_mae, fill = group)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = pmax(0, mean_mae - sd_mae),
                    ymax = mean_mae + sd_mae),
                width = 0.25, colour = "grey30", linewidth = 0.6) +
  geom_hline(yintercept = summary_df$mean_mae[summary_df$model == "Naive_Median"],
             linetype = "dashed", colour = "#1565C0", linewidth = 0.8) +
  coord_flip(ylim = c(0, max(sum_df$mean_mae + sum_df$sd_mae) * 1.08)) +
  scale_fill_manual(values = group_colours, name = "Model group") +
  labs(
    title    = "Mean MAE ± SD across 16 Scenarios",
    subtitle = "Error bars show ±1 SD (scenario-to-scenario stability)",
    x        = NULL,
    y        = "MAE (mhl)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(colour = "grey40", size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(out_dir, "plot_comparison_mean_sd.png"),
       p4, width = 10, height = 6, dpi = 150)
cat("Saved: plot_comparison_mean_sd.png\n")

# ═══════════════════════════════════════════════════════════════════════════
# PLOT 5: Production context — what does MAE mean in real terms?
# ═══════════════════════════════════════════════════════════════════════════

full_df <- read.csv("data/dataPrep_cont_dataset_rvv.csv",
                    stringsAsFactors = FALSE)

prod_mean   <- mean(full_df$Wine_mhl)
prod_median <- median(full_df$Wine_mhl)
prod_sd     <- sd(full_df$Wine_mhl)
prod_iqr    <- IQR(full_df$Wine_mhl)

# Key MAE reference levels
mae_levels <- data.frame(
  label = c("Naive (median) — 140 mhl",
            "CarenR Sup+Thresh — 172 mhl",
            "CarenR Conf — 172 mhl",
            "Naive (MA-10) — 170 mhl",
            "M5Rules — 267 mhl",
            "Decision Tree — 479 mhl"),
  wt_mae = c(140, 172, 172, 170, 267, 479),
  group  = c("Naive", "CarenR (best)", "CarenR", "Naive", "Other ML", "Other ML")
)
mae_levels$pct_mean <- mae_levels$wt_mae / prod_mean * 100

# ── 5a: Time series + ±MAE band around the mean (context) ──────────────────
# show only the 3 most informative MAE levels
band_df <- data.frame(
  label  = c("±Naive (median) 140 mhl", "±CarenR best 172 mhl", "±Decision Tree 479 mhl"),
  ymin   = prod_mean - c(140, 172, 479),
  ymax   = prod_mean + c(140, 172, 479),
  fill   = c("#BBDEFB", "#C8E6C9", "#EEEEEE")
)

p5a <- ggplot() +
  # shaded bands (outermost first)
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = band_df$ymin[3], ymax = band_df$ymax[3],
           fill = "#F5F5F5", alpha = 0.8) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = band_df$ymin[2], ymax = band_df$ymax[2],
           fill = "#C8E6C9", alpha = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = band_df$ymin[1], ymax = band_df$ymax[1],
           fill = "#BBDEFB", alpha = 0.5) +
  # actual production time series
  geom_line(data = full_df, aes(x = year, y = Wine_mhl),
            colour = "grey30", linewidth = 0.7) +
  geom_point(data = full_df, aes(x = year, y = Wine_mhl),
             colour = "grey30", size = 1.5) +
  # mean line
  geom_hline(yintercept = prod_mean, linetype = "dashed",
             colour = "grey50", linewidth = 0.7) +
  annotate("text", x = min(full_df$year) + 1, y = prod_mean + 60,
           label = sprintf("Mean = %.0f mhl", prod_mean),
           hjust = 0, colour = "grey40", size = 3.2) +
  # band labels on right margin
  annotate("text", x = max(full_df$year) + 0.5,
           y = band_df$ymax[1], label = "±140 (Naive)", hjust = 0,
           colour = "#1565C0", size = 3.0, fontface = "italic") +
  annotate("text", x = max(full_df$year) + 0.5,
           y = band_df$ymax[2], label = "±172 (CarenR)", hjust = 0,
           colour = "#2E7D32", size = 3.0, fontface = "italic") +
  annotate("text", x = max(full_df$year) + 0.5,
           y = band_df$ymax[3], label = "±479 (DT)", hjust = 0,
           colour = "grey50", size = 3.0, fontface = "italic") +
  coord_cartesian(xlim = c(min(full_df$year), max(full_df$year) + 7),
                  clip = "off") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Wine Production (RVV, 1942–2021) with MAE Error Bands",
    subtitle = sprintf(
      "Full series: mean = %.0f mhl, SD = %.0f mhl, CV = %.0f%%, range = %.0f–%.0f mhl",
      prod_mean, prod_sd, prod_sd / prod_mean * 100,
      min(full_df$Wine_mhl), max(full_df$Wine_mhl)),
    x = "Year",
    y = "Wine production (mhl)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(colour = "grey40", size = 10),
    plot.margin   = margin(5, 60, 5, 5),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "plot_context_timeseries.png"),
       p5a, width = 12, height = 5, dpi = 150)
cat("Saved: plot_context_timeseries.png\n")

# ── 5b: MAE as % of mean production — the "how big is this error?" view ────
# compute per key model for the test years only
pred_df <- read.csv("data/linear/scenario_predictions_rvv.csv",
                    stringsAsFactors = FALSE)

focus_models <- c("Naive_Median", "Naive", "Naive_MA",
                  "CarenR_Dist_Sup_Thresh", "CarenR_Dist_Stack",
                  "M5Rules", "DT")

pct_df <- pred_df %>%
  filter(model %in% focus_models) %>%
  mutate(
    abs_err     = abs(actual - predicted),
    pct_err     = abs_err / actual * 100,        # % of that year's actual
    label       = label_map[model]
  ) %>%
  group_by(label) %>%
  summarise(
    mean_abs_err  = mean(abs_err,  na.rm = TRUE),
    mean_pct_err  = mean(pct_err,  na.rm = TRUE),
    median_pct_err = median(pct_err, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = group_map[gsub(" ★", "", label)],
    label = fct_reorder(label, mean_pct_err)
  )
# patch group for CarenR best
pct_df$group[grepl("Sup\\+Thresh", pct_df$label)] <- "CarenR (best)"

p5b <- ggplot(pct_df, aes(x = label, y = mean_pct_err, fill = group)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.0f%%", mean_pct_err)),
            hjust = -0.15, size = 3.2, colour = "grey20") +
  geom_hline(yintercept = pct_df$mean_pct_err[grepl("median", pct_df$label)],
             linetype = "dashed", colour = "#1565C0", linewidth = 0.8) +
  coord_flip(ylim = c(0, max(pct_df$mean_pct_err) * 1.15)) +
  scale_fill_manual(values = group_colours, name = "Model group") +
  labs(
    title    = "Mean Absolute Error as % of Actual Production",
    subtitle = "Each prediction error expressed relative to that year's true production value",
    x        = NULL,
    y        = "Mean |error| / actual  (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(colour = "grey40", size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(file.path(out_dir, "plot_context_pct_error.png"),
       p5b, width = 10, height = 5, dpi = 150)
cat("Saved: plot_context_pct_error.png\n")

# ═══════════════════════════════════════════════════════════════════════════
# PLOT 6: Linear vs LOESS detrend comparison (if lowess results exist)
# ═══════════════════════════════════════════════════════════════════════════
lowess_summary <- "data/lowess/scenario_summary_rvv.csv"

if (file.exists(lowess_summary)) {
  lin_df <- read.csv(summary_csv,    stringsAsFactors = FALSE) %>%
    mutate(detrend = "Linear")
  low_df <- read.csv(lowess_summary, stringsAsFactors = FALSE) %>%
    mutate(detrend = "LOESS (span=0.40)")

  both_df <- bind_rows(lin_df, low_df) %>%
    mutate(
      label = label_map[model],
      group = group_map[model]
    ) %>%
    filter(!is.na(label), !is.na(weighted_mae))

  model_order <- lin_df %>% arrange(weighted_mae) %>% pull(model)
  both_df$label <- factor(both_df$label,
                           levels = rev(label_map[model_order]))

  naive_refs <- both_df %>%
    filter(model == "Naive_Median") %>%
    group_by(detrend) %>% slice(1)

  p6 <- ggplot(both_df, aes(x = label, y = weighted_mae, fill = detrend)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    geom_hline(data = naive_refs,
               aes(yintercept = weighted_mae, colour = detrend),
               linetype = "dashed", linewidth = 0.9, show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(
      values = c("Linear" = "#90CAF9", "LOESS (span=0.40)" = "#A5D6A7"),
      name   = "Detrend method"
    ) +
    scale_colour_manual(
      values = c("Linear" = "#1565C0", "LOESS (span=0.40)" = "#2E7D32")
    ) +
    labs(
      title    = "Weighted MAE: Linear vs LOESS Detrending",
      subtitle = "Dashed lines = Naive (median) reference for each detrend method",
      x        = NULL,
      y        = "Weighted MAE (mhl)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = "grey40", size = 10),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank()
    )

  ggsave(file.path("data/linear", "plot_detrend_comparison.png"),
         p6, width = 11, height = 7, dpi = 150)
  cat("Saved: plot_detrend_comparison.png\n")

  cat("\n--- Linear vs LOESS weighted MAE (key models) ---\n")
  cat(sprintf("%-28s %8s %8s %10s\n", "Model","Linear","LOESS","Improvement"))
  focus <- c("Naive_Median","Naive","Naive_MA",
             "CarenR_Dist_Sup_Thresh","CarenR_Dist_Stack","M5Rules","DT")
  for (m in focus) {
    lin_v <- lin_df$weighted_mae[lin_df$model == m]
    low_v <- low_df$weighted_mae[low_df$model == m]
    if (length(lin_v) && length(low_v) && !is.na(low_v)) {
      cat(sprintf("%-28s %8.1f %8.1f %+9.1f%%\n",
                  m, lin_v, low_v, (lin_v - low_v) / lin_v * 100))
    }
  }
} else {
  cat("(Skipping detrend comparison — data/lowess/ not found)\n")
}

# ── quick console summary ────────────────────────────────────────────────────
cat(sprintf("\n--- Production context (full series, n=%d years) ---\n", nrow(full_df)))
cat(sprintf("  Mean:   %.0f mhl\n", prod_mean))
cat(sprintf("  SD:     %.0f mhl  (CV = %.0f%%)\n", prod_sd, prod_sd/prod_mean*100))
cat(sprintf("  IQR:    %.0f mhl\n", prod_iqr))
cat(sprintf("  Range:  %.0f – %.0f mhl\n",
            min(full_df$Wine_mhl), max(full_df$Wine_mhl)))
cat(sprintf("\n  MAE 140 mhl  =  %.1f%% of mean,  %.1f%% of SD\n",
            140/prod_mean*100, 140/prod_sd*100))
cat(sprintf("  MAE 172 mhl  =  %.1f%% of mean,  %.1f%% of SD\n",
            172/prod_mean*100, 172/prod_sd*100))
cat(sprintf("  MAE 200 mhl  =  %.1f%% of mean,  %.1f%% of SD\n",
            200/prod_mean*100, 200/prod_sd*100))
cat(sprintf("  MAE 479 mhl  =  %.1f%% of mean,  %.1f%% of SD\n",
            479/prod_mean*100, 479/prod_sd*100))

cat("\nAll plots saved to", out_dir, "\n")

cat("\nAll plots saved to", out_dir, "\n")

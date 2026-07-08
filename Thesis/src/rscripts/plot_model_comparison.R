# ─────────────────────────────────────────────────────────────────────────────
# Plot: Model Comparison vs Naive Baselines
# Run for one region at a time:
#   Rscript plot_model_comparison.R RDD
#   Rscript plot_model_comparison.R RVV
# Defaults to RVV if no argument supplied.
# ─────────────────────────────────────────────────────────────────────────────

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(forcats)

# ── region argument ───────────────────────────────────────────────────────────
args   <- commandArgs(trailingOnly = TRUE)
region <- if (length(args) >= 1) toupper(args[1]) else "RVV"
stopifnot(region %in% c("RDD", "RVV"))

region_lower <- tolower(region)
n_scenarios  <- if (region == "RDD") 18 else 16
region_label <- if (region == "RDD") "Douro (RDD), 1934–2022" else "Vinho Verde (RVV), 1942–2021"
best_carenr  <- if (region == "RDD") "CarenR_Dist_Eta2" else "CarenR_Dist_Sup_FS"

cat(sprintf("Region: %s | n_scenarios: %d | best CarenR: %s\n",
            region, n_scenarios, best_carenr))

# ── paths ─────────────────────────────────────────────────────────────────────
summary_csv  <- sprintf("data/linear/scenario_summary_%s.csv",  region_lower)
scenario_csv <- sprintf("data/linear/scenario_metrics_%s.csv",  region_lower)
prod_csv     <- sprintf("data/dataPrep_cont_dataset_%s.csv",     region_lower)
pred_csv     <- sprintf("data/linear/scenario_predictions_%s.csv", region_lower)
out_dir      <- "data/linear"

summary_df  <- read.csv(summary_csv,  stringsAsFactors = FALSE)
scenario_df <- read.csv(scenario_csv, stringsAsFactors = FALSE)

# ── colour palette & model grouping ──────────────────────────────────────────
label_map <- c(
  "Naive"                  = "Naive (last value)",
  "Naive_Median"           = "Naive (median)",
  "Naive_MA"               = "Naive (MA-3)",
  "CarenR_Dist"            = "CarenR Dist",
  "CarenR_Dist_FS"         = "CarenR FS",
  "CarenR_Dist_Thresh"     = "CarenR Thresh",
  "CarenR_Dist_Sup"        = "CarenR Sup",
  "CarenR_Dist_Sup_Thresh" = "CarenR Sup+Thresh",
  "CarenR_Dist_Sup_FS"     = "CarenR Sup+FS",
  "CarenR_Dist_Eta2"       = "CarenR Eta2",
  "CarenR_Dist_Lag"        = "CarenR Lag",
  "CarenR_Dist_Conf"       = "CarenR Conf",
  "CarenR_Dist_Stack"      = "CarenR Stack",
  "CarenR_Supervised"      = "CarenR Supervised",
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
  "CarenR_Dist_Sup_Thresh" = "CarenR",
  "CarenR_Dist_Sup_FS"     = "CarenR",
  "CarenR_Dist_Eta2"       = "CarenR",
  "CarenR_Dist_Lag"        = "CarenR",
  "CarenR_Dist_Conf"       = "CarenR",
  "CarenR_Dist_Stack"      = "CarenR",
  "CarenR_Supervised"      = "CarenR",
  "DT"                     = "Other ML",
  "M5Rules"                = "Other ML",
  "Ripper"                 = "Other ML"
)
# mark the best CarenR variant for this region
group_map[best_carenr] <- "CarenR (best)"

group_colours <- c(
  "Naive"         = "#2196F3",
  "CarenR"        = "#4CAF50",
  "CarenR (best)" = "#1B5E20",
  "Other ML"      = "#9E9E9E"
)

# ── prepare summary data ──────────────────────────────────────────────────────
sum_df <- summary_df %>%
  filter(model %in% names(label_map)) %>%
  mutate(
    label = label_map[model],
    group = group_map[model]
  ) %>%
  arrange(weighted_mae)

sum_df$label <- fct_reorder(sum_df$label, sum_df$weighted_mae)
naive_med_wt <- sum_df$weighted_mae[sum_df$model == "Naive_Median"]

# ═══════════════════════════════════════════════════════════════════════════════
# PLOT 1: Weighted MAE bar chart (primary ranking)
# ═══════════════════════════════════════════════════════════════════════════════
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
    title    = sprintf("Model Comparison: Weighted MAE across %d Walk-Forward Scenarios (%s)",
                       n_scenarios, region),
    subtitle = "Weighted by test-set size (n_test) — lower is better",
    x        = NULL,
    y        = "Weighted MAE (mhl)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

fname1 <- sprintf("plot_comparison_weighted_mae_%s.png", region_lower)
ggsave(file.path(out_dir, fname1), p1, width = 10, height = 6, dpi = 150)
cat("Saved:", fname1, "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PLOT 2: Per-scenario MAE — key models only
# ═══════════════════════════════════════════════════════════════════════════════
key_models <- c("Naive_Median", "Naive", "Naive_MA", best_carenr,
                "CarenR_Dist_Stack", "M5Rules", "DT")
key_models <- key_models[key_models %in% unique(scenario_df$model)]

best_label <- label_map[best_carenr]

scen_key <- scenario_df %>%
  filter(model %in% key_models) %>%
  mutate(label = label_map[model], group = group_map[model])

lt_map <- setNames(
  c("dashed", "dashed", "dashed", "solid", "solid", "dotted", "dotted"),
  c("Naive (median)", "Naive (last value)", "Naive (MA-3)",
    best_label, "CarenR Stack", "M5Rules", "Decision Tree")
)
col_map2 <- setNames(
  c("#1565C0", "#42A5F5", "#90CAF9", "#1B5E20", "#81C784", "#757575", "#BDBDBD"),
  c("Naive (median)", "Naive (last value)", "Naive (MA-3)",
    best_label, "CarenR Stack", "M5Rules", "Decision Tree")
)
# keep only entries present in this region
lt_map  <- lt_map[names(lt_map)  %in% unique(scen_key$label)]
col_map2 <- col_map2[names(col_map2) %in% unique(scen_key$label)]

p2 <- ggplot(scen_key, aes(x = scenario, y = mae,
                            colour = label, linetype = label, group = label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(values = col_map2, name = NULL) +
  scale_linetype_manual(values = lt_map, name = NULL) +
  scale_x_continuous(breaks = 1:n_scenarios) +
  labs(
    title    = sprintf("Per-Scenario MAE: Key Models vs Naive Baselines (%s)", region),
    subtitle = sprintf("%d expanding-window scenarios — earlier scenarios have larger test sets",
                       n_scenarios),
    x        = "Scenario (walk-forward split)",
    y        = "MAE (mhl)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

fname2 <- sprintf("plot_comparison_per_scenario_%s.png", region_lower)
ggsave(file.path(out_dir, fname2), p2, width = 11, height = 5.5, dpi = 150)
cat("Saved:", fname2, "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PLOT 3: Relative MAE vs Naive_Median (% gap)
# ═══════════════════════════════════════════════════════════════════════════════
naive_ref <- summary_df$weighted_mae[summary_df$model == "Naive_Median"]
rel_df <- sum_df %>%
  mutate(pct_vs_naive = (weighted_mae - naive_ref) / naive_ref * 100)

p3 <- ggplot(rel_df, aes(x = label, y = pct_vs_naive, fill = group)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, colour = "#1565C0", linewidth = 1.0) +
  geom_text(aes(label = sprintf("%+.0f%%", pct_vs_naive),
                vjust = ifelse(pct_vs_naive >= 0, -0.3, 1.2)),
            size = 3.0, colour = "grey20") +
  coord_flip() +
  scale_fill_manual(values = group_colours, name = "Model group") +
  labs(
    title    = sprintf("Relative Weighted MAE vs Naive (median) — %s", region),
    subtitle = "0% = matches Naive (median); positive = worse",
    x        = NULL,
    y        = "% difference in Weighted MAE"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

fname3 <- sprintf("plot_comparison_relative_%s.png", region_lower)
ggsave(file.path(out_dir, fname3), p3, width = 10, height = 6, dpi = 150)
cat("Saved:", fname3, "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PLOT 4: Mean MAE + SD error bars (stability view)
# ═══════════════════════════════════════════════════════════════════════════════
p4 <- ggplot(sum_df, aes(x = label, y = mean_mae, fill = group)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = pmax(0, mean_mae - sd_mae), ymax = mean_mae + sd_mae),
                width = 0.25, colour = "grey30", linewidth = 0.6) +
  geom_hline(yintercept = summary_df$mean_mae[summary_df$model == "Naive_Median"],
             linetype = "dashed", colour = "#1565C0", linewidth = 0.8) +
  coord_flip(ylim = c(0, max(sum_df$mean_mae + sum_df$sd_mae) * 1.08)) +
  scale_fill_manual(values = group_colours, name = "Model group") +
  labs(
    title    = sprintf("Mean MAE ± SD across %d Scenarios (%s)", n_scenarios, region),
    subtitle = "Error bars show ±1 SD (scenario-to-scenario stability)",
    x        = NULL,
    y        = "MAE (mhl)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

fname4 <- sprintf("plot_comparison_mean_sd_%s.png", region_lower)
ggsave(file.path(out_dir, fname4), p4, width = 10, height = 6, dpi = 150)
cat("Saved:", fname4, "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PLOT 5: Production context (time series + MAE bands)
# ═══════════════════════════════════════════════════════════════════════════════
if (file.exists(prod_csv)) {
  full_df     <- read.csv(prod_csv, stringsAsFactors = FALSE)
  prod_col    <- if ("Wine_mhl" %in% names(full_df)) "Wine_mhl" else names(full_df)[2]
  full_df$y   <- full_df[[prod_col]]
  prod_mean   <- mean(full_df$y);  prod_sd <- sd(full_df$y)

  best_mae <- sum_df$weighted_mae[sum_df$model == best_carenr]
  naive_mae <- naive_med_wt

  p5a <- ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = prod_mean - best_mae,  ymax = prod_mean + best_mae,
             fill = "#C8E6C9", alpha = 0.5) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = prod_mean - naive_mae, ymax = prod_mean + naive_mae,
             fill = "#BBDEFB", alpha = 0.5) +
    geom_line(data = full_df,  aes(x = year, y = y), colour = "grey30", linewidth = 0.7) +
    geom_point(data = full_df, aes(x = year, y = y), colour = "grey30", size = 1.5) +
    geom_hline(yintercept = prod_mean, linetype = "dashed",
               colour = "grey50", linewidth = 0.7) +
    annotate("text", x = min(full_df$year) + 1, y = prod_mean + 60,
             label = sprintf("Mean = %.0f mhl", prod_mean),
             hjust = 0, colour = "grey40", size = 3.2) +
    annotate("text", x = max(full_df$year) + 0.5,
             y = prod_mean + naive_mae,
             label = sprintf("±%.0f (Naive)", naive_mae),
             hjust = 0, colour = "#1565C0", size = 3.0, fontface = "italic") +
    annotate("text", x = max(full_df$year) + 0.5,
             y = prod_mean + best_mae,
             label = sprintf("±%.0f (CarenR best)", best_mae),
             hjust = 0, colour = "#2E7D32", size = 3.0, fontface = "italic") +
    coord_cartesian(xlim = c(min(full_df$year), max(full_df$year) + 8), clip = "off") +
    scale_y_continuous(labels = comma) +
    labs(
      title    = sprintf("Wine Production (%s) with MAE Error Bands", region_label),
      subtitle = sprintf("Mean = %.0f mhl, SD = %.0f mhl, CV = %.0f%%",
                         prod_mean, prod_sd, prod_sd / prod_mean * 100),
      x = "Year", y = "Wine production (mhl)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = "grey40", size = 10),
      plot.margin   = margin(5, 70, 5, 5),
      panel.grid.minor = element_blank()
    )

  fname5 <- sprintf("plot_context_timeseries_%s.png", region_lower)
  ggsave(file.path(out_dir, fname5), p5a, width = 12, height = 5, dpi = 150)
  cat("Saved:", fname5, "\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PLOT 6: Linear vs LOESS detrend comparison
# ═══════════════════════════════════════════════════════════════════════════════
lowess_summary <- sprintf("data/lowess/scenario_summary_%s.csv", region_lower)
if (file.exists(lowess_summary)) {
  lin_df <- read.csv(summary_csv,    stringsAsFactors = FALSE) %>% mutate(detrend = "Linear")
  low_df <- read.csv(lowess_summary, stringsAsFactors = FALSE) %>% mutate(detrend = "LOESS (span=0.40)")

  both_df <- bind_rows(lin_df, low_df) %>%
    filter(model %in% names(label_map)) %>%
    mutate(label = label_map[model], group = group_map[model]) %>%
    filter(!is.na(label), !is.na(weighted_mae))

  model_order <- lin_df %>% arrange(weighted_mae) %>% pull(model)
  both_df$label <- factor(both_df$label, levels = rev(label_map[model_order[model_order %in% names(label_map)]]))

  naive_refs <- both_df %>% filter(model == "Naive_Median") %>% group_by(detrend) %>% slice(1)

  p6 <- ggplot(both_df, aes(x = label, y = weighted_mae, fill = detrend)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    geom_hline(data = naive_refs, aes(yintercept = weighted_mae, colour = detrend),
               linetype = "dashed", linewidth = 0.9, show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values  = c("Linear" = "#90CAF9", "LOESS (span=0.40)" = "#A5D6A7"),
                      name    = "Detrend method") +
    scale_colour_manual(values = c("Linear" = "#1565C0", "LOESS (span=0.40)" = "#2E7D32")) +
    labs(
      title    = sprintf("Weighted MAE: Linear vs LOESS Detrending (%s)", region),
      subtitle = "Dashed lines = Naive (median) reference for each detrend method",
      x = NULL, y = "Weighted MAE (mhl)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(colour = "grey40", size = 10),
      legend.position    = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank()
    )

  fname6 <- sprintf("plot_detrend_comparison_%s.png", region_lower)
  ggsave(file.path(out_dir, fname6), p6, width = 11, height = 7, dpi = 150)
  cat("Saved:", fname6, "\n")
}

cat(sprintf("\nAll plots saved to %s/ with _%s suffix.\n", out_dir, region_lower))
cat("To regenerate:\n")
cat(sprintf("  Rscript src/rscripts/plot_model_comparison.R %s\n", region))

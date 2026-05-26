# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                    Detrend Method Diagnostics
#                    by:  hugonogueira
#
#  Compares "linear" vs "lowess" detrending of Wine_mhl production time
#  series for both regions (RDD, RVV).
#
#  The goal is NOT to pick the method with the lowest CV error (that would
#  be leaking the model selection step into the same data used to evaluate
#  predictions). Instead, we select based on the statistical properties of
#  the residuals — a detrended series is only useful if the trend was
#  correctly removed, leaving stationary, uncorrelated residuals.
#
#  Tests applied to residuals from each method × region:
#    1. ADF  (Augmented Dickey-Fuller)  — stationarity (want p < 0.05)
#    2. KPSS (Kwiatkowski-Phillips-Schmidt-Shin) — stationarity (want p > 0.05)
#    3. Ljung-Box lag 1  — autocorrelation (want p > 0.05)
#    4. Ljung-Box lag 3  — autocorrelation (want p > 0.05)
#    5. Shapiro-Wilk     — normality       (want p > 0.05)
#    6. AR(1) coefficient — magnitude of 1-lag autocorrelation (want |rho| small)
#
#  Outputs:
#    data/comparison/detrend_diag_rdd.png   — 6-panel diagnostic plot, RDD
#    data/comparison/detrend_diag_rvv.png   — 6-panel diagnostic plot, RVV
#    data/comparison/detrend_diagnostics_table.csv  — full test results
#    Console summary with significance markers and automatic recommendation
#
#  Requires: tidyverse, tseries
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# libs --------------------------------------------------------------------------------

library(tidyverse)

if (!requireNamespace("tseries", quietly = TRUE)) install.packages("tseries")
library(tseries)

source("src/rscripts/utils/config.R")        # pulls in loess_span (shared)
source("src/rscripts/utils/detrend_utils.R")


# config ------------------------------------------------------------------------------
# loess_span comes from utils/config.R. This script deliberately compares BOTH
# detrending methods, so it ignores detrend_method from config and sweeps over
# its own `methods` vector below.

regions     <- c("rdd", "rvv")
methods     <- c("linear", "lowess")
out_dir     <- "data/comparison"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


# helper: significance stars ----------------------------------------------------------

sig_stars <- function(p) {
  if (is.na(p))   return("   ")
  if (p < 0.001)  return("***")
  if (p < 0.01)   return("** ")
  if (p < 0.05)   return("*  ")
  return("   ")
}

# helper: single-test result row ------------------------------------------------------

make_row <- function(region, method, test_name, statistic, p_value,
                     h0, want_reject, passed) {
  data.frame(
    region     = region,
    method     = method,
    test       = test_name,
    statistic  = round(statistic, 4),
    p_value    = round(p_value,   4),
    h0         = h0,
    want_reject= want_reject,
    passed     = passed,
    stars      = sig_stars(p_value),
    stringsAsFactors = FALSE
  )
}


# main diagnostic loop ----------------------------------------------------------------

all_results  <- list()    # collect test result rows
all_residuals <- list()   # residuals keyed by "{region}_{method}"

for (reg in regions) {

  # --- load data ---
  csv_path <- file.path('data', paste0('dataPrep_cont_dataset_', reg, '.csv'))
  if (!file.exists(csv_path)) {
    cat('[!] File not found, skipping region:', reg, '\n')
    next
  }
  df <- read.csv(csv_path)

  cat('\n', strrep('=', 64), '\n', sep = '')
  cat('  Region:', toupper(reg),
      '| n =', nrow(df), '| years:', min(df$year), '—', max(df$year), '\n')
  cat(strrep('=', 64), '\n')

  for (meth in methods) {

    cat('\n  --- Method:', meth, '---\n')

    # --- fit trend and get residuals ---
    tr   <- get_trend_residuals(df, method = meth, span = loess_span, verbose = TRUE)
    resid <- tr$residuals

    key <- paste0(reg, '_', meth)
    all_residuals[[key]] <- list(
      resid  = resid,
      years  = df$year,
      raw    = df$Wine_mhl,
      fitted = df$Wine_mhl - resid,
      region = toupper(reg),
      method = meth
    )

    ts_resid <- ts(resid, start = min(df$year), frequency = 1)
    rows     <- list()

    # 1. ADF — H0: unit root (non-stationary); reject → stationary
    adf_res  <- tryCatch(adf.test(ts_resid, alternative = "stationary"),
                          error = function(e) NULL)
    if (!is.null(adf_res)) {
      passed <- adf_res$p.value < 0.05
      cat('    ADF      p =', round(adf_res$p.value, 4),
          sig_stars(adf_res$p.value),
          if (passed) '✓ stationary' else '✗ non-stationary (unit root not rejected)', '\n')
      rows[[length(rows) + 1]] <- make_row(toupper(reg), meth,
        "ADF", adf_res$statistic, adf_res$p.value,
        "unit root present (non-stationary)", TRUE, passed)
    }

    # 2. KPSS — H0: level-stationary; fail to reject → stationary
    kpss_res <- tryCatch(kpss.test(ts_resid, null = "Level"),
                          error = function(e) NULL)
    if (!is.null(kpss_res)) {
      passed <- kpss_res$p.value > 0.05
      cat('    KPSS     p =', round(kpss_res$p.value, 4),
          sig_stars(kpss_res$p.value),
          if (passed) '✓ stationary' else '✗ non-stationary (KPSS rejected)', '\n')
      rows[[length(rows) + 1]] <- make_row(toupper(reg), meth,
        "KPSS", kpss_res$statistic, kpss_res$p.value,
        "series is level-stationary", FALSE, passed)
    }

    # 3. Ljung-Box lag 1 — H0: no autocorrelation up to lag 1
    lb1 <- tryCatch(Box.test(ts_resid, lag = 1, type = "Ljung-Box"),
                    error = function(e) NULL)
    if (!is.null(lb1)) {
      passed <- lb1$p.value > 0.05
      cat('    LB lag=1 p =', round(lb1$p.value, 4),
          sig_stars(lb1$p.value),
          if (passed) '✓ no autocorrelation' else '✗ autocorrelation at lag 1', '\n')
      rows[[length(rows) + 1]] <- make_row(toupper(reg), meth,
        "Ljung-Box lag=1", lb1$statistic, lb1$p.value,
        "no autocorrelation up to lag 1", FALSE, passed)
    }

    # 4. Ljung-Box lag 3 — H0: no autocorrelation up to lag 3
    lb3 <- tryCatch(Box.test(ts_resid, lag = 3, type = "Ljung-Box"),
                    error = function(e) NULL)
    if (!is.null(lb3)) {
      passed <- lb3$p.value > 0.05
      cat('    LB lag=3 p =', round(lb3$p.value, 4),
          sig_stars(lb3$p.value),
          if (passed) '✓ no autocorrelation' else '✗ autocorrelation at lags 1-3', '\n')
      rows[[length(rows) + 1]] <- make_row(toupper(reg), meth,
        "Ljung-Box lag=3", lb3$statistic, lb3$p.value,
        "no autocorrelation up to lag 3", FALSE, passed)
    }

    # 5. Shapiro-Wilk — H0: residuals are normally distributed
    sw  <- tryCatch(shapiro.test(resid), error = function(e) NULL)
    if (!is.null(sw)) {
      passed <- sw$p.value > 0.05
      cat('    Shapiro  p =', round(sw$p.value, 4),
          sig_stars(sw$p.value),
          if (passed) '✓ normal' else '✗ non-normal residuals', '\n')
      rows[[length(rows) + 1]] <- make_row(toupper(reg), meth,
        "Shapiro-Wilk", sw$statistic, sw$p.value,
        "residuals are normally distributed", FALSE, passed)
    }

    # 6. AR(1) coefficient — fit AR(1) to residuals, report phi
    ar1  <- tryCatch(ar(ts_resid, order.max = 1, aic = FALSE, method = "yule-walker"),
                     error = function(e) NULL)
    ar1_coef <- if (!is.null(ar1)) round(ar1$ar[1], 4) else NA_real_
    cat('    AR(1) φ  =', ar1_coef,
        '  (|φ| <', 0.20, '→ low serial dependence)\n')

    rows[[length(rows) + 1]] <- data.frame(
      region      = toupper(reg),
      method      = meth,
      test        = "AR(1) phi",
      statistic   = ar1_coef,
      p_value     = NA_real_,
      h0          = "phi is the lag-1 autocorrelation coefficient",
      want_reject = NA,
      passed      = if (!is.na(ar1_coef)) abs(ar1_coef) < 0.20 else NA,
      stars       = "",
      stringsAsFactors = FALSE
    )

    all_results <- c(all_results, rows)
  }
}


# compile results table ---------------------------------------------------------------

results_df <- bind_rows(all_results)

# count tests passed (exclude AR(1) NA p-value row from pass count)
summary_tbl <- results_df %>%
  filter(test != "AR(1) phi") %>%
  group_by(region, method) %>%
  summarise(
    n_tests = n(),
    n_pass  = sum(passed, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(
    results_df %>%
      filter(test == "AR(1) phi") %>%
      select(region, method, ar1_phi = statistic),
    by = c("region", "method")
  )


# print comparison table --------------------------------------------------------------

cat('\n', strrep('=', 64), '\n', sep = '')
cat('  SUMMARY — Tests passed (higher = better residual quality)\n')
cat(strrep('=', 64), '\n')
cat(sprintf('  %-6s  %-8s  %10s  %10s\n',
            'Region', 'Method', 'Passed/5', 'AR(1) phi'))
cat(strrep('-', 40), '\n')

for (i in seq_len(nrow(summary_tbl))) {
  row <- summary_tbl[i, ]
  cat(sprintf('  %-6s  %-8s  %5d / %d      %8.4f\n',
              row$region, row$method, row$n_pass, row$n_tests,
              ifelse(is.na(row$ar1_phi), 0, row$ar1_phi)))
}
cat(strrep('-', 40), '\n')


# automatic recommendation ------------------------------------------------------------

cat('\n  RECOMMENDATIONS\n')
cat(strrep('-', 64), '\n')

for (reg in toupper(regions)) {

  sub <- summary_tbl %>% filter(region == reg)
  if (nrow(sub) < 2) next

  lin  <- sub %>% filter(method == "linear")
  low  <- sub %>% filter(method == "lowess")

  lin_pass  <- lin$n_pass;  low_pass  <- low$n_pass
  lin_ar1   <- abs(ifelse(is.na(lin$ar1_phi), 0, lin$ar1_phi))
  low_ar1   <- abs(ifelse(is.na(low$ar1_phi), 0, low$ar1_phi))

  if (lin_pass > low_pass) {
    winner <- "linear"
    reason <- sprintf("passes more stationarity/normality tests (%d vs %d)",
                      lin_pass, low_pass)
  } else if (low_pass > lin_pass) {
    winner <- "lowess"
    reason <- sprintf("passes more stationarity/normality tests (%d vs %d)",
                      low_pass, lin_pass)
  } else {
    # tie on test count — break by AR(1)
    if (lin_ar1 <= low_ar1) {
      winner <- "linear"
      reason <- sprintf("tie on tests (%d/%d each); lower AR(1): %.4f vs %.4f",
                        lin_pass, low_pass, lin_ar1, low_ar1)
    } else {
      winner <- "lowess"
      reason <- sprintf("tie on tests (%d/%d each); lower AR(1): %.4f vs %.4f",
                        low_pass, lin_pass, low_ar1, lin_ar1)
    }
  }

  cat(sprintf('  %s → preferred method: %-8s  (%s)\n', reg, winner, reason))
}

cat(strrep('=', 64), '\n\n')


# save CSV ----------------------------------------------------------------------------

write.csv(results_df,
          file.path(out_dir, 'detrend_diagnostics_table.csv'),
          row.names = FALSE)
cat('Saved test results:', file.path(out_dir, 'detrend_diagnostics_table.csv'), '\n')


# 6-panel diagnostic plots ------------------------------------------------------------
# One PNG per region.  Panels:
#   [1,1] Trend overlay on raw series   [1,2] Residuals vs time (both methods)
#   [2,1] ACF — linear residuals        [2,2] ACF — lowess residuals
#   [3,1] Q-Q plot (both methods)       [3,2] Density overlay

for (reg in regions) {

  lin_key <- paste0(reg, '_linear')
  low_key <- paste0(reg, '_lowess')

  if (!(lin_key %in% names(all_residuals)) ||
      !(low_key %in% names(all_residuals))) next

  lin_data <- all_residuals[[lin_key]]
  low_data <- all_residuals[[low_key]]

  region_label <- toupper(reg)
  out_png <- file.path(out_dir, paste0('detrend_diag_', reg, '.png'))

  png(out_png, width = 1400, height = 1050, res = 110)
  par(mfrow = c(3, 2), mar = c(4, 4.5, 3, 1.5), oma = c(0, 0, 3, 0))

  years <- lin_data$years
  raw   <- lin_data$raw

  # --- Panel 1: Trend overlay ---
  plot(years, raw, type = 'l', lwd = 1.5, col = 'grey40',
       xlab = 'Year', ylab = 'Wine_mhl',
       main = 'Trend overlay on raw series',
       ylim = range(c(raw, lin_data$fitted, low_data$fitted), na.rm = TRUE))
  lines(years, lin_data$fitted, col = '#4E79A7', lwd = 2.2, lty = 1)
  lines(years, low_data$fitted, col = '#F28E2B', lwd = 2.2, lty = 2)
  legend('topleft', legend = c('Observed', 'Linear trend', 'LOWESS trend'),
         col = c('grey40', '#4E79A7', '#F28E2B'), lty = c(1, 1, 2),
         lwd = c(1.5, 2.2, 2.2), bty = 'n', cex = 0.85)

  # --- Panel 2: Residuals vs time ---
  y_lim <- range(c(lin_data$resid, low_data$resid), na.rm = TRUE)
  plot(years, lin_data$resid, type = 'l', lwd = 2, col = '#4E79A7',
       xlab = 'Year', ylab = 'Residual (mhl)',
       main = 'Residuals vs time', ylim = y_lim)
  lines(years, low_data$resid, lwd = 2, col = '#F28E2B', lty = 2)
  abline(h = 0, lty = 3, col = 'grey60')
  legend('topleft', legend = c('Linear', 'LOWESS'),
         col = c('#4E79A7', '#F28E2B'), lty = c(1, 2), lwd = 2,
         bty = 'n', cex = 0.85)

  # --- Panel 3: ACF — linear residuals ---
  acf(lin_data$resid, main = 'ACF — Linear residuals',
      col = '#4E79A7', lwd = 2, lag.max = 15)

  # --- Panel 4: ACF — LOWESS residuals ---
  acf(low_data$resid, main = 'ACF — LOWESS residuals',
      col = '#F28E2B', lwd = 2, lag.max = 15)

  # --- Panel 5: Q-Q plot ---
  lin_qq <- qqnorm(lin_data$resid, plot.it = FALSE)
  low_qq <- qqnorm(low_data$resid, plot.it = FALSE)

  xlim <- range(c(lin_qq$x, low_qq$x))
  ylim <- range(c(lin_qq$y, low_qq$y))

  plot(lin_qq$x, lin_qq$y, pch = 16, col = '#4E79A7', cex = 0.85,
       xlab = 'Theoretical quantiles', ylab = 'Sample quantiles',
       main = 'Q-Q plot (Normal)',
       xlim = xlim, ylim = ylim)
  points(low_qq$x, low_qq$y, pch = 17, col = '#F28E2B', cex = 0.85)
  qqline(lin_data$resid, col = '#4E79A7', lwd = 1.5, lty = 2)
  qqline(low_data$resid, col = '#F28E2B', lwd = 1.5, lty = 2)
  legend('topleft', legend = c('Linear', 'LOWESS'),
         col = c('#4E79A7', '#F28E2B'), pch = c(16, 17),
         bty = 'n', cex = 0.85)

  # --- Panel 6: Density overlay ---
  d_lin <- density(lin_data$resid, na.rm = TRUE)
  d_low <- density(low_data$resid, na.rm = TRUE)
  xlim  <- range(c(d_lin$x, d_low$x))
  ylim  <- range(c(0, d_lin$y, d_low$y))

  plot(d_lin, col = '#4E79A7', lwd = 2, main = 'Residual density',
       xlab = 'Residual (mhl)', xlim = xlim, ylim = ylim)
  lines(d_low, col = '#F28E2B', lwd = 2, lty = 2)

  # add normal curve for reference
  mu_lin <- mean(lin_data$resid, na.rm = TRUE)
  sd_lin <- sd(lin_data$resid,   na.rm = TRUE)
  xseq   <- seq(xlim[1], xlim[2], length.out = 200)
  lines(xseq, dnorm(xseq, mu_lin, sd_lin), col = 'grey50', lwd = 1.2, lty = 3)

  legend('topleft', legend = c('Linear', 'LOWESS', 'Normal ref.'),
         col = c('#4E79A7', '#F28E2B', 'grey50'), lty = c(1, 2, 3),
         lwd = c(2, 2, 1.2), bty = 'n', cex = 0.85)

  # overall title
  mtext(paste0(region_label, ' — Detrend Diagnostic Plots'),
        outer = TRUE, cex = 1.3, font = 2, line = 0.8)

  dev.off()
  cat('Saved diagnostic plot:', out_png, '\n')
}


# done --------------------------------------------------------------------------------

cat('\nAll outputs saved to:', out_dir, '\n')
cat('Done.\n')

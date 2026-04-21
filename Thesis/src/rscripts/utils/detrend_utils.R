# ============================================================
#  detrend_utils.R  —  Shared detrending utilities
#  by: hugonogueira
#
#  Provides three functions that work identically for both
#  "linear" (lm) and "lowess" (loess) detrending methods,
#  so all pipeline scripts share one consistent implementation.
#
#  Usage:
#    source("src/rscripts/utils/detrend_utils.R")
#
#    # Fit a trend model on training data
#    model <- fit_trend(train_df, method = detrend_method, span = loess_span)
#
#    # Predict trend value at a new observation (e.g. test year)
#    offset <- apply_trend(model, newdata = test_obs)
#
#    # Fit + get residuals in one call (full-dataset or fold-level)
#    tr <- get_trend_residuals(train_df, target_df = test_fold,
#                              method = detrend_method, span = loess_span)
#    tr$residuals   # numeric vector of residuals for target_df
#    tr$model       # the fitted model (can be reused with apply_trend)
# ============================================================


# fit_trend -------------------------------------------------------------------
#
# Fits a trend model to Wine_mhl ~ year on the supplied data frame.
# Both 'year' (numeric or factor) and 'Wine_mhl' must be present.
#
# Arguments:
#   train_df  data.frame with columns 'year' and 'Wine_mhl'
#   method    "linear"  → lm()    — fits a straight-line trend
#             "lowess"  → loess() — fits a locally-weighted smooth trend
#   span      Fraction of points used in each local loess fit (default 0.75).
#             Only used when method = "lowess". Higher = smoother.
#             Keep fixed across runs for reproducibility.
#
# Returns: a fitted model object that supports predict()

fit_trend <- function(train_df, method = "linear", span = 0.75) {

  # Always work in numeric year space; handles both numeric and factor columns
  tmp <- data.frame(
    Wine_mhl = train_df$Wine_mhl,
    year     = as.numeric(as.character(train_df$year))
  )

  if (method == "linear") {
    lm(Wine_mhl ~ year, data = tmp)

  } else if (method == "lowess") {
    loess(Wine_mhl ~ year, data = tmp, span = span)

  } else {
    stop("Unknown detrend method: '", method,
         "'. Valid options: 'linear' or 'lowess'.")
  }
}


# apply_trend -----------------------------------------------------------------
#
# Predicts the trend value at each row of newdata.
# Works for both lm and loess objects returned by fit_trend().
#
# Note on loess extrapolation: when newdata$year falls outside the training
# range, loess extrapolates linearly from the nearest edge points.
# In walk-forward CV (test year = train_max + 1) this is always a one-step
# extrapolation, which is acceptable.
#
# Arguments:
#   trend_model  Object returned by fit_trend()
#   newdata      data.frame with column 'year' (numeric or factor)
#
# Returns: numeric vector, length == nrow(newdata)

apply_trend <- function(trend_model, newdata) {
  nd <- data.frame(year = as.numeric(as.character(newdata$year)))
  as.numeric(predict(trend_model, newdata = nd))
}


# get_trend_residuals ---------------------------------------------------------
#
# Convenience wrapper: fit trend on train_df, return residuals for target_df.
#
# When train_df == target_df  →  full-dataset detrend (scripts 3, 4, global
#                                 class boundaries in 7).
# When train_df != target_df  →  fold-correct detrend: trend fitted only on
#                                 training fold, applied to test fold (scripts 6, 7 CV).
#                                 No information from target_df leaks into the
#                                 trend estimate.
#
# Arguments:
#   train_df   data.frame used to fit the trend (must have 'year', 'Wine_mhl')
#   target_df  data.frame to compute residuals for (defaults to train_df)
#   method     "linear" or "lowess"
#   span       loess span (only used when method = "lowess")
#   verbose    if TRUE, prints a short summary of the fitted trend
#
# Returns: list(residuals, model)
#   $residuals  numeric vector, length == nrow(target_df)
#   $model      fitted model object (reuse with apply_trend() if needed)

get_trend_residuals <- function(train_df, target_df = NULL,
                                method = "linear", span = 0.75,
                                verbose = TRUE) {

  if (is.null(target_df)) target_df <- train_df

  model  <- fit_trend(train_df, method = method, span = span)
  fitted <- apply_trend(model, newdata = target_df)
  resids <- target_df$Wine_mhl - fitted

  if (verbose) {
    if (method == "linear") {
      b0 <- round(coef(model)[1], 1)
      b1 <- round(coef(model)[2], 3)
      cat("    Trend (linear): Wine_mhl =", b0, "+", b1, "* year\n")
    } else {
      yrs <- as.numeric(as.character(train_df$year))
      cat("    Trend (lowess): span =", span,
          "| train years [", min(yrs), "-", max(yrs), "]",
          "| fitted range [",
          round(min(apply_trend(model, train_df)), 1), ",",
          round(max(apply_trend(model, train_df)), 1), "]\n")
    }
  }

  list(residuals = resids, model = model)
}

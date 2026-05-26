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
# For loess: R's predict.loess() returns NA for years outside the training
# range. We handle this with linear extrapolation from the boundary: fit a
# short OLS line through the last `extrap_tail` fitted training values and
# project forward. This is conservative (smooth trend continues linearly)
# and correct for one-step-ahead walk-forward CV.
#
# Arguments:
#   trend_model   Object returned by fit_trend()
#   newdata       data.frame with column 'year' (numeric or factor)
#   extrap_tail   Number of tail training points used to estimate the
#                 extrapolation slope (loess only). Default 5.
#
# Returns: numeric vector, length == nrow(newdata)

apply_trend <- function(trend_model, newdata, extrap_tail = 5) {
  nd   <- data.frame(year = as.numeric(as.character(newdata$year)))
  pred <- as.numeric(predict(trend_model, newdata = nd))

  # loess: fill NAs (out-of-range years) with linear extrapolation
  if (inherits(trend_model, "loess") && any(is.na(pred))) {
    # Recover training years + fitted values from the loess object
    train_years  <- as.numeric(trend_model$x)
    train_fitted <- as.numeric(fitted(trend_model))

    # Use the last extrap_tail points to estimate slope at the boundary
    n_tail  <- min(extrap_tail, length(train_years))
    tail_yr <- tail(train_years,  n_tail)
    tail_ft <- tail(train_fitted, n_tail)
    slope   <- coef(lm(tail_ft ~ tail_yr))[["tail_yr"]]
    intercept_val <- tail(tail_ft, 1) - slope * tail(tail_yr, 1)

    na_idx       <- which(is.na(pred))
    pred[na_idx] <- intercept_val + slope * nd$year[na_idx]
  }

  pred
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

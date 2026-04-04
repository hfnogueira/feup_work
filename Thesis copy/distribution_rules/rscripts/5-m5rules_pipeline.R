# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       M5Rules Pipeline
#                       by:  hugonogueira
#
#  Applies M5Rules (WEKA's regression rule learner) to predict wine
#  production (Wine_mhl) as a continuous variable.
#
#  Unlike RIPPER (classifier), M5Rules works directly in the regression
#  space — no target discretisation needed. Each rule has a linear model
#  at its leaf, making predictions continuous and directly comparable
#  to CarenR subgroup means.
#
#  Pipeline:
#    1. Load continuous feature dataset (output of features creation_v2.R)
#    2. Train M5Rules on full dataset — compare rules with CarenR
#    3. Evaluate with k-fold cross-validation (MAE, RMSE, R²)
#    4. Compare against naive baseline (predict training mean)
#    5. Print and save discovered rules
#
#  Requires: RWeka, tidyverse
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# libs --------------------------------------------------------------------------------

library(tidyverse)

if (!requireNamespace("RWeka", quietly = TRUE)) install.packages("RWeka")
library(RWeka)


# config ------------------------------------------------------------------------------

file     <- 1    # 1 = RDD   |   other = RVV
n_folds  <- 10   # cross-validation folds
seed     <- 42

# M5Rules control options:
#   -N  use unsmoothed predictions (default: smoothed)
#   -U  use unsmoothed linear models in leaves
# Default settings (smoothed) are generally better for small datasets
# We keep defaults but expose them here for experimentation
m5_unsmoothed <- FALSE   # set TRUE to disable smoothing


# load data ---------------------------------------------------------------------------

if (file == 1) {
   df     <- read.csv(file = '../data/dataPrep_cont_dataset_rdd.csv')
   region <- 'RDD'
} else {
   df     <- read.csv(file = '../data/dataPrep_cont_dataset_rvv.csv')
   region <- 'RVV'
}

cat('------------------------------------------------------------\n')
cat('Region:', region, '| Observations:', nrow(df), '| Features:', ncol(df) - 2, '\n')
cat('Global Wine_mhl — Mean:', round(mean(df$Wine_mhl), 2),
    '| SD:', round(sd(df$Wine_mhl), 2),
    '| Min:', round(min(df$Wine_mhl), 2),
    '| Max:', round(max(df$Wine_mhl), 2), '\n')
cat('------------------------------------------------------------\n')


# prepare feature matrix --------------------------------------------------------------
# Wine_mhl stays CONTINUOUS — no discretisation needed

df_m5 <- df %>% select(-year)

cat('\nDimensions going into M5Rules:', nrow(df_m5), 'rows x', ncol(df_m5) - 1, 'features\n')


# naive baseline ----------------------------------------------------------------------
# Always predicting the training mean — the floor any model must beat

naive_pred   <- mean(df_m5$Wine_mhl)
naive_errors <- df_m5$Wine_mhl - naive_pred
naive_mae    <- mean(abs(naive_errors))
naive_rmse   <- sqrt(mean(naive_errors^2))
naive_r2     <- 0   # by definition, predicting the mean gives R² = 0

cat('\n=== Naive Baseline (always predict training mean) ===\n')
cat('Predicted value:', round(naive_pred, 2), 'mhl\n')
cat('MAE :', round(naive_mae,  2), 'mhl\n')
cat('RMSE:', round(naive_rmse, 2), 'mhl\n')
cat('R²  : 0.000 (by definition)\n')


# train M5Rules -----------------------------------------------------------------------

cat('\n============================================================\n')
cat(' Training M5Rules on full dataset ...\n')
cat('============================================================\n')

set.seed(seed)

ctrl <- if (m5_unsmoothed) Weka_control(N = TRUE) else Weka_control()

model <- M5Rules(Wine_mhl ~ ., data = df_m5, control = ctrl)

cat('\n=== Discovered Rules ===\n')
print(model)


# training set performance ------------------------------------------------------------

train_pred   <- predict(model, df_m5)
train_errors <- df_m5$Wine_mhl - train_pred
train_mae    <- mean(abs(train_errors))
train_rmse   <- sqrt(mean(train_errors^2))
ss_res       <- sum(train_errors^2)
ss_tot       <- sum((df_m5$Wine_mhl - mean(df_m5$Wine_mhl))^2)
train_r2     <- 1 - ss_res / ss_tot

cat('\n=== Training Set Performance ===\n')
cat('MAE :', round(train_mae,  2), 'mhl\n')
cat('RMSE:', round(train_rmse, 2), 'mhl\n')
cat('R²  :', round(train_r2,   3), '\n')


# cross-validation evaluation ---------------------------------------------------------

cat('\n============================================================\n')
cat(' ', n_folds, '-fold Cross Validation\n')
cat('============================================================\n')

eval_cv <- evaluate_Weka_classifier(
   model,
   numFolds   = n_folds,
   complexity = FALSE,
   seed       = seed
)

print(eval_cv)

# extract CV metrics manually
cv_mae  <- eval_cv$details['meanAbsoluteError']
cv_rmse <- eval_cv$details['rootMeanSquaredError']
cv_rae  <- eval_cv$details['relativeAbsoluteError']
cv_rse  <- eval_cv$details['rootRelativeSquaredError']

# R² from relative squared error: R² = 1 - (RRSE²)
cv_r2 <- 1 - (cv_rse / 100)^2

cat('\n=== Cross-Validation Summary ===\n')
cat('MAE :', round(cv_mae,  2), 'mhl\n')
cat('RMSE:', round(cv_rmse, 2), 'mhl\n')
cat('R²  :', round(cv_r2,   3), '\n')

cat('\n=== vs Naive Baseline ===\n')
cat('MAE  improvement over baseline:', round((1 - cv_mae  / naive_mae)  * 100, 1), '%\n')
cat('RMSE improvement over baseline:', round((1 - cv_rmse / naive_rmse) * 100, 1), '%\n')


# per-rule coverage -------------------------------------------------------------------

cat('\n=== Rule Coverage on Training Data ===\n')
cat('(shows how many years each rule fires on the full dataset)\n\n')

# get predictions and residuals per observation
df_m5$predicted  <- train_pred
df_m5$residual   <- train_errors
df_m5$abs_error  <- abs(train_errors)

cat('Overall training predictions:\n')
cat('Mean predicted:', round(mean(train_pred), 2), 'mhl\n')
cat('Mean actual:   ', round(mean(df_m5$Wine_mhl), 2), 'mhl\n')
cat('Correlation (predicted vs actual):', round(cor(df_m5$Wine_mhl, train_pred), 3), '\n')


# save rules to text file -------------------------------------------------------------

rules_file <- paste0('../data/m5rules_rules_', tolower(region), '.txt')

sink(rules_file)
cat('M5Rules —', region, '\n')
cat('Generated:', format(Sys.time(), '%Y-%m-%d %H:%M'), '\n')
cat('Observations:', nrow(df_m5), '\n')
cat('Target: Wine_mhl (continuous)\n\n')

cat('=== Naive Baseline ===\n')
cat('MAE:', round(naive_mae, 2), '| RMSE:', round(naive_rmse, 2), '\n\n')

cat('=== Discovered Rules ===\n')
print(model)

cat('\n=== Training Performance ===\n')
cat('MAE:', round(train_mae, 2), '| RMSE:', round(train_rmse, 2), '| R2:', round(train_r2, 3), '\n')

cat('\n=== Cross-Validation (', n_folds, 'folds) ===\n')
print(eval_cv)
cat('R2 (CV):', round(cv_r2, 3), '\n')
sink()

cat('\nRules saved to:', rules_file, '\n')
cat('Done.\n')

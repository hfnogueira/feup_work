# Thesis Block — Target Variable Detrending

## Consistent Treatment of the Target Variable Across All Analyses

A central methodological decision in this work concerns the definition of the target variable. Wine production (Wine\_mhl) exhibits a long-term upward linear trend over the observation period, reflecting structural improvements in viticulture, vineyard management, and regional expansion rather than climate variability alone. If raw production values are used as the target, observations from early decades are structurally assigned to the Low class and observations from recent decades to the High class, regardless of the climatic conditions in those years. This would cause any learned model to partially capture era-level effects rather than the climate-to-production relationship that motivates the analysis.

To remove this confound, a linear detrending procedure was applied consistently across all three analytical frameworks. A linear model of the form Wine\_mhl = α + β × year was fitted, and the residuals — representing the deviation of each year's production from the expected trend value — were used as the effective target variable throughout. Positive residuals correspond to above-trend production years; negative residuals correspond to below-trend years. The classification thresholds (Low, Medium, High) and distribution rule discovery are therefore defined relative to era-expected production, making the class labels and distributional patterns comparable across the full temporal range of the dataset.

This detrending strategy was applied consistently and in a methodologically sound manner across all three components of the experimental framework:

In the **descriptive distribution rule analysis**, the trend was estimated on the full dataset, which is appropriate since the goal is characterisation rather than prediction. The rules discovered describe climate conditions associated with above- or below-trend production years across the entire historical record.

In the **classification model comparison** (carenR vs RIPPER), a cross-validation-correct detrending procedure was used. Within each fold of the 10-fold stratified cross-validation, the linear trend was estimated exclusively from the training partition and applied to both training and test observations before class assignment. This prevents any information from the held-out fold from influencing the target construction, preserving the integrity of the evaluation.

In the **walk-forward temporal validation**, the same fold-correct principle was applied in a stricter temporal sense. At each step T, the trend was fitted on all years up to T−1 and used to detrend both the training window and the single test observation. Model predictions were made in residual space and subsequently converted back to the original mhl scale by adding the trend value at year T. This ensures that all reported error metrics (MAE, RMSE, R²) remain interpretable in the original units of wine production.

The consistent use of detrending across all analyses ensures that the three components of the experimental framework — descriptive rule discovery, classifier comparison, and temporal forecasting — operate on the same underlying definition of the target variable, making their results directly comparable and jointly coherent as a unified study.

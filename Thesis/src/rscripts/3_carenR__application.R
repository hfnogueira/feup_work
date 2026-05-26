# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       carenR algorithm application
#                       by:  hugonogueira
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# libs --------------------------------------------------------------------------------

# install CarenR from my computer if need
#remotes::install_local("/Users/hugonogueira/git_hugo/feup/Thesis/CarenR")

library(carenR)
library(tidyverse)
library(gt)



# config ---------------------------------------------------------------------------------
# Shared params (detrend, detrend_method, loess_span) come from utils/config.R.
# Change them THERE — they propagate to every pipeline script.

source("src/rscripts/utils/config.R")
source("src/rscripts/utils/detrend_utils.R")


# read dataset ------------------------------------------------------------------------

repeat {

  file <- menu(c("RDD", "RVV"), title = "Select dataset:")
  region = c("RDD", "RVV")[file]

  if (file %in% c(1, 2)) break

  cat("Invalid option. Please select either 1 (RDD) or 2 (RVV).\n")
}

path <- c(
  "data/dataPrep_dis_rule_dataset_rdd.csv",
  "data/dataPrep_dis_rule_dataset_rvv.csv"
)[file]
1
df <- read.csv(path)

glimpse(df)

head(df)

# show df in a nice table
table <- knitr::kable(head(df), caption = "First 6 rows of the dataset")
gt(head(df))

for (col in names(df)) {
  if (col == 'Wine_mhl') {
    df[[col]] = as.numeric(df[[col]])
  } else{
    df[[col]] = as.factor(df[[col]])
  }
  cat(col, '---', class(df[[col]]), '\n')
}


# detrend (optional) ------------------------------------------------------------------

if (detrend) {

  cat('\n--- Detrending enabled (method:', detrend_method, ') ---\n')

  # df_for_trend: numeric year + Wine_mhl, needed by detrend_utils
  df_for_trend <- data.frame(
    year     = as.numeric(as.character(df$year)),
    Wine_mhl = df$Wine_mhl
  )

  tr <- get_trend_residuals(df_for_trend, method = detrend_method,
                            span = loess_span, verbose = TRUE)

  df$Wine_mhl_original <- df$Wine_mhl   # keep raw values for reference
  df$Wine_mhl          <- tr$residuals  # replace with deviations from trend

  cat('Wine_mhl now contains RESIDUALS (deviation from', detrend_method, 'trend).\n')
  cat('Positive = above-trend year  |  Negative = below-trend year\n')
  cat('Residual mean:', round(mean(df$Wine_mhl), 2),
      '| SD:', round(sd(df$Wine_mhl), 2), '\n\n')

} else {

  cat('\n--- Detrending disabled — using raw Wine_mhl (original behaviour) ---\n\n')

}


# Build distribution rule set --------------------------------------------------------------------------------
par(mfrow = c(1, 1))
ts.plot(df$Wine_mhl,
        main = if (detrend) paste(region, '— Wine_mhl residuals (detrended)')
               else         paste(region, '— Wine_mhl raw'))


# Remove the year column --------------------------------------------------------------------------------

df <- df %>% select(-year)

# build the distribution rule set --------------------------------------------------------------------------------

drs <- caren(
  df,
  Dist = TRUE,
  POI = 'Wine_mhl',
  min.sup = 0.2,
  min.conf = 0.1
)

drs


"src/rscripts/"


# Show the rules ----------------------------------------------------------------------

print("---------------")
cat("Showing rules for the region: ", region)


# View rules
plot.drs(drs)

# View next rules
plot.drs(drs, iterate = TRUE)


# View specific rules -----------------------------------------------------------------

# View the first rule represented as a frequency polygon
drs[1,]$Dist

# View the first rule using density estimation
# use plot.drs with n=1, m=1 to show just one rule (simplest):
# print the number of rules founded
cat("There are", max(0,nrow(drs) - 1), "rules founded")
cat("There are", max(0,nrow(drs %>% filter(Ant_sup >= 0.2, Ant_sup < 1))
), "rules founded with sup > 20%")


#print the rules with sup > 20%
drs %>% filter(Ant_sup >= 0.2, Ant_sup < 1)

rule = 23
# st is the argument to the rule
plot.drs(drs, st = rule, n = 1, m = 1)   # st = rule index you want
#plot.dr(drs[1,], dist.rep="boxplot")


# save distribution rules to CSV (used by script 6 — walk-forward validation) --------
# File name encodes region AND detrend method so both runs can coexist.

if (detrend) {
  # Save CarenR rules alongside the RIPPER / M5Rules outputs so all model
  # artefacts for a given detrend method live in one folder.
  rules_out_dir  <- file.path('data', detrend_method)
  if (!dir.exists(rules_out_dir)) dir.create(rules_out_dir, recursive = TRUE)

  rules_out_file <- file.path(rules_out_dir,
                              paste0('RulesTemp_', tolower(region),
                                     '_', detrend_method, '.csv'))

  # write.table with sep=";" dec="." matches the format load_carenr_rules() in
  # script 6 expects: read.csv(path, sep = ";", dec = ".", ...).
  # Convert any list-type columns (e.g. Subgroup from Dist = TRUE) to
  # character so write.table can serialize them.
  drs_save <- drs
  for (col in names(drs_save)) {
    if (is.list(drs_save[[col]])) {
      drs_save[[col]] <- sapply(drs_save[[col]],
                                function(x) paste(as.character(x), collapse = "|"),
                                USE.NAMES = FALSE)
    }
  }
  write.table(drs_save, rules_out_file, sep = ";", dec = ".",
              row.names = FALSE, quote = FALSE)
  cat('\nDistribution rules saved to:', rules_out_file, '\n')
  cat('Script 6 will load this file when detrend_method = "', detrend_method, '".\n\n', sep = '')
}


rule <- 1
max_rule <- nrow(drs) -1

repeat {
  
  cat("\nShowing rule:", rule, "\n")
  
  plot.drs(drs, st = rule, n = 1, m = 1)
  
  title(
    main = paste(
      "Rule", rule,
      " for", region, "region ",
      "| suport:", round(drs$Ant_sup[rule], 3)
    )
  )
  user_input <- readline(prompt = "Press [Enter] for next, 'q' to quit: ")
  
  if (user_input == "q") break
  
  rule <- rule + 1
  
  if (rule > max_rule) {
    cat("Reached last rule. Restarting...\n")
    rule <- 1
  }
}



# examples ----------------------------------------------------------------------------

# dont execute this block, it is just for demonstration of how to use CarenR with different datasets and settings.
if (FALSE) {
  # Attribute-Value (UCI like) dataset
  data(mushrooms)
  
  head(mushrooms)
  
  # build rule set, should take less than 1 minute
  rls <- caren(mushrooms,
               class = 'class',
               min.sup = 0.1,
               imp = 0.001)
  
  # view first 10 rules
  ar.pp(rls[1:10, ])
  
  # view 10 rules randomly chosen
  ar.pp(rls[sample(1:nrow(rls), 10), ])
  
  
  # view 10 rules with highest support
  ar.pp(rls[order(rls$Sup, decreasing = TRUE)[1:10], ])
  
  # Two Column Datasets
  # Load dataset with basket (two column) format
  data(greengrocers)
  
  head(greengrocers)
  
  # Build rule set
  rls <- caren(greengrocers, Bas = TRUE)
  
  # view first rule
  rls[1, ]
  
  # Distribution Rules
  # Load dataset with one numerical Property of Interest (MPG)
  data(auto.mpg.discr)
  
  head(auto.mpg.discr)
  
  # Build distribution rule set
  drs <- caren(auto.mpg.discr, Dist = TRUE, POI = 'MPG')
  
  auto.mpg.discr$HP %>% unique()
  
  # View rules
  plot.drs(drs)
  
  # View next rules
  plot.drs(drs, iterate = TRUE)
  
  mushrooms$class = as.numeric(mushrooms$class)
  
  drs <- caren(mushrooms, Dist = TRUE, POI = 'class')
  plot.drs(drs)
  plot.drs(drs, iterate = TRUE)
  
  for (i in 1:ncol(auto.mpg.discr)) {
    print(class(auto.mpg.discr[, i]))
  }
  
  
  # Example data frame
  df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  
  # Discretize x into intervals of unequal width
  df$interval <- cut(df$x, breaks = c(0, 3, 7, Inf))
  
  # View the result
  df
  
  
  
  # Example data frame
  df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  
  # Discretize x into 3 intervals of equal width
  df$interval <- cut(df$x, breaks = 3)
  
  # View the result
  df
  
}

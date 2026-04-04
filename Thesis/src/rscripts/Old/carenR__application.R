# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       carenR algorithm application
#                       by:  hugonogueira
#                       at:  Fri May 12 13:47:05 2023
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# libs --------------------------------------------------------------------------------

# install CarenR from my computer if need
#remotes::install_local("/Users/hugonogueira/git_hugo/feup/Thesis/CarenR")

library(carenR)
library(tidyverse)
library(gt)



# read dataset ------------------------------------------------------------------------

repeat {
  
  file <- menu(c("RDD", "RVV"), title = "Select dataset:")
  
  if (file %in% c(1, 2)) break
  
  cat("Invalid option. Please select either 1 (RDD) or 2 (RVV).\n")
}

path <- c(
  "../data/dataPrep_dis_rule_dataset_rdd.csv",
  "../data/dataPrep_dis_rule_dataset_rvv.csv"
)[file]

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


# Build distribution rule set --------------------------------------------------------------------------------

ts.plot(df$Wine_mhl)

drs <- caren(
  df,
  Dist = TRUE,
  POI = 'Wine_mhl',
  min.sup = 0.2,
  min.conf = 0.1
)

drs

# print the rules





# Show the rules ----------------------------------------------------------------------


# View rules
plot.drs(drs)

# View next rules
plot.drs(drs, iterate = TRUE)



# examples ----------------------------------------------------------------------------

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
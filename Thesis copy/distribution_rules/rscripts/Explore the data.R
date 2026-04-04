# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       carenR algorithm application
#                       by:  hugonogueira
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Librarys ----------------------------------------------------------------------------

library(tidyverse)
library(gt)


# select the file region --------------------------------------------------------------

repeat {
  
  file <- menu(c("RDD", "RVV"), title = "Select dataset:")
  region = c("RDD", "RVV")[file]
  
  if (file %in% c(1, 2)) break
  
  cat("Invalid option. Please select either 1 (RDD) or 2 (RVV).\n")
}



# read production and meteo data ----------------------------------------------------------------


if (file == 1) {
  
  data_prd <-
    read.csv(file = '../data/dataPrep_production_rdd.csv') %>%
    mutate(var = 'rdd', Wine_mhl = as.double(Wine_mhl))
  
  data_meteo <-
    read_csv(file = '../data/dataPrep_meteo_rdd.csv') %>%
    mutate(Year = lubridate::year(Date))
  
  
} else {
  
  ##  RVV data ----
  
  data_prd <-
    read.csv(file = '../data/dataPrep_production_rvv.csv') %>%
    mutate(var = 'rdd', Wine_mhl = as.double(Wine_mhl)) %>% 
    filter(Year != 2022)
  
  data_meteo <-
    read_csv(file = '../data/dataPrep_meteo_rvv.csv') %>%
    mutate(Year = lubridate::year(Date))
  
}


glimpse(data_prd)
glimpse(data_meteo)


# plot the production data -  Wine_mhl vesus Year
ggplot(data_prd, aes(x = Year, y = Wine_mhl)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Wine_mhl over the years in", region),
       x = "Year",
       y = "Wine_mhl (mhl)") +
  theme_minimal()




# read rules dataset ------------------------------------------------------------------------

repeat {
  
  file <- menu(c("RDD", "RVV"), title = "Select dataset:")
  region = c("RDD", "RVV")[file]
  
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

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       data preparation stage 1
#                       by:  hugonogueira
#                       at:  Mon Apr 10 23:45:14 2023
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# libs --------------------------------------------------------------------

library(tidyverse)
library(readxl)




# Meterological data ------------------------------------------------------------------


# Date
# Dia: day of the month
# Doy: Day of the year

# Climate:
#   Tmed: mean temperature
#   Tmin: Minimum temperature
#   Tmax: Maximum temperature
#   Rain: precipitation

# SM: soil moisture
# IAF: Leaf Area Index




df_rdd <-
  read_excel("../data/RDD/Dados_Finais_regua_1933_2022.xls", 
             sheet = "Matrix_modelacao") %>%
  select(Date, Month, Dom, Doy, Tmax, Tmin, Tmed, Rain, Sm, Iaf , Eto)

str(df_rdd)



df_rvv <-
  read_excel("../data/RVV/dados_finais_Braga_1941_2022_IM.xlsx", 
             sheet = "Daily_meteo") %>%
  select(Date, Month, Dom, Doy, Tmax, Tmin, Tmed, Rain, Sm, Iaf , Eto)

str(df_rvv)



# features engineering ----------------------------------------------------------------

## expert insights  ----------------------


# wt and fc for RDD region 
wt = 402
fc = 142.8



df_rdd <-
  df_rdd %>%
  mutate(
    `R>0.1` = ifelse(Rain > 0.1, 1, 0),       # R>0,mm: days with R>0,1mm
    `Tmed<15` = ifelse(Tmed < 15, 1, 0),      # Tmed<15>: days with mean temperature <15ºC
    `Tmax>35` = ifelse(Tmax > 35, 1, 0),      # Tmax>35>: days with maximum temperature >35ºC
    `Tmin<0` = ifelse(Tmin < 0, 1, 0),        # Tmin<0: days with minimum temperature < 0ºC
    `Sm<1.5Wp` = ifelse(Sm < 1.5 * wt, 1, 0), # Sm<1.5Wp>: days with soil water less than 1.5 times the wilting point (wt)
    `Sm>0.9Fc` = ifelse(Sm > 0.9 * fc, 1, 0)  # Sm>0.9Fc>: days with soil water higher than 0.9 times the field capacity (fc)
  )


# wt and fc for RVV region 
wt = 338
fc = 77


df_rvv <-
  df_rvv %>%
  mutate(
    `R>0.1` = ifelse(Rain > 0.1, 1, 0),       # R>0,mm: days with R>0,1mm
    `Tmed<15` = ifelse(Tmed < 15, 1, 0),      # Tmed<15>: days with mean temperature <15ºC
    `Tmax>35` = ifelse(Tmax > 35, 1, 0),      # Tmax>35>: days with maximum temperature >35ºC
    `Tmin<0` = ifelse(Tmin < 0, 1, 0),        # Tmin<0: days with minimum temperature < 0ºC
    `Sm<1.5Wp` = ifelse(Sm < 1.5 * wt, 1, 0), # Sm<1.5Wp>: days with soil water less than 1.5 times the wilting point (wt)
    `Sm>0.9Fc` = ifelse(Sm > 0.9 * fc, 1, 0)  # Sm>0.9Fc>: days with soil water higher than 0.9 times the field capacity (fc)
  )


head(df_rdd)
head(df_rvv)


# save metereological row data ----------------------------------------------------------------------

# saveRDS(object = df_rdd, file = '../data/dataPrep_meteo_rdd')
# saveRDS(object = df_rvv, file = '../data/dataPrep_meteo_rvv')

write.csv(x = df_rdd,
          file = '../data/dataPrep_meteo_rdd.csv',
          row.names = FALSE)
write.csv(x = df_rvv,
          file = '../data/dataPrep_meteo_rvv.csv',
          row.names = FALSE)


# Production Data ---------------------------------------------------------------------

df_prd_rdd <-
  read_excel("../data/RDD/Pheno_Prd_1933_2022_recente.xlsx", sheet = "data") %>%
  rename(Wine_mhl = `Wine (MhL)`)


df_prd_rvv <-
  read_excel("../data/RVV/Pheno_Prd_1941_2022_recente.xlsx", sheet = "data") %>%
  rename(Wine_mhl = `Wine (MhL)`)



# save production row data ----------------------------------------------------------------------

# saveRDS(object = df_prd_rdd, file = '../data/dataPrep_production_rdd')
# saveRDS(object = df_prd_rvv, file = '../data/dataPrep_production_rvv')

write.csv(x = df_prd_rdd,
          file = '../data/dataPrep_production_rdd.csv',
          row.names = FALSE)
write.csv(x = df_prd_rvv,
          file = '../data/dataPrep_production_rvv.csv',
          row.names = FALSE)

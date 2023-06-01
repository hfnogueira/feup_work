# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       Dataset creation
#                       by:  hugonogueira
#                       at:  Mon May  1 00:22:48 2023
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# libs --------------------------------------------------------------------------------
library(tidyverse)
library(arules)

# read data for RDD region ---------------------------------------------------------------------------

## meterological data ----


data_meteo <-
  read_csv(file = '../data/dataPrep_meteo_rdd.csv') %>%
  mutate(Year = lubridate::year(Date))

head(data_meteo)

# names(data_meteo)
# "Date"    "Month"   "Dom"
# "Doy"     "Tmax"    "Tmin"
# "Tmed"    "Rain"    "Sm"
# "Iaf"     "Eto"     "R>0.1"
# "Tmed<15" "Tmax>35" "Tmin<-2"


## production data ----

data_prd_rdd <-
  read.csv(file = '../data/dataPrep_production_rdd.csv') %>%
  mutate(var = 'rdd', Wine_mhl = as.double(Wine_mhl))


head(data_prd_rdd)

#names(data_prd_rdd)
# "Year"     "Wine_mhl" "DOY_BB"
# "DOY_Fl"   "DOY_sM"   "DOY_Fs"
# "DOY_Hv"   "var"




# Features engineering -------------------------------------------------------------------

## Build the data frame

##  main loop ----
## Execute this loop over the years of the production data set



# create empty data frame -------------------------------------------------------------


my_df <-
  data.frame(Year = numeric(),
             Wine_mhl = numeric(),
             tm_fl_y1_c = numeric(),
             tm_fl_y1_b1 = numeric(),
             tm_fl_y1_a1 = numeric(),
             tm_fl_y0_b1 = numeric(),
             tm_fl_y0_a1 = numeric(),
             tm_bb_y0_a1 = numeric(),
             tm_bb_y0_a2 = numeric(),
             tm_sm_y1_c = numeric(),
             tm_sm_y0_a1 = numeric(),
             tm_sm_y0_b1 = numeric(),
             tm_hv_y1_c = numeric(),
             tm_hv_y1_b1 = numeric(),
             tn_hv_y0_a1 = numeric(),
             tm.days.less.15_fl_y1_c = numeric(),
             tm.days.less.15_fl_y1_b1 = numeric(),
             tm.days.less.0_bb_y1_b1 = numeric(),
             tm.days.less.0_bb_y1_a1 = numeric(),
             tm.days.less.0_fl_y1_a1 = numeric(),
             tm.days.above.35_fl_y1_a1 = numeric(),
             tm.days.above.35_fl_y1_a2 = numeric(),
             tm.days.above.35_sm_y1_b1 = numeric(),
             tm.days.above.35_sm_y1_a1 = numeric(),
             rf_fl_y1_a1 = numeric(),
             rf_hv_y1_b1 = numeric(),
             rf_hv_y1_a1 = numeric(),
             rf.above.1mm_fl_y1_c = numeric(),
             rf.above.1mm_fl_y1_a1 = numeric(),
             rf.above.1mm_sm_y1_b1 = numeric(),
             rf.above.1mm_sm_y1_a1 = numeric(),
             rf.above.1mm_hv_y1_c = numeric(),
             rf.above.1mm_hv_y1_a1 = numeric(),
             iaf_bb_y1_c = numeric(),
             iaf_fl_y1_b1 = numeric(),
             iaf_fl_y1_b2 = numeric(),
             iaf_fl_y1_b3 = numeric(),
             iaf_fl_y1_a1 = numeric(),
             iaf_hv_y1_c = numeric(),
             iaf_fl_y0_b1 = numeric(),
             iaf_fl_y0_b2 = numeric(),
             iaf_fl_y0_b3 = numeric(),
             iaf_fl_y0_a1 = numeric(),
             swa_fl_y1_a1 = numeric(),
             swa_fl_y1_a2 = numeric(),
             swa_hv_y1_b1 = numeric(),
             swa_hv_y1_a1 = numeric(),
             swa_hv_y0_a1 = numeric(),
             swa_hv_y0_a2 = numeric(),
             sw.less.15wp_fl_y1_a1 = numeric(),
             sw.less.15wp_fl_y1_a2 = numeric(),
             sw.less.15wp_sm_y1_a1 = numeric(),
             sw.less.15wp_sm_y1_a2 = numeric(),
             sw.less.15wp_hv_y1_b1 = numeric(),
             sw.less.15wp_hv_y1_a1 = numeric(),
             sw.less.15wp_fl_y0_a1 = numeric(),
             sw.less.15wp_fl_y0_a2 = numeric(),
             sw.above.09fc_fl_y1_a1 = numeric(),
             sw.above.09fc_fl_y1_a2 = numeric(),
             sw.above.09fc_fl_y0_a1 = numeric(),
             sw.above.09fc_fl_y0_a2 = numeric()
             )




for (i in 2:nrow(data_prd_rdd)) {

  ##  Year ----
  year = data_prd_rdd$Year[i]
  
  cat('Processing Year: ', year,'\n')
  
  ##  Wine production ----
  
  Wine_mhl = data_prd_rdd$Wine_mhl[i]
  
  
  
  
  # Mean temperature (tm) ---------------------------------------------------------------

  
  ##  Mean Temperature Flowering year 1 ----
  
    C = 15;  B1 = 10; A1 = 10
    
    pheno_day = data_prd_rdd$DOY_Fl[i]
  
  # tm_fl_y1_c
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    tm_fl_y1_c = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Tmed)
  
  
  # tm_fl_y1_b1
    var = B1
    period = (pheno_day - var + 1): pheno_day
    tm_fl_y1_b1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Tmed)
  
  
 # tm_fl_y1_a1
    var = A1
    period = pheno_day:(pheno_day + var - 1)
    tm_fl_y1_a1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Tmed)
  
    C = NA;  B1 = NA; A1 = NA
    
    

    
  ##  Mean Temperature Flowering year 0  ----
  
    B1 = 10; A1 = 10
    
    pheno_day = data_prd_rdd$DOY_Fl[i-1]
  
  # tm_fl_y0_b1
    var = B1
    period = (pheno_day - var + 1):pheno_day
    tm_fl_y0_b1 = mean((data_meteo %>% 
                          filter(Year == year -1, Doy %in% period))$Tmed)
    
  # tm_fl_y0_a1
    var = A1
    period = pheno_day:(pheno_day + var - 1)
    tm_fl_y0_a1 = mean((data_meteo %>% 
                          filter(Year == year -1, Doy %in% period))$Tmed)
  
    B1 = NA; A1 = NA

  
  

  
  ##  Mean Temperature Bud Break year 0  ----
  
    A1 = 15; A2 = 30
    
    pheno_day = data_prd_rdd$DOY_BB[i-1]
    
    # tm_bb_y0_a1
    var = A1
    period = (pheno_day - var + 1):pheno_day
    tm_bb_y0_a1 = mean((data_meteo %>% 
                          filter(Year == year -1, Doy %in% period))$Tmed)
    
    # tm_bb_y0_a2
    var = A2
    period = pheno_day:(pheno_day + var - 1)
    tm_bb_y0_a2 = mean((data_meteo %>% 
                          filter(Year == year -1, Doy %in% period))$Tmed)  
    
    A1 = NA; A2 = NA
    
    
  
  
  
    
    
  
  ##  Mean Temperature Start of Maturity year 1 ----
  
    C = 20;  A1 = 20
    
    pheno_day = data_prd_rdd$DOY_sM[i]
  
  # tm_sm_y1_c
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    tm_sm_y1_c = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Tmed)
  
  # tm_sm_y0_a1
    var = A1
    period = pheno_day:(pheno_day + var - 1)
    tm_sm_y0_a1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Tmed)

    C = NA;  A1 = NA
    
    
    
    
  ##  Mean Temperature Start of Maturity year 0 ----
    
      B1 = 20
      
      pheno_day = data_prd_rdd$DOY_sM[i-1]
    
    # tm_sm_y0_b1
      var = B1
      period = (pheno_day - var + 1):pheno_day
      tm_sm_y0_b1 = mean((data_meteo %>% 
                            filter(Year == year -1, Doy %in% period))$Tmed)
    
    
     B1 = NA
    
   
    
  ##  Mean Temperature Harvesting year 1 ----
  
    C = 10;  B1 = 20
    
    pheno_day = data_prd_rdd$DOY_Hv[i]
    
  # tm_hv_y1_c
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    tm_hv_y1_c = mean((data_meteo %>% 
                        filter(Year == year, Doy %in% period))$Tmed)
  
  
  # tm_hv_y1_b1
    var = B1
    period = (pheno_day - var + 1): pheno_day
    tm_hv_y1_b1 = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Tmed)

  
    C = NA;  B1 = NA
    
    
  
  ##  Mean Temperature Harvesting year 0 ----
  
    A1 = 30
    
    pheno_day = data_prd_rdd$DOY_Hv[i-1]
    
    # tn_hv_y0_a1
      var = A1
      period = pheno_day:(pheno_day + var - 1)
      tn_hv_y0_a1 = mean((data_meteo %>% 
                           filter(Year == year -1, Doy %in% period))$Tmed)
      
    A1 = NA
  
  
  
    
    
    

  ##  Mean Temperature < 15ºC Flowering year 1 ----
  
  # count number of days
    
    C = 15;  B1 = 15
    
    pheno_day = data_prd_rdd$DOY_Fl[i]
    
    # tm.days.less.15_fl_y1_c
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    tm.days.less.15_fl_y1_c = sum(data_meteo %>% 
                                 filter(Year == year, Doy %in% period) %>% 
                                 select(`Tmed<15`))
    
    
    # tm.days.less.15_fl_y1_b1
    var = B1
    period = (pheno_day - var + 1): pheno_day
    tm.days.less.15_fl_y1_b1 = sum(data_meteo %>% 
                                  filter(Year == year, Doy %in% period) %>% 
                                  select(`Tmed<15`))
    
    C = NA;  B1 = NA
    
    
    
    

# Minimum temperature (ti) ------------------------------------------------------------

 
  ##  Min Temperature < 0ºC Bud Break year 1 ----
    
    # count number of days
    
    C = 20;  A1 = 30
    
    pheno_day = data_prd_rdd$DOY_BB[i]
    
    # tm.days.less.0_bb_y1_b1
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    tm.days.less.0_bb_y1_b1 = sum(data_meteo %>%
                                 filter(Year == year, Doy %in% period) %>%
                                 select(`Tmin<0`))
    
    
    # tm.days.less.0_bb_y1_a1
    var = A1
    period = (pheno_day + var - 1): pheno_day
    tm.days.less.0_bb_y1_a1 = sum(data_meteo %>% 
                                  filter(Year == year, Doy %in% period) %>% 
                                  select(`Tmin<0`))
    
    C = NA;   A1 = NA
    
    
    
  ##  Min Temperature < 0ºC Flowering year 1 ----
    
    # count number of days
    
    A1 = 10
    
    pheno_day = data_prd_rdd$DOY_Fl[i]
    
    

    # tm.days.less.0_fl_y1_a1
    var = A1
    period = (pheno_day + var - 1): pheno_day
    tm.days.less.0_fl_y1_a1 = sum(data_meteo %>% 
                                  filter(Year == year, Doy %in% period) %>% 
                                  select(`Tmin<0`))
    
    A1 = NA

    
    
# Maximum temperature (ti) ------------------------------------------------------------
   
   
  ##  Max Temperature > 35ºC Flowering year 1 ----
   
   # count number of days
   
   A1 = 10; A2 = 10
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # tm.days.above.35_fl_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   tm.days.above.35_fl_y1_a1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`Tmax>35`))
   
  # tm.days.above.35_fl_y1_a2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   tm.days.above.35_fl_y1_a2 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`Tmax>35`))
   
   A1 = NA;   A2 = NA
   
   
   
   
  ##  Max Temperature > 35ºC Start of Maturity  ----
   
   # count number of days
   
   B1 = 20; A1 = 20
   
   pheno_day = data_prd_rdd$DOY_sM[i]

   # tm.days.above.35_sm_y1_b1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   tm.days.above.35_sm_y1_b1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`Tmax>35`))
   
   # tm.days.above.35_sm_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   tm.days.above.35_sm_y1_a1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`Tmax>35`))
   
   B1 = NA;   A1 = NA
   


 # Rainfall ----------------------------------------------------------------------------


     
  ##  Rain quantity Flowering year 1 ----
   # sum rain quantity
   
   A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   

   # rf_fl_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   rf_fl_y1_a1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(Rain))
   
  
   
   A1 = NA
   
   
   
   ##  Rain quantity Harvesting year 1 ----
   # sum rain quantity
   
   B1 = 10; A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # rf_hv_y1_b1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   rf_hv_y1_b1 = sum(data_meteo %>% 
                      filter(Year == year, Doy %in% period) %>% 
                      select(Rain))
   
   # rf_hv_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   rf_hv_y1_a1 = sum(data_meteo %>% 
                       filter(Year == year, Doy %in% period) %>% 
                       select(Rain))
   
   
   
   B1 = NA; A1 = NA
   
   
   
  ##  Rain  >1mm Flowering year 1 ----
  # count number of days
   
   C = 15;  A1 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
  
   # rf.above.1mm_fl_y1_c
   var = C
   period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
   rf.above.1mm_fl_y1_c = sum(data_meteo %>%
                                filter(Year == year, Doy %in% period) %>%
                                select(`R>0.1`))
   
   
   # rf.above.1mm_fl_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   rf.above.1mm_fl_y1_a1 = sum(data_meteo %>% 
                                 filter(Year == year, Doy %in% period) %>% 
                                 select(`R>0.1`))
   
   C = NA;   A1 = NA
   
   
   
  ##  Rain>1mm Start of maturity year 1 ----
  # count number of days
   
   B1 = 20;  A1 = 20
   
   pheno_day = data_prd_rdd$DOY_sM[i]
   
   # rf.above.1mm_sm_y1_b1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   rf.above.1mm_sm_y1_b1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`R>0.1`))
   
   
   # rf.above.1mm_sm_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   rf.above.1mm_sm_y1_a1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`R>0.1`))
   
   B1 = NA;  A1 = NA
   
   
  ##  Rain >1mm Harvesting year 1 ----
  # count number of days
   
   C = 10;  A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # rf.above.1mm_hv_y1_c
   var = C
   period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
   rf.above.1mm_hv_y1_c = sum(data_meteo %>%
                                filter(Year == year, Doy %in% period) %>%
                                select(`R>0.1`))
   
   
   # rf.above.1mm_hv_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   rf.above.1mm_hv_y1_a1 = sum(data_meteo %>% 
                                 filter(Year == year, Doy %in% period) %>% 
                                 select(`R>0.1`))
   
   C = NA;   A1 = NA



# IAF (Indice Area Foliar) ------------------------------------------------------------

   ##  Iaf  Bud break year 1 ----
   # average values
  
   C = 10
   
   pheno_day = data_prd_rdd$DOY_BB[i]
   
   # iaf_bb_y1_c
   var = C
   period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
   iaf_bb_y1_c = mean((data_meteo %>% 
                        filter(Year == year, Doy %in% period))$Iaf)
   
   
   
   ##  Iaf  Flowering year 1 ----
   # average values
   
   B1 = 10; B2 = 20; B3 = 30; A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # iaf_fl_y1_b1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   iaf_fl_y1_b1 = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Iaf)
   
   # iaf_fl_y1_b2
   var = B2
   period = (pheno_day - var + 1): pheno_day
   iaf_fl_y1_b2 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Iaf)
   
   # iaf_fl_y1_b3
   var = B3
   period = (pheno_day - var + 1): pheno_day
   iaf_fl_y1_b3 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Iaf)
   
   
   # iaf_fl_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   iaf_fl_y1_a1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Iaf)
   
   B1 = NA; B2 = NA; B3 = NA; A1 = NA
   
   
   
   
   ##  Iaf  Harvesting year 1 ----
   # average values
   
   C = 10
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # iaf_hv_y1_c
   var = C
   period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
   iaf_hv_y1_c = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Iaf)
   
   C = NA
   
   
   ##  Iaf  Flowering year 0 ----
   # average values
   
   B1 = 10; B2 = 20; B3 = 30; A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Fl[i-1]
   
   # iaf_fl_y0_b1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   iaf_fl_y0_b1 = mean((data_meteo %>% 
                         filter(Year == year - 1, Doy %in% period))$Iaf)
   
   # iaf_fl_y0_b2
   var = B2
   period = (pheno_day - var + 1): pheno_day
   iaf_fl_y0_b2 = mean((data_meteo %>% 
                          filter(Year == year - 1, Doy %in% period))$Iaf)
   
   # iaf_fl_y0_b3
   var = B3
   period = (pheno_day - var + 1): pheno_day
   iaf_fl_y0_b3 = mean((data_meteo %>% 
                          filter(Year == year - 1, Doy %in% period))$Iaf)
   
   
   # iaf_fl_y0_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   iaf_fl_y0_a1 = mean((data_meteo %>% 
                          filter(Year == year - 1, Doy %in% period))$Iaf)
   
   
   B1 = NA; B2 = NA; B3 = NA; A1 = NA
   
   

# Available soil water ----------------------------------------------------------------

   
   ##  swa  Flowering year 1 ----
   # average values
   
   A1 =15 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # swa_fl_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   swa_fl_y1_a1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Sm)
   
   
   # swa_fl_y1_a2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   swa_fl_y1_a2 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Sm)
  
   A1 =NA ; A2 = NA
   
   
   
   
   ##  swa  Harvesting year 1 ----
   # average values
   
   B1 =20 ; A1 = 20
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # swa_hv_y1_b1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   swa_hv_y1_b1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Sm)
   
   
   # swa_hv_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   swa_hv_y1_a1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Sm)
   B1 =NA ; A1 = NA
   
   
   
   ##  swa  Harvesting year 0 ----
   # average values
   
   A1 =15 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Hv[i-1]
   
   # swa_hv_y0_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   swa_hv_y0_a1 = mean((data_meteo %>% 
                         filter(Year == year - 1, Doy %in% period))$Sm)
   
   # swa_hv_y0_a2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   swa_hv_y0_a2 = mean((data_meteo %>% 
                         filter(Year == year - 1, Doy %in% period))$Sm)
   
   
   
   
   
   ##  swa <1.5xWp Flowering year 1 ----
   
   # S_c  e <1.5 x Wp, A1, 10; 
   # refere ao nº de ocorrência nos 10 dias após em que em a 
   # água no solo foi 1,5 x menor que Wp (wilting point)
   
   # Os valores de wilting point (Wp) e field capacity (Fc) dependem da região 
   # No Douro: Wp é de 402 e o Fc é de 142,8
   # Nos verdes: Wp é de 338 mm e o Fc é de 77 mm
   
   
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # sw.less.15wp_fl_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   sw.less.15wp_fl_y1_a1 = sum((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   # sw.less.15wp_fl_y1_a2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   sw.less.15wp_fl_y1_a2 = sum((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
  
   A1 =NA ; A2 = NA
   
   
   ##  sw_c <1.5 x Wp   Start of maturity year 1 ----
  
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_sM[i]
   
   # sw.less.15wp_sm_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   sw.less.15wp_sm_y1_a1 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   # sw.less.15wp_sm_y1_a2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   sw.less.15wp_sm_y1_a2 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   A1 =NA ; A2 = NA
   
  
   ##  sw_c <1.5 x Wp   Harvesting year 1 ----
   
   B1 =20 ; A1 = 20
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # sw.less.15wp_hv_y1_b1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   sw.less.15wp_hv_y1_b1 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   # sw.less.15wp_hv_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   sw.less.15wp_hv_y1_a1 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   B1 =NA ; A1 = NA
   
   
   
   ##  sw_c <1.5 x Wp   Flowering year 0 ----
   
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i-1]
   
   # sw.less.15wp_fl_y0_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   sw.less.15wp_fl_y0_a1 = sum((data_meteo %>% 
                                 filter(Year == year -1, Doy %in% period))$`Sm<1.5Wp`)
   
   
   # sw.less.15wp_fl_y0_a2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   sw.less.15wp_fl_y0_a2 = sum((data_meteo %>% 
                                 filter(Year == year -1, Doy %in% period))$`Sm<1.5Wp`)
   
   A1 =NA ; A2 = NA
   
   
   
   ##  sw_c >0.9 x Fc  Flowering year 1 ----

   # Douro:  Fc é de 142,8
   # Verdes: Fc é de 77 mm
   
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   

   # sw.above.09fc_fl_y1_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   sw.above.09fc_fl_y1_a1 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm>0.9Fc`)
   
   
   # sw.above.09fc_fl_y1_a2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   sw.above.09fc_fl_y1_a2 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm>0.9Fc`)
   
   A1 =NA ; A2 = NA
   
   
   
   ##  sw_c >0.9 x Fc  Flowering year 0 ----
   
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i-1]
   

   # sw.above.09fc_fl_y0_a1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   sw.above.09fc_fl_y0_a1 = sum((data_meteo %>% 
                                  filter(Year == year -1, Doy %in% period))$`Sm>0.9Fc`)
   
   
   # sw.above.09fc_fl_y0_a2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   sw.above.09fc_fl_y0_a2 = sum((data_meteo %>% 
                                  filter(Year == year -1, Doy %in% period))$`Sm>0.9Fc`)
   
   A1 =NA ; A2 = NA
   
   
   
   

# add all values to the data frame for the year ---------------------------------------

  
  
  
  my_df <- rbind(my_df,
                 data.frame(
                   Year = year,
                   Wine_mhl = Wine_mhl,
                   tm_fl_y1_c = tm_fl_y1_c,
                   tm_fl_y1_b1 = tm_fl_y1_b1,
                   tm_fl_y1_a1 = tm_fl_y1_a1,
                   tm_fl_y0_b1 = tm_fl_y0_b1,
                   tm_fl_y0_a1 = tm_fl_y0_a1,
                   tm_bb_y0_a1 = tm_bb_y0_a1,
                   tm_bb_y0_a2 = tm_bb_y0_a2,
                   tm_sm_y1_c = tm_sm_y1_c,
                   tm_sm_y0_a1 = tm_sm_y0_a1,
                   tm_sm_y0_b1 = tm_sm_y0_b1,
                   tm_hv_y1_c = tm_hv_y1_c,
                   tm_hv_y1_b1 = tm_hv_y1_b1,
                   tn_hv_y0_a1 = tn_hv_y0_a1,
                   tm.days.less.15_fl_y1_c = tm.days.less.15_fl_y1_c,
                   tm.days.less.15_fl_y1_b1 = tm.days.less.15_fl_y1_b1,
                   tm.days.less.0_bb_y1_b1 = tm.days.less.0_bb_y1_b1,
                   tm.days.less.0_bb_y1_a1 = tm.days.less.0_bb_y1_a1,
                   tm.days.less.0_fl_y1_a1 = tm.days.less.0_fl_y1_a1,
                   tm.days.above.35_fl_y1_a1 = tm.days.above.35_fl_y1_a1,
                   tm.days.above.35_fl_y1_a2 = tm.days.above.35_fl_y1_a2,
                   tm.days.above.35_sm_y1_b1 = tm.days.above.35_sm_y1_b1,
                   tm.days.above.35_sm_y1_a1 = tm.days.above.35_sm_y1_a1,
                   rf_fl_y1_a1 = rf_fl_y1_a1,
                   rf_hv_y1_b1 = rf_hv_y1_b1,
                   rf_hv_y1_a1 = rf_hv_y1_a1,
                   rf.above.1mm_fl_y1_c = rf.above.1mm_fl_y1_c,
                   rf.above.1mm_fl_y1_a1 = rf.above.1mm_fl_y1_a1,
                   rf.above.1mm_sm_y1_b1 = rf.above.1mm_sm_y1_b1,
                   rf.above.1mm_sm_y1_a1 = rf.above.1mm_sm_y1_a1,
                   rf.above.1mm_hv_y1_c = rf.above.1mm_hv_y1_c,
                   rf.above.1mm_hv_y1_a1 = rf.above.1mm_hv_y1_a1,
                   iaf_bb_y1_c = iaf_bb_y1_c,
                   iaf_fl_y1_b1 = iaf_fl_y1_b1,
                   iaf_fl_y1_b2 = iaf_fl_y1_b2,
                   iaf_fl_y1_b3 = iaf_fl_y1_b3,
                   iaf_fl_y1_a1 = iaf_fl_y1_a1,
                   iaf_hv_y1_c = iaf_hv_y1_c,
                   iaf_fl_y0_b1 = iaf_fl_y0_b1,
                   iaf_fl_y0_b2 = iaf_fl_y0_b2,
                   iaf_fl_y0_b3 = iaf_fl_y0_b3,
                   iaf_fl_y0_a1 = iaf_fl_y0_a1,
                   swa_fl_y1_a1 = swa_fl_y1_a1,
                   swa_fl_y1_a2 = swa_fl_y1_a2,
                   swa_hv_y1_b1 = swa_hv_y1_b1,
                   swa_hv_y1_a1 = swa_hv_y1_a1,
                   swa_hv_y0_a1 = swa_hv_y0_a1,
                   swa_hv_y0_a2 = swa_hv_y0_a2,
                   sw.less.15wp_fl_y1_a1 = sw.less.15wp_fl_y1_a1,
                   sw.less.15wp_fl_y1_a2 = sw.less.15wp_fl_y1_a2,
                   sw.less.15wp_sm_y1_a1 = sw.less.15wp_sm_y1_a1,
                   sw.less.15wp_sm_y1_a2 = sw.less.15wp_sm_y1_a2,
                   sw.less.15wp_hv_y1_b1 = sw.less.15wp_hv_y1_b1,
                   sw.less.15wp_hv_y1_a1 = sw.less.15wp_hv_y1_a1,
                   sw.less.15wp_fl_y0_a1 = sw.less.15wp_fl_y0_a1,
                   sw.less.15wp_fl_y0_a2 = sw.less.15wp_fl_y0_a2,
                   sw.above.09fc_fl_y1_a1 = sw.above.09fc_fl_y1_a1,
                   sw.above.09fc_fl_y1_a2 = sw.above.09fc_fl_y1_a2,
                   sw.above.09fc_fl_y0_a1 = sw.above.09fc_fl_y0_a1,
                   sw.above.09fc_fl_y0_a2 = sw.above.09fc_fl_y0_a2
                   
              
                 ))
  
  
}



# Discretize --------------------------------------------------------------------------

# Convert a Continuous Variable into a Categorical Variable
# This function implements several basic unsupervised methods to convert a continuous variable 
# into a categorical variable (factor) using different binning strategies. 
# For convenience, a whole data.frame can be discretized (i.e., all numeric columns 
# are discretized).

# Discretization method: 
# Available are: 
#     "interval" (equal interval width), 
#     "frequency" (equal frequency), 
#     "cluster" (k-means clustering)
#     "fixed" (categories specifies interval boundaries). 


#     Note that equal frequency does not achieve perfect equally sized groups if the 
#     data contains duplicated values.



my_df <- 
   my_df %>% select(-Year)



# transform into categorical variables the columns with less than 10 unique values


for (col in names(my_df)) {
   
   if (col == 'Wine_mhl') {
      next
   }
   
   lenCol = length(unique(my_df[[col]]))
   
   if (lenCol < 10) {
      cat(col, '--',lenCol ,'\n')
      my_df[[col]] = as.factor(my_df[[col]])
   }
}

# run discretization for the continuous variables

for (col in names(my_df)) {
   
   if (col == 'Wine_mhl') {
      next
   }
   
   if (!is.factor(my_df[[col]])) {
      cat('discretizing column ', col,'\n')
      my_df[[col]] <- discretize(my_df[[col]], method   = "frequency")
      
   }
}

str(my_df)



# Final output ------------------------------------------------------------------------


#### CHANGE NAMES VARIOABALES RO BE MORE ESY TRRO READ


write.csv(x = my_df,file = 'data/dis_rule_dataset_rdd.csv',
          row.names = FALSE)

getwd()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       Dataset creation
#                       by:  hugonogueira
#                       at:  Mon May  1 00:22:48 2023
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# libs --------------------------------------------------------------------------------
library(tidyverse)

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
             Fl_Y1_Tm_C = numeric(),
             Fl_Y1_Tm_B1 = numeric(),
             Fl_Y1_Tm_A1 = numeric(),
             Fl_Y0_Tm_B1 = numeric(),
             Fl_Y0_Tm_A1 = numeric(),
             H_Y1_Tm_C = numeric(),
             H_Y1_Tm_B1 = numeric(),
             H_Y0_Tm_A1 = numeric(),
             BB_Y0_Tm_A1 = numeric(),
             BB_Y0_Tm_A2 = numeric(),
             Fl_Y1_Tmed_less_15_C = numeric(),
             Fl_Y1_Tmed_less_15_B1 = numeric(),
             BB_Y1_Tmmin_less_0_C = numeric(),
             FL_Y1_Tmmin_less_0_A1 = numeric(),
             FL_Y1_Tmmax_above_35_A1 = numeric(),
             FL_Y1_Tmmax_above_35_A2 = numeric(),
             sM_Y1_Tmmax_above_35_B1 = numeric(),
             sM_Y1_Tmmax_above_35_A1= numeric(),
             FL_Y1_rs_A1 = numeric(),
             H_Y1_rs_B1 = numeric(),
             H_Y1_rs_A1 = numeric(),
             FL_Y1_rain_above_1mm_C = numeric(),
             FL_Y1_rain_above_1mm_A1 = numeric(),
             sM_Y1_rain_above_1mm_B1 = numeric(),
             sM_Y1_rain_above_1mm_A1 = numeric(),
             H_Y1_rain_above_1mm_C = numeric(),
             H_Y1_rain_above_1mm_A1 = numeric(),
             BB_Y1_Iaf_C = numeric(),
             FL_Y1_Iaf_B1 = numeric(),
             FL_Y1_Iaf_B2 = numeric(),
             FL_Y1_Iaf_B3 = numeric(),
             FL_Y1_Iaf_A1 = numeric(),
             H_Y1_Iaf_C = numeric(),
             FL_Y1_swa_A1 = numeric(),
             FL_Y1_swa_A2 = numeric(),
             H_Y0_swa_A1 = numeric(),
             H_Y0_swa_A2 = numeric(),
             FL_Y1_swc_Sm1.5Wp_A1 = numeric(),
             FL_Y1_swc_Sm1.5Wp_A2 = numeric(),
             H_Y1_swc_Sm1.5Wp_B1 = numeric(),
             H_Y1_swc_Sm1.5Wp_A1 = numeric(),
             FL_Y0_swc_Sm1.5Wp_A1 = numeric(),
             FL_Y0_swc_Sm1.5Wp_A2 = numeric(),
             FL_Y1_swc_Sm_0.9Fc_A1 = numeric(),
             FL_Y1_swc_Sm_0.9Fc_A2 = numeric(),
             FL_Y0_swc_Sm_0.9Fc_A1 = numeric(),
             FL_Y0_swc_Sm_0.9Fc_A2 = numeric()

             
             )




for (i in 2:nrow(data_prd_rdd)) {

  ##  Year ----
  year = data_prd_rdd$Year[i]
  
  print(paste('Year: ', year))
  
  ##  Wine production ----
  
  Wine_mhl = data_prd_rdd$Wine_mhl[i]
  
  
  
  
  # Mean temperature (tm) ---------------------------------------------------------------

  
  ##  Mean Temperature Flowering year 1 ----
  
    C = 15;  B1 = 10; A1 = 10
    
    pheno_day = data_prd_rdd$DOY_Fl[i]
  
  # Fl_Y1_Tm_C
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    Fl_Y1_Tm_C = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Tmed)
  
  
  # Fl_Y1_Tm_B1
    var = B1
    period = (pheno_day - var + 1): pheno_day
    Fl_Y1_Tm_B1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Tmed)
  
  
 # Fl_Y1_Tm_A1
    var = A1
    period = pheno_day:(pheno_day + var - 1)
    Fl_Y1_Tm_A1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Tmed)
  
    C = NA;  B1 = NA; A1 = NA
    
    

    
  ##  Mean Temperature Flowering year 0  ----
  
    B1 = 10; A1 = 10
    
    pheno_day = data_prd_rdd$DOY_Fl[i-1]
  
  # Fl_Y0_Tm_B1
    var = B1
    period = (pheno_day - var + 1):pheno_day
    Fl_Y0_Tm_B1 = mean((data_meteo %>% 
                          filter(Year == year -1, Doy %in% period))$Tmed)
    
  # Fl_Y0_Tm_A1
    var = A1
    period = pheno_day:(pheno_day + var - 1)
    Fl_Y0_Tm_A1 = mean((data_meteo %>% 
                          filter(Year == year -1, Doy %in% period))$Tmed)
  
    B1 = NA; A1 = NA

  
  

  
  ##  Mean Temperature Bud Break year 0  ----
  
    A1 = 15; A2 = 30
    
    pheno_day = data_prd_rdd$DOY_BB[i-1]
    
    # BB_Y0_Tm_A1
    var = A1
    period = (pheno_day - var + 1):pheno_day
    BB_Y0_Tm_A1 = mean((data_meteo %>% 
                          filter(Year == year -1, Doy %in% period))$Tmed)
    
    # BB_Y0_Tm_A2
    var = A2
    period = pheno_day:(pheno_day + var - 1)
    BB_Y0_Tm_A2 = mean((data_meteo %>% 
                          filter(Year == year -1, Doy %in% period))$Tmed)  
    
    A1 = NA; A2 = NA
    
    
  
  
  
    
    
  
  ##  Mean Temperature Start of Maturity year 1 ----
  
    C = 20;  A1 = 20
    
    pheno_day = data_prd_rdd$DOY_sM[i]
  
  # sM_Y1_Tm_C
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    sM_Y1_Tm_C = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Tmed)
  
  # sM_Y1_Tm_A1
    var = A1
    period = pheno_day:(pheno_day + var - 1)
    sM_Y1_Tm_A1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Tmed)

    C = NA;  A1 = NA
    
    
    
    
  ##  Mean Temperature Start of Maturity year 0 ----
    
      B1 = 20
      
      pheno_day = data_prd_rdd$DOY_sM[i-1]
    
    # sM_Y0_Tm_B1
      var = B1
      period = (pheno_day - var + 1):pheno_day
      sM_Y0_Tm_B1 = mean((data_meteo %>% 
                            filter(Year == year -1, Doy %in% period))$Tmed)
    
    
     B1 = NA
    
    
    
    
    
  ##  Mean Temperature Harvesting year 1 ----
  
    C = 10;  B1 = 20
    
    pheno_day = data_prd_rdd$DOY_Hv[i]
    
  # H_Y1_Tm_C
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    H_Y1_Tm_C = mean((data_meteo %>% 
                        filter(Year == year, Doy %in% period))$Tmed)
  
  
  # H_Y1_Tm_B1
    var = B1
    period = (pheno_day - var + 1): pheno_day
    H_Y1_Tm_B1 = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Tmed)

  
    C = NA;  B1 = NA
    
    
  
  ##  Mean Temperature Harvesting year 0 ----
  
    A1 = 30
    
    pheno_day = data_prd_rdd$DOY_Hv[i-1]
    
    # H_Y0_Tm_A1
      var = A1
      period = pheno_day:(pheno_day + var - 1)
      H_Y0_Tm_A1 = mean((data_meteo %>% 
                           filter(Year == year -1, Doy %in% period))$Tmed)
      
    A1 = NA
  
  
  
    
    
    

  ##  Mean Temperature < 15ºC Flowering year 1 ----
  
  # count number of days
    
    C = 15;  B1 = 15
    
    pheno_day = data_prd_rdd$DOY_Fl[i]
    
    # Fl_Y1_Tmed_less_15_C
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    Fl_Y1_Tmed_less_15_C = sum(data_meteo %>% 
                                 filter(Year == year, Doy %in% period) %>% 
                                 select(`Tmed<15`))
    
    
    # Fl_Y1_Tmed_less_15_B1
    var = B1
    period = (pheno_day - var + 1): pheno_day
    Fl_Y1_Tmed_less_15_B1 = sum(data_meteo %>% 
                                  filter(Year == year, Doy %in% period) %>% 
                                  select(`Tmed<15`))
    
    C = NA;  B1 = NA
    
    
    
    

# Minimum temperature (ti) ------------------------------------------------------------

 
  ##  Min Temperature < 0ºC Bud Break year 1 ----
    
    # count number of days
    
    C = 20;  A1 = 30
    
    pheno_day = data_prd_rdd$DOY_BB[i]
    
    # BB_Y1_Tmmin_less_0_C
    var = C
    period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
    BB_Y1_Tmmin_less_0_C = sum(data_meteo %>%
                                 filter(Year == year, Doy %in% period) %>%
                                 select(`Tmin<0`))
    
    
    # BB_Y1_Tmmin_less_0_A1
    var = A1
    period = (pheno_day + var - 1): pheno_day
    BB_Y1_Tmmin_less_0_A1 = sum(data_meteo %>% 
                                  filter(Year == year, Doy %in% period) %>% 
                                  select(`Tmin<0`))
    
    C = NA;   A1 = NA
    
    
    
  ##  Min Temperature < 0ºC Flowering year 1 ----
    
    # count number of days
    
    A1 = 10
    
    pheno_day = data_prd_rdd$DOY_Fl[i]
    
    
    # FL_Y1_Tmmin_less_0_A1
    var = A1
    period = (pheno_day + var - 1): pheno_day
    FL_Y1_Tmmin_less_0_A1 = sum(data_meteo %>% 
                                  filter(Year == year, Doy %in% period) %>% 
                                  select(`Tmin<0`))
    
    A1 = NA

    
    
# Maximum temperature (ti) ------------------------------------------------------------
   
   
  ##  Max Temperature > 35ºC Flowering year 1 ----
   
   # count number of days
   
   A1 = 10; A2 = 10
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # FL_Y1_Tmmax_above_35_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_Tmmax_above_35_A1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`Tmax>35`))
   
  # FL_Y1_Tmmax_above_35_A2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_Tmmax_above_35_A2 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`Tmax>35`))
   
   A1 = NA;   A2 = NA
   
   
   
   
  ##  Max Temperature > 35ºC Start of Maturity  ----
   
   # count number of days
   
   B1 = 20; A1 = 20
   
   pheno_day = data_prd_rdd$DOY_sM[i]
   
   # sM_Y1_Tmmax_above_35_B1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   sM_Y1_Tmmax_above_35_B1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`Tmax>35`))
   
   # sM_Y1_Tmmax_above_35_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   sM_Y1_Tmmax_above_35_A1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`Tmax>35`))
   
   B1 = NA;   A1 = NA
   


 # Rainfall ----------------------------------------------------------------------------


     
  ##  Rain quantity Flowering year 1 ----
   # sum rain quantity
   
   A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # FL_Y1_rs_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_rs_A1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(Rain))
   
  
   
   A1 = NA
   
   
   
   ##  Rain quantity Harvesting year 1 ----
   # sum rain quantity
   
   B1 = 10; A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # H_Y1_rs_B1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   H_Y1_rs_B1 = sum(data_meteo %>% 
                      filter(Year == year, Doy %in% period) %>% 
                      select(Rain))
   
   # H_Y1_rs_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   H_Y1_rs_A1 = sum(data_meteo %>% 
                       filter(Year == year, Doy %in% period) %>% 
                       select(Rain))
   
   
   
   B1 = NA; A1 = NA
   
   
   
  ##  Rain  >1mm Flowering year 1 ----
  # count number of days
   
   C = 15;  A1 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # FL_Y1_rain_above_1mm_C
   var = C
   period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
   FL_Y1_rain_above_1mm_C = sum(data_meteo %>%
                                filter(Year == year, Doy %in% period) %>%
                                select(`R>0.1`))
   
   
   # FL_Y1_rain_above_1mm_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_rain_above_1mm_A1 = sum(data_meteo %>% 
                                 filter(Year == year, Doy %in% period) %>% 
                                 select(`R>0.1`))
   
   C = NA;   A1 = NA
   
   
   
  ##  Rain  >1mm Start of maturity year 1 ----
  # count number of days
   
   B1 = 20;  A1 = 20
   
   pheno_day = data_prd_rdd$DOY_sM[i]
   
   # sM_Y1_rain_above_1mm_B1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   sM_Y1_rain_above_1mm_B1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`R>0.1`))
   
   
   # sM_Y1_rain_above_1mm_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   sM_Y1_rain_above_1mm_A1 = sum(data_meteo %>% 
                                   filter(Year == year, Doy %in% period) %>% 
                                   select(`R>0.1`))
   
   B1 = NA;  A1 = NA
   
   
  ##  Rain >1mm Harvesting year 1 ----
  # count number of days
   
   C = 10;  A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # H_Y1_rain_above_1mm_C
   var = C
   period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
   H_Y1_rain_above_1mm_C = sum(data_meteo %>%
                                filter(Year == year, Doy %in% period) %>%
                                select(`R>0.1`))
   
   
   # H_Y1_rain_above_1mm_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   H_Y1_rain_above_1mm_A1 = sum(data_meteo %>% 
                                 filter(Year == year, Doy %in% period) %>% 
                                 select(`R>0.1`))
   
   C = NA;   A1 = NA



# IAF (Indice Area Foliar) ------------------------------------------------------------

   ##  Iaf  Bud break year 1 ----
   # average values
  
   C = 10
   
   pheno_day = data_prd_rdd$DOY_BB[i]
   
   # BB_Y1_Iaf_C
   var = C
   period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
   BB_Y1_Iaf_C = mean((data_meteo %>% 
                        filter(Year == year, Doy %in% period))$Iaf)
   
   
   
   ##  Iaf  Flowering year 1 ----
   # average values
   
   B1 = 10; B2 = 20; B3 = 30; A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # FL_Y1_Iaf_B1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   FL_Y1_Iaf_B1 = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Iaf)
   
   # FL_Y1_Iaf_B2
   var = B2
   period = (pheno_day - var + 1): pheno_day
   FL_Y1_Iaf_B2 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Iaf)
   
   # FL_Y1_Iaf_B3
   var = B3
   period = (pheno_day - var + 1): pheno_day
   FL_Y1_Iaf_B3 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Iaf)
   
   
   # FL_Y1_Iaf_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_Iaf_A1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Iaf)
   
   B1 = NA; B2 = NA; B3 = NA; A1 = NA
   
   
   
   
   ##  Iaf  Harvesting year 1 ----
   # average values
   
   C = 10
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # H_Y1_Iaf_C
   var = C
   period = (pheno_day - floor(var / 2) ):(pheno_day + floor(var / 2) )
   H_Y1_Iaf_C = mean((data_meteo %>% 
                         filter(Year == year, Doy %in% period))$Iaf)
   
   C = NA
   
   
   ##  Iaf  Flowering year 0 ----
   # average values
   
   B1 = 10; B2 = 20; B3 = 30; A1 = 10
   
   pheno_day = data_prd_rdd$DOY_Fl[i-1]
   
   # FL_Y0_Iaf_B1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   FL_Y1_Iaf_B1 = mean((data_meteo %>% 
                         filter(Year == year - 1, Doy %in% period))$Iaf)
   
   # FL_Y0_Iaf_B2
   var = B2
   period = (pheno_day - var + 1): pheno_day
   FL_Y1_Iaf_B2 = mean((data_meteo %>% 
                          filter(Year == year - 1, Doy %in% period))$Iaf)
   
   # FL_Y0_Iaf_B3
   var = B3
   period = (pheno_day - var + 1): pheno_day
   FL_Y1_Iaf_B3 = mean((data_meteo %>% 
                          filter(Year == year - 1, Doy %in% period))$Iaf)
   
   
   # FL_Y0_Iaf_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_Iaf_A1 = mean((data_meteo %>% 
                          filter(Year == year - 1, Doy %in% period))$Iaf)
   
   
   B1 = NA; B2 = NA; B3 = NA; A1 = NA

# Available soil water ----------------------------------------------------------------

   
   ##  sw_a  Flowering year 1 ----
   # average values
   
   A1 =15 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # FL_Y1_swa_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_swa_A1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Sm)
   
   
   # FL_Y1_swa_A2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_swa_A2 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Sm)
  
   A1 =NA ; A2 = NA
   
   
   
   
   ##  sw_a  Harvesting year 1 ----
   # average values
   
   B1 =20 ; A1 = 20
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # H_Y1_swa_B1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   H_Y1_swa_B1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Sm)
   
   
   # H_Y1_swa_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   H_Y1_swa_A1 = mean((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$Sm)
   B1 =NA ; A1 = NA
   
   
   
   ##  sw_a  Harvesting year 0 ----
   # average values
   
   A1 =15 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Hv[i-1]
   
   # H_Y0_swa_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   H_Y0_swa_A1 = mean((data_meteo %>% 
                         filter(Year == year - 1, Doy %in% period))$Sm)
   
   
   # H_Y0_swa_A2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   H_Y0_swa_A2 = mean((data_meteo %>% 
                         filter(Year == year - 1, Doy %in% period))$Sm)
   
   
   
   
   
   ##  sw_c <1.5 x Wp Flowering year 1 ----
   
   # S_c  e <1.5 x Wp, A1, 10; 
   # refere ao nº de ocorrência nos 10 dias após em que em a 
   # água no solo foi 1,5 x menor que Wp (wilting point)
   
   # Os valores de wilting point (Wp) e field capacity (Fc) dependem da região 
   # No Douro: Wp é de 402 e o Fc é de 142,8
   # Nos verdes: Wp é de 338 mm e o Fc é de 77 mm
   
   
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # FL_Y1_swc_Sm1.5Wp_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_swc_Sm1.5Wp_A1 = sum((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   # FL_Y1_swc_Sm1.5Wp_A2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_swc_Sm1.5Wp_A2 = sum((data_meteo %>% 
                          filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
  
   A1 =NA ; A2 = NA
   
   
   ##  sw_c <1.5 x Wp   Start of maturity year 1 ----
  
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_sM[i]
   
   # sM_Y1_swc_Sm1.5Wp_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   sM_Y1_swc_Sm1.5Wp_A1 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   # sM_Y1_swc_Sm1.5Wp_A2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   sM_Y1_swc_Sm1.5Wp_A2 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   A1 =NA ; A2 = NA
   
  
   ##  sw_c <1.5 x Wp   Harvesting year 1 ----
   
   B1 =20 ; A1 = 20
   
   pheno_day = data_prd_rdd$DOY_Hv[i]
   
   # H_Y1_swc_Sm1.5Wp_B1
   var = B1
   period = (pheno_day - var + 1): pheno_day
   H_Y1_swc_Sm1.5Wp_B1 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   # H_Y1_swc_Sm1.5Wp_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   H_Y1_swc_Sm1.5Wp_A1 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm<1.5Wp`)
   
   
   B1 =NA ; A1 = NA
   
   
   
   ##  sw_c <1.5 x Wp   Flowering year 0 ----
   
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i-1]
   
   # FL_Y0_swc_Sm1.5Wp_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y0_swc_Sm1.5Wp_A1 = sum((data_meteo %>% 
                                 filter(Year == year -1, Doy %in% period))$`Sm<1.5Wp`)
   
   
   # FL_Y0_swc_Sm1.5Wp_A2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   FL_Y0_swc_Sm1.5Wp_A2 = sum((data_meteo %>% 
                                 filter(Year == year -1, Doy %in% period))$`Sm<1.5Wp`)
   
   A1 =NA ; A2 = NA
   
   
   
   ##  sw_c >0.9 x Fc  Flowering year 1 ----

   # Douro:  Fc é de 142,8
   # Verdes: Fc é de 77 mm
   
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i]
   
   # FL_Y1_swc_Sm_0.9Fc_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_swc_Sm_0.9Fc_A1 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm>0.9Fc`)
   
   
   # FL_Y1_swc_Sm_0.9Fc_A2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   FL_Y1_swc_Sm_0.9Fc_A2 = sum((data_meteo %>% 
                                 filter(Year == year, Doy %in% period))$`Sm>0.9Fc`)
   
   A1 =NA ; A2 = NA
   
   
   
   ##  sw_c >0.9 x Fc  Flowering year 0 ----
   
   A1 =10 ; A2 = 20
   
   pheno_day = data_prd_rdd$DOY_Fl[i-1]
   
   # FL_Y0_swc_Sm_0.9Fc_A1
   var = A1
   period = (pheno_day + var - 1): pheno_day
   FL_Y0_swc_Sm_0.9Fc_A1 = sum((data_meteo %>% 
                                  filter(Year == year -1, Doy %in% period))$`Sm>0.9Fc`)
   
   
   # FL_Y0_swc_Sm_0.9Fc_A2
   var = A2
   period = (pheno_day + var - 1): pheno_day
   FL_Y0_swc_Sm_0.9Fc_A2 = sum((data_meteo %>% 
                                  filter(Year == year -1, Doy %in% period))$`Sm>0.9Fc`)
   
   A1 =NA ; A2 = NA
   
   
   
   

# add all values to the data frame for the year ---------------------------------------

  
  
  
  my_df <- rbind(my_df,
                 data.frame(
                   Year = year,
                   Wine_mhl = Wine_mhl,
                   Fl_Y1_Tm_C = Fl_Y1_Tm_C,
                   Fl_Y1_Tm_B1 = Fl_Y1_Tm_B1,
                   Fl_Y1_Tm_A1 = Fl_Y1_Tm_A1,
                   Fl_Y0_Tm_B1 = Fl_Y0_Tm_B1, 
                   Fl_Y0_Tm_A1 = Fl_Y0_Tm_A1,
                   sM_Y1_Tm_C = sM_Y1_Tm_C,
                   sM_Y1_Tm_A1 = sM_Y1_Tm_A1,
                   sM_Y0_Tm_B1 = sM_Y0_Tm_B1,
                   H_Y1_Tm_C = H_Y1_Tm_C,
                   H_Y1_Tm_B1 = H_Y1_Tm_B1,
                   H_Y0_Tm_A1 = H_Y0_Tm_A1,
                   Fl_Y1_Tmed_less_15_C = Fl_Y1_Tmed_less_15_C,
                   Fl_Y1_Tmed_less_15_B1 = Fl_Y1_Tmed_less_15_B1,
                   BB_Y1_Tmmin_less_0_C = BB_Y1_Tmmin_less_0_C,
                   FL_Y1_Tmmin_less_0_A1 = FL_Y1_Tmmin_less_0_A1,
                   FL_Y1_Tmmax_above_35_A1 = FL_Y1_Tmmax_above_35_A1,
                   FL_Y1_Tmmax_above_35_A2 = FL_Y1_Tmmax_above_35_A2,
                   sM_Y1_Tmmax_above_35_B1 = sM_Y1_Tmmax_above_35_B1,
                   sM_Y1_Tmmax_above_35_A1 = sM_Y1_Tmmax_above_35_A1,
                   FL_Y1_rs_A1 = FL_Y1_rs_A1,
                   H_Y1_rs_B1 = H_Y1_rs_B1,
                   H_Y1_rs_A1 = H_Y1_rs_A1,
                   FL_Y1_rain_above_1mm_C = FL_Y1_rain_above_1mm_C,
                   FL_Y1_rain_above_1mm_A1 = FL_Y1_rain_above_1mm_A1,
                   sM_Y1_rain_above_1mm_B1 = sM_Y1_rain_above_1mm_B1,
                   sM_Y1_rain_above_1mm_A1 = sM_Y1_rain_above_1mm_B1,
                   H_Y1_rain_above_1mm_C = H_Y1_rain_above_1mm_C,
                   H_Y1_rain_above_1mm_A1 = H_Y1_rain_above_1mm_A1,
                   BB_Y1_Iaf_C = BB_Y1_Iaf_C,
                   FL_Y1_Iaf_B1 = FL_Y1_Iaf_B1,
                   FL_Y1_Iaf_B2 = FL_Y1_Iaf_B2,
                   FL_Y1_Iaf_B3 = FL_Y1_Iaf_B3,
                   FL_Y1_Iaf_A1 = FL_Y1_Iaf_A1,
                   H_Y1_Iaf_C = H_Y1_Iaf_C,
                   FL_Y1_swa_A1 = FL_Y1_swa_A1,
                   FL_Y1_swa_A2 = FL_Y1_swa_A2,
                   H_Y0_swa_A1 = H_Y0_swa_A1,
                   H_Y0_swa_A2 = H_Y0_swa_A2,
                   FL_Y1_swc_Sm1.5Wp_A1 = FL_Y1_swc_Sm1.5Wp_A1,
                   FL_Y1_swc_Sm1.5Wp_A2 = FL_Y1_swc_Sm1.5Wp_A2,
                   H_Y1_swc_Sm1.5Wp_B1 = H_Y1_swc_Sm1.5Wp_B1,
                   H_Y1_swc_Sm1.5Wp_A1 = H_Y1_swc_Sm1.5Wp_A1,
                   FL_Y0_swc_Sm1.5Wp_A1 = FL_Y0_swc_Sm1.5Wp_A1,
                   FL_Y0_swc_Sm1.5Wp_A2 = FL_Y0_swc_Sm1.5Wp_A2,
                   FL_Y1_swc_Sm_0.9Fc_A1 = FL_Y1_swc_Sm_0.9Fc_A1,
                   FL_Y1_swc_Sm_0.9Fc_A2 = FL_Y1_swc_Sm_0.9Fc_A2,
                   FL_Y0_swc_Sm_0.9Fc_A1 = FL_Y0_swc_Sm_0.9Fc_A1,
                   FL_Y0_swc_Sm_0.9Fc_A2 = FL_Y0_swc_Sm_0.9Fc_A2
              
                 ))
  
  
}


my_df %>% View()

write.csv(x = my_df,file = 'data/dis_rule_dataset_rdd.csv',
          row.names = FALSE)

getwd()

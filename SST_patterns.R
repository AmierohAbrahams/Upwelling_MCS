# SST_patterns
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Loading all of the data 
# 3: Determining the temperatures at the various distances from the coastline
# 4: Loading final data products
# 5: Climatology

# 1: Setup environment ----------------------------------------------------
#update.packages() 
library(tidyverse)
library(lubridate)
library(ggpubr)
library(zoo)
library(FNN)
library(circular)
library(fossil)
library(broom)
library(doMC); doMC::registerDoMC(cores = 4)
library(fasttime)
library(data.table)
library(heatwaveR)

# 2: Loading data ----------------------------------------------------

# load("Data/site_list_sub.Rdata")
load("Data/SACTN_US.RData")
#load("Data/site_pixels.RData") # 5 decimal places
load("Data/OISST.RData") # 2 decimal places
OISST <- BC_avhrr_only_v2_Document_Document 
rm(BC_avhrr_only_v2_Document_Document ); gc()
load("Data/CMC.RData") # 1decimal places
load("Data_coast_angle/MUR.RData")
load("Data_coast_angle/site_pixels.RData")
load("~/Documents/Upwelling_MCS/Data/BC2015_2016.RData")

# ### The temperature products
# load("~/Documents/Upwelling_MCS/Data/Final_G1SST.RData") #G1SST

OISST_prod <- BC2015_2016 %>%
  dplyr::select(lon, lat) %>%
  unique() %>%
  mutate(product = "OISST")

CMC_all_yrs <- rbind(CMC, CMC_2015_2016)

CMC_prod <- CMC %>%
  dplyr::select(lon, lat) %>%
  unique() %>%
  mutate(product = "CMC")

MUR_prod <- MUR_dat %>%
  dplyr::select(lon, lat) %>%
  unique() %>%
  mutate(product = "MUR")

G1SST_prod <- G1SST %>%
  dplyr::select(lon, lat) %>%
  unique() %>%
  mutate(product = "G1SST")

sat_data <- rbind(MUR_prod, OISST_prod) %>% 
  dplyr::select(product, lon, lat)

sat_pixels <- sat_data %>%
  dplyr::select(product, lon, lat) %>%
  unique()

match_func <- function(df){
  df <- df %>%
    dplyr::rename(lon_site = lon, lat_site = lat)
  OISST_index <- OISST_prod[as.vector(knnx.index(as.matrix(OISST_prod[,c("lon", "lat")]),
                                                 as.matrix(df[,c("lon_site", "lat_site")]), k = 1)),] %>%
    cbind(., df)
  MUR_index <- MUR_prod[as.vector(knnx.index(as.matrix(MUR_prod[,c("lon", "lat")]),
                                             as.matrix(df[,c("lon_site", "lat_site")]), k = 1)),] %>%
    cbind(., df)
  res <- rbind(OISST_index, MUR_index)
  return(res)
}


pixel_match <- site_pixels %>%
  group_by(site) %>%
  group_modify(~match_func(.x))


OISST_fill <- right_join(BC2015_2016, filter(pixel_match, product == "OISST"), by = c("lon", "lat"))
MUR_fill_2 <- right_join(MUR_dat, filter(pixel_match, product == "MUR"), by = c("lon", "lat"))
load("~/Documents/Upwelling_MCS/Data_coast_angle/MUR_fill_1.RData")
MUR_fill_1 <- rbind(MUR_fill_1, MUR_fill_2)
save(MUR_fill_1, file = "Data_coast_angle/MUR_fill_1.RData")

MUR_data_arr <- MUR_fill_1 %>% 
  arrange(Month_Yr) %>% 
  rename(date = Month_Yr)

MUR_data_arr$date <- as.Date(date)

rm(OISST, MUR); gc()

selected_sites <- c("Port Nolloth", "Lamberts Bay", "Sea Point", "Saldanha Bay")

OISST_fill_2015_2016 <- OISST_fill %>%
  filter(site %in% selected_sites)

CMC_fill_2015_2016 <- CMC_fill %>%
  filter(site %in% selected_sites)

save(OISST_fill_2015_2016, file = "Data_coast_angle/OISST_fill_2015_2016.RData")
save(CMC_fill_2015_2016, file = "Data_coast_angle/CMC_fill_2015_2016.RData")

# save(OISST_fill, file = "Data_coast_angle/OISST_fill.RData")
# save(CMC_fill, file = "Data_coast_angle/CMC_fill.RData")
# save(MUR_fill, file = "Data_coast_angle/MUR_fill.RData")
# save(G1SST_fill, file = "Data_coast_angle/G1SST_fill.RData")

# load("Data/UI_angle.RData")
load("Data_coast_angle/UI_angle.RData") # Created in script 'upwell_IDX.Rmd'
load("Data_coast_angle/G1SST_last.RData") # Created in Extracting folder : extract_tidync.R script"

CMC_yrs_complete <- rbind(CMC_fill,CMC_fill_2015_2016)
CMC_yrs_complete <- CMC_yrs_complete %>% 
  mutate(date =as.Date(date)) %>% 
  arrange(date)

OISST_yrs_complete <- rbind(OISST_fill, OISST_fill_2015_2016)
OISST_yrs_complete <- OISST_yrs_complete %>% 
  mutate(date =as.Date(date)) %>% 
  arrange(date)

upwelling <- UI_angle %>% 
  mutate(exceedance = ifelse(ui.saws >= 1, TRUE, FALSE),
         t = as.Date(t))

detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 1, coldSpells = T)$event 
  return(res)
}

ts2clm_custom <- function(df){
  res <- ts2clm(df, pctile = 25, climatologyPeriod = c("1991-09-01", " 2016-12-31")) #Length of MUR time series: Chnage according to length os SST product
  return(res)
}

# Calculate the upwelling event metrics
upwelling_detect_event <- function(df){
  upwell_base <- df %>%
    dplyr::rename(t = date) %>%
    group_by(site, product, heading, distance, lon, lat) %>%
    #group_modify(~ts2clm_custom(.x)) %>%
    left_join(upwelling, by = c("site", "t")) %>%
    filter(!is.na(exceedance)) %>%
    group_by(site, product, heading, distance, lon, lat) %>%
    group_modify(~detect_event_custom(.x))
}

# G1SST_finally <- G1SST_finally %>% 
#   arrange(date)

G1SST_last <- G1SST_last %>% 
  arrange(date)

G1SST_upwell_base <- upwelling_detect_event(df = G1SST_last)
#save(G1SST_upwell_base, file = "Data_coast_angle/G1SST_upwell_base.RData")
OISST_upwell_base <- upwelling_detect_event(df = OISST_fill)
#save(OISST_upwell_base, file = "Data_coast_angle/OISST_upwell_base.RData")
CMC_upwell_base <- upwelling_detect_event(df = CMC_fill)
#save(CMC_upwell_base, file = "Data_coast_angle/CMC_upwell_base.RData")
MUR_upwell_base <- upwelling_detect_event(df = MUR_fill)
#save(MUR_upwell_base, file = "Data_coast_angle/MUR_upwell_base.RData")

# 2015 - 2016
OISST_2015_upwell_base <- upwelling_detect_event(df = OISST_fill_2015_2016)
# save(OISST_2015_upwell_base, file = "Data_coast_angle/OISST_2015_upwell_base.RData")
CMC_2015_upwell_base <- upwelling_detect_event(df = CMC_fill_2015_2016)
# save(CMC_2015_upwell_base, file = "Data_coast_angle/CMC_2015_upwell_base.RData")


# Here we remove the site Hout Bay so that we have a long time series. The length of Hout Bay time series ends in 200. Many sites change from here
SACTN_US <- SACTN_US %>% 
  filter(site %in% selected_sites)

load("Data/site_list_v4.2.RData")

SACTN_upwell_base <- SACTN_US %>%
 left_join(site_list[,c(4, 5, 6)], by = "index")%>%
  dplyr::rename(t = date) %>%
  group_by(site, lon, lat) %>%
  group_modify(~ts2clm_custom(.x)) %>%
  left_join(upwelling, by = c("site", "t")) %>%
  filter(!is.na(exceedance)) %>%
  group_by(site, lon, lat) %>%
  group_modify(~detect_event_custom(.x))

save(SACTN_upwell_base, file = "Data/SACTN_upwell_base.RData")

# 5: Calculating the climatology ----------------------------------------------------

detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 1, coldSpells = T)$climatology 
  return(res)
}

ts2clm_custom <- function(df){
  res <- ts2clm(df, pctile = 25, climatologyPeriod = c("1982-01-01", "2005-10-19 ")) #Length of MUR time series: Chnage according to length os SST product
  return(res)
}

CMC_clims <- rbind(CMC_fill,CMC_fill_2015_2016)
OISST_clims <- rbind(OISST_fill,OISST_fill_2015_2016)


G1SST_last <- G1SST_last %>% 
  arrange(date)

G1SST_upwell_clims <- upwelling_detect_event(df = G1SST_last)
G1SST_upwell_clims$distance <- as.numeric(G1SST_upwell_clims$distance)
#save(G1SST_upwell_clims, file = "Data_coast_angle/G1SST_upwell_clims.RData")
OISST_upwell_clims <- upwelling_detect_event(df = OISST_fill)
#save(OISST_upwell_clims, file = "Data_coast_angle/OISST_upwell_clims.RData")
CMC_upwell_clims <- upwelling_detect_event(df = CMC_fill)
# save(CMC_upwell_clims, file = "Data_coast_angle/CMC_upwell_clims.RData")
MUR_fill$distance <- as.numeric(MUR_fill$distance)
MUR_upwell_clims <- upwelling_detect_event(df = MUR_fill)
MUR_upwell_clims$distance <- as.numeric(MUR_upwell_clims$distance)
#save(MUR_upwell_clims, file = "Data_coast_angle/MUR_upwell_clims.RData")

#### With wind only as a filter

ts2clm_custom <- function(df){
  res <- ts2clm(df, pctile = 100, climatologyPeriod = c("2011-01-02", "2014-06-30")) #Length of MUR time series: Chnage according to length os SST product
  return(res)
}

G1SST_without_temp <- upwelling_detect_event(df = G1SST_last)
OISST_without_temp <- upwelling_detect_event(df = OISST_fill)
MUR_without_temp <- upwelling_detect_event(df = MUR_fill)
CMC_without_temp <- upwelling_detect_event(df = CMC_fill)

OISST_without_temp2<- upwelling_detect_event(df = OISST_fill)
# save(OISST_2015_upwell_base, file = "Data_coast_angle/OISST_2015_upwell_base.RData")
CMC_2015_without_temp <- upwelling_detect_event(df = CMC_fill)
# save(CMC_2015_upwell_base, file = "Data_coast_angle/CMC_2015_upwell_base.RData")

OISST_wind_only <- rbind(OISST_without_temp,OISST_without_temp2)
OISST_tot <- rbind(OISST_2015_upwell_base, OISST_upwell_base)

Number_signals_withtemp <- SACTN_upwell_base %>% 
  group_by(site) %>% 
  summarise(y = n())

Number_signals <- SACTN_upwell_base_WITHOUT %>% 
  group_by(site) %>% 
  summarise(y = n())

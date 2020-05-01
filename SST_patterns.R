# SST_patterns
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Loading all of the data 
# 3: Determining the temperatures at the various distances from the coastline
# 4: Loading final data products

# 1: Setup environment ----------------------------------------------------
#update.packages() 
library(tidyverse)
library(lubridate)
library(ggpubr)
library(zoo)
library(FNN)
library(scales)
library(gridExtra)
library(circular)
library(fossil)
library(mapproj)
library(broom)
# library(ncdf4) # This was used to process NetCDF files in an earlier version
library(stringr)
library(doMC); doMC::registerDoMC(cores = 4)
library(fasttime)
library(xtable)
library(ncdf4) # library for processing netCDFs
library(data.table)
# library(plyr) # RWS: Never load the plyr package as it interferes with the tidyverse
library(heatwaveR)
library(grid)
library(ggthemes)

# 2: Loading data ----------------------------------------------------

load("Data/site_list_sub.Rdata")
load("Data/SACTN_US.RData")
#load("Data/site_pixels.RData") # 5 decimal places
load("Data/OISST.RData") # 2 decimal places
OISST <- BC_avhrr_only_v2_Document_Document 
rm(BC_avhrr_only_v2_Document_Document ); gc()
load("Data/CMC.RData") # 1decimal places
load("Data/MUR.RData")
load("Data_coast_angle/site_pixels.RData")

# ### The temperature products
# load("~/Documents/Upwelling_MCS/Data/Final_G1SST.RData") #G1SST

OISST_prod <- OISST %>%
  dplyr::select(lon, lat) %>%
  unique() %>%
  mutate(product = "OISST")

CMC_prod <- CMC %>%
  dplyr::select(lon, lat) %>%
  unique() %>%
  mutate(product = "CMC")

sat_data <- rbind(CMC_prod, OISST_prod) %>% 
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
  CMC_index <- CMC_prod[as.vector(knnx.index(as.matrix(CMC_prod[,c("lon", "lat")]),
                                             as.matrix(df[,c("lon_site", "lat_site")]), k = 1)),] %>%
    cbind(., df)
  res <- rbind(OISST_index, CMC_index)
  return(res)
}

pixel_match <- site_pixels %>%
  group_by(site) %>%
  group_modify(~match_func(.x))

OISST_fill <- right_join(OISST, filter(pixel_match, product == "OISST"), by = c("lon", "lat"))
CMC_fill <- right_join(CMC, filter(pixel_match, product == "CMC"), by = c("lon", "lat"))
rm(OISST, CMC); gc()

selected_sites <- c("Port Nolloth", "Lamberts Bay", "Sea Point", "Saldanha Bay")

OISST_fill <- OISST_fill %>%
  filter(site %in% selected_sites)

CMC_fill <- CMC_fill %>%
  filter(site %in% selected_sites)

# save(OISST_fill, file = "Data_coast_angle/OISST_fill.RData")
# save(CMC_fill, file = "Data_coast_angle/CMC_fill.RData")

# load("Data/UI_angle.RData")
load("Data_coast_angle/UI_angle.RData")

upwelling <- UI_angle %>%
  mutate(date = as.Date(date)) %>% 
  dplyr::rename(temp = ui.saws) %>%
  group_by(site) %>%
  nest() %>% # apply the following functions to all of the variables in te dataset
  mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = c("1992-07-09", "2017-12-21")), # creating a column of climatologies. Column will be named clim
                 exceed = purrr::map(clim, exceedance, minDuration = 1, threshold = 1)) %>%  #Upwelling cannot be descrbed as an event. Upwelling can last for a few hours. Given that we have daily data, upwelling events minimum duration here will be 1day
  select(-data, -clim) %>%
  unnest() %>%
  filter(row_number() %% 2 == 1) %>%
  unnest() %>% # creates a column for each variables
  dplyr::rename(ui.saws = temp) %>% # rename upwelling index vale to temp so that it could work with the function
  select(site, t, ui.saws, exceedance)

detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 1, coldSpells = T)$event # 1 or 3?
  return(res)
}
# 
ts2clm_custom <- function(df){
  res <- ts2clm(df, pctile = 25, climatologyPeriod = c("2010-06-23", "2014-06-30"))
  return(res)
}
# 
# Calculate the upwelling event metrics
upwelling_detect_event <- function(df){
  upwell_base <- df %>%
    dplyr::rename(t = date) %>%
    group_by(site, product, heading, distance, lon, lat) %>%
    group_modify(~ts2clm_custom(.x)) %>%
    left_join(upwelling, by = c("site", "t")) %>%
    filter(!is.na(exceedance)) %>%
    group_by(site, product, heading, distance, lon, lat) %>%
    group_modify(~detect_event_custom(.x))
}
# 
OISST_upwell_base <- upwelling_detect_event(df = OISST_fill)
#save(OISST_upwell_base, file = "Data/OISST_upwell_base.RData")
save(OISST_upwell_base, file = "Data_coast_angle/OISST_upwell_base.RData")
CMC_upwell_base <- upwelling_detect_event(df = CMC_fill)
save(CMC_upwell_base, file = "Data/CMC_upwell_base.RData")
save(CMC_upwell_base, file = "Data_coast_angle/CMC_upwell_base.RData")
MUR_upwell_base <- upwelling_detect_event(df = Final)
save(MUR_upwell_base, file = "Data_coast_angle/MUR_upwell_base.RData")

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

# 4: Loading the final data  ----------------------------------------------------

load("Data/OISST_upwell_base.RData")
load("Data/CMC_upwell_base.RData")
load("Data/SACTN_upwell_base.RData")
load("Data/MUR_upwell_base.RData")
load("Data/G1SST_upwell_base.RData")

# Removing the distance of 20 and 40kms
library(dplyr)
removing_distance_func <- function(df){
  removing_dist_func<- df %>% 
    filter(!distance %in% c(20000,40000))
}

OISST_final <- removing_distance_func(df = OISST_upwell_base)
G1SST_final <- removing_distance_func(df = G1SST_upwell_base)
MUR_final <- removing_distance_func(df = MUR_upwell_base)
CMC_final <- removing_distance_func(df = CMC_upwell_base)

# save(OISST_final, file = "Data/OISST_final.RData")
# save(G1SST_final, file = "Data/G1SST_final.RData")
# save(MUR_final, file = "Data/MUR_final.RData")
# save(CMC_final, file = "Data/CMC_final.RData")



















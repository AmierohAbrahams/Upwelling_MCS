# Loading libraries
library(tidyverse)
library(lubridate)
library(heatwaveR)


load("Data_coast_angle/UI_angle.RData") 
load("G1SST.RData") 

upwelling <- UI_angle %>% 
  mutate(exceedance = ifelse(ui.saws >= 1, TRUE, FALSE),
         t = as.Date(t))

detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 1, coldSpells = T)$event 
  return(res)
}

ts2clm_custom <- function(df){
  res <- ts2clm(df, pctile = 25, climatologyPeriod = c("1982-01-01", "2017-12-31")) #Length of MUR time series: Chnage according to length os SST product
  return(res)
}

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


G1SST_upwell_base <- upwelling_detect_event(df = G1SST)

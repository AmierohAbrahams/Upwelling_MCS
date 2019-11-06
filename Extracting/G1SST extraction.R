library(tidyverse)
# library(stringr) # RWS: 'stringr' is loaded as part of 'tidyverse'
# library(reshape2) # RWS: Better not to call this ecplicitly as it is pre-tidyverse and may ause issues
library(ncdf4) # library for processing netCDFs
# library(plyr) # RWS: Please never load plyr directly. You keep doing this...
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 7)
library(FNN)
library(heatwaveR)
# devtools::install_github("robwschlegel/heatwaveR")
# # rm(G1SST)
# ncDir <- "/home/amieroh/Documents/Data/Datasets/G1SST/daily/Extract 90.2"
# csvDir <- "/media/amieroh/Seagate EXpansion/Extract"
# 
# # #          1         2         3         4
# # # 12345678901234567890123456789012345678901
# # # 20020601-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc
# 
# ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
# ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
# ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
# strtDate <- str_sub(ncFirst, start = 1, end = 8)
# endDate <- str_sub(ncLast, start = 1, end = 8)
# 
# # # ncFile <- '/home/amieroh/Documents/Data/Datasets/MUR/daily/20020606-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc'
# 
# ncFun <- function(ncFile = ncFile, csvDir = csvDir) {
#   nc <- nc_open(ncFile)
#   pathLen <- nchar(paste0(ncDir, "/")) + 1
#   fNameStem <-
#     substr(basename(ncFile), 10, 58)
#   fDate <- substr(basename(ncFile), 1, 8)
#   sst <- ncvar_get(nc, varid = "analysed_sst") %>%
#     round(4)
#   dimnames(sst) <- list(lon = nc$dim$lon$vals,
#                         lat = nc$dim$lat$vals)
#   nc_close(nc)
#   sst <- as_tibble(melt(sst, value.name = "temp"))
#   sst$t <- ymd(fDate)
#   na.omit(sst)
#   fwrite(sst,
#          file = paste0(csvDir, "/", fNameStem, "-", strtDate, "-", endDate, ".csv"),
#          append = TRUE, col.names = FALSE)
#   rm(sst)
# }
# 
# llply(ncList, ncFun, csvDir = csvDir, .parallel = TRUE)
# 
# G1SST <- read_csv("/media/amieroh/Seagate EXpansion/Extract/JPL_OUROCEAN-L4UHfnd-GLOB-v01-fv01_0-G1SST_subset-20140323-20140331.csv")
# names(G1SST)<-c("lon","lat", "temp", "date")
# G1SST <- G1SST %>%
#   mutate(temp = temp - 273.15)
# 
# save(G1SST, file = "/media/amieroh/Seagate EXpansion/G1SST/G1SST_90.2.RData")
# #####################
# # rm(G1SST)
# # rm(Final)
# # rm(G1SST_1)
# # rm(G1SST_fill)
# # rm(G1SST_2)
# # rm(G1SST_prod)
# # rm(G1SST_prod2)
# # rm(G1SST2_fill)
# # rm(sat_data)
# # rm(sat_pixels)
# load("/media/amieroh/Seagate EXpansion/G1SST/G1SST_1.RData")
# G1SST_1 <- G1SST %>%
#   na.omit()
# load("/media/amieroh/Seagate EXpansion/G1SST/G1SST_90.2.RData")
# G1SST_2 <- G1SST %>%
#   na.omit()
# # 
# # 
# rm(G1SST); gc()
# 
# G1SST_prod <- G1SST_1 %>%
#   select(lon, lat) %>%
#   unique() %>%
#   mutate(product = "G1SST")
# 
# 
# G1SST_prod2 <- G1SST_2 %>%
#   select(lon, lat) %>%
#   unique() %>%
#   mutate(product = "G1SST")
# 
# # 
# sat_data <- rbind(G1SST_prod, G1SST_prod2) %>%
#   #   #rbind(., CMC) %>%
#   select(product, lon, lat)
# 
# sat_pixels <- sat_data %>%
#   select(product, lon, lat) %>%
#   unique() %>%
#   na.omit()
# 
# match_func <- function(df){
#   df <- df %>%
#     dplyr::rename(lon_site = lon, lat_site = lat)
#   G1SST_index <- G1SST_prod[as.vector(knnx.index(as.matrix(G1SST_prod[,c("lon", "lat")]),
#                                              as.matrix(df[,c("lon_site", "lat_site")]), k = 1)),] %>%
#     cbind(., df)
#   G1SST_index2 <- G1SST_prod2[as.vector(knnx.index(as.matrix(G1SST_prod2[,c("lon", "lat")]),
#                                                as.matrix(df[,c("lon_site", "lat_site")]), k = 1)),] %>%
#     cbind(., df)
#   res <- rbind(G1SST_index, G1SST_index2)
#   return(res)
# }
# 
# #load("Data/site_pixels.RData")
# pixel_match <- site_pixels %>%
#   group_by(site) %>%
#   group_modify(~match_func(.x))
# 
# G1SST_fill <- right_join(G1SST_1, filter(pixel_match, product == "G1SST"), by = c("lon", "lat"))
# G1SST2_fill <- right_join(G1SST_2, filter(pixel_match, product == "G1SST"), by = c("lon", "lat"))
# 
# # rm(G1SST); gc()
# 
# selected_sites <- c("Port Nolloth", "Lamberts Bay", "Sea Point", "Saldanha Bay")
# 
# G1SST_fill <- G1SST_fill %>%
#   filter(site %in% selected_sites)
# 
# G1SST2_fill <- G1SST2_fill %>%
#   filter(site %in% selected_sites)
# 
# Final <- rbind(G1SST_fill, G1SST2_fill)
# 
# # Final_G1SST <- Final_G1SST %>%# we get the df that is positive
# #   filter(temp > 0)
# 
# Final_G1SST <- rbind(Final, Final_G1SST)
# 
# save(Final_G1SST, file = "Data/Final_G1SST.RData")
# rm(Final_G1SST)


#  Detect upwell MCS in G1SST data ----------------------------------------

load("Data/Final_G1SST.RData")
load("Data/upwelling.RData")

detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 3, coldSpells = T)$event
  return(res)
}
# 
ts2clm_custom <- function(df){
  # The climatology base period used here is up for debate...
  # The choice of the 25th percentile threshold also needs to be justified and sensitivty tested
  res <- ts2clm(df, pctile = 25, climatologyPeriod = c("2010-07-14", "2014-03-09"))
  return(res)
}

# # Calculate the upwelling event metrics
upwelling_detect_event <- function(df){
  upwell_base <- df %>%
    dplyr::rename(t = date) %>%
    group_by(site, product, heading, distance, lon, lat) %>%
    filter(min(t) <= "2010-07-14") %>%  # RWS: Remove pixels that are too short
    group_modify(~ts2clm_custom(.x)) %>%
    left_join(upwelling, by = c("site", "t")) %>%
    filter(!is.na(exceedance)) %>%
    group_by(site, product, heading, distance, lon, lat) %>%
    group_modify(~detect_event_custom(.x))
}
# 
G1SST_upwell_base <- upwelling_detect_event(df = Final_G1SST)


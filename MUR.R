library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 7)
library(FNN)
library(heatwaveR)

ncDir <- "/home/amieroh/Documents/Data/Datasets/MUR/daily/Extract 296"
csvDir <- "/media/amieroh/Seagate Expansion/Extract"

# #          1         2         3         4
# # 12345678901234567890123456789012345678901
# # 20020601-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc

  ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
  ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
  ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
  strtDate <- str_sub(ncFirst, start = 1, end = 8)
  endDate <- str_sub(ncLast, start = 1, end = 8)

# # ncFile <- '/home/amieroh/Documents/Data/Datasets/MUR/daily/20020606-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc'

  ncFun <- function(ncFile = ncFile, csvDir = csvDir) {
    nc <- nc_open(ncFile)
    pathLen <- nchar(paste0(ncDir, "/")) + 1
fNameStem <-
  substr(basename(ncFile), 10, 38)
    fDate <- substr(basename(ncFile), 1, 8)
    sst <- ncvar_get(nc, varid = "analysed_sst") %>%
      round(4)
    dimnames(sst) <- list(lon = nc$dim$lon$vals,
                          lat = nc$dim$lat$vals)
    nc_close(nc)
    sst <- as_tibble(melt(sst, value.name = "temp"))
    sst$t <- ymd(fDate)
    na.omit(sst)
    fwrite(sst,
           file = paste0(csvDir, "/", fNameStem, "-", strtDate, "-", endDate, ".csv"),
           append = TRUE, col.names = FALSE)
    rm(sst)
  }

llply(ncList, ncFun, csvDir = csvDir, .parallel = TRUE)

MUR <- read_csv("/media/amieroh/Seagate Expansion/Extract/JPL-L4UHfnd-GLOB-v01-fv04-MUR-20140701-20140715.csv")
names(MUR)<-c("lon","lat", "temp", "date")
MUR <- MUR %>%
  mutate(temp = temp - 273.15)

save(MUR, file = "/media/amieroh/Seagate Expansion/MUR/MUR_296.RData")

rm(MUR)
rm(MUR_1)
rm(MUR_2)
rm(MUR_prod)
rm(MUR_prod2)
rm(MUR_fill)
rm(MUR2_fill)
rm(MUR_combined)
rm(MUR_combined_2)
load("/media/amieroh/Seagate Expansion/MUR/MUR_294.RData")
MUR_1 <- MUR %>% 
  na.omit()
load("/media/amieroh/Seagate Expansion/MUR/MUR_295.RData")
MUR_2 <- MUR %>% 
  na.omit()


# RWS: Both of these files load with the same onbject name
# So the way you had it you were loading one over the other
# And then assigning different names to the same object
load("Data/MUR_1.RData")
MUR_1 <- na.omit(MUR) # Half of this file is deadspace, why?

load("Data/MUR_2.RData")
MUR_2 <- na.omit(MUR)

# Free up RAM
rm(MUR); gc()

MUR_prod <- MUR_1 %>%
  select(lon, lat) %>% 
  unique() %>%
  mutate(product = "MUR")
gc()

MUR_prod2 <- MUR_2 %>%
  select(lon, lat) %>%
  unique() %>%
  mutate(product = "MUR")
gc()

sat_data <- rbind(MUR_prod, MUR_prod2) %>% 
  #   #rbind(., CMC) %>% 
  select(product, lon, lat)

sat_pixels <- sat_data %>%
  select(product, lon, lat) %>%
  unique() %>% 
  na.omit()

match_func <- function(df){
  df <- df %>%
    dplyr::rename(lon_site = lon, lat_site = lat)
  MUR_index <- MUR_prod[as.vector(knnx.index(as.matrix(MUR_prod[,c("lon", "lat")]),
                                                 as.matrix(df[,c("lon_site", "lat_site")]), k = 1)),] %>%
    cbind(., df)
  MUR_index2 <- MUR_prod2[as.vector(knnx.index(as.matrix(MUR_prod2[,c("lon", "lat")]),
                                             as.matrix(df[,c("lon_site", "lat_site")]), k = 1)),] %>%
    cbind(., df)
  res <- rbind(MUR_index, MUR_index2)
  return(res)
}

#load("Data/site_pixels.RData")
pixel_match <- site_pixels %>%
  group_by(site) %>%
  group_modify(~match_func(.x))

MUR_fill <- right_join(MUR_1, filter(pixel_match, product == "MUR"), by = c("lon", "lat"))
MUR2_fill <- right_join(MUR_2, filter(pixel_match, product == "MUR"), by = c("lon", "lat"))

rm(MUR); gc()

selected_sites <- c("Port Nolloth", "Lamberts Bay", "Sea Point", "Saldanha Bay")

MUR_fill <- MUR_fill %>%
  filter(site %in% selected_sites)

MUR2_fill <- MUR2_fill %>%
  filter(site %in% selected_sites)

MUR_combined_2 <- rbind(MUR_fill, MUR2_fill)

Final <- rbind(Final, MUR_combined_2)
save(Final, file = "Data/Final.RData")

load("Data/Final_RData")


detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 3, coldSpells = T)$event
  return(res)
}
# 
ts2clm_custom <- function(df){
  # The climatology base period used here is up for debate...
  # The choice of the 25th percentile threshold also needs to be justified and sensitivty tested
  res <- ts2clm(df, pctile = 25, climatologyPeriod = c("2002-06-01", "2012-12-15"))
  return(res)
}
load("Data/upwelling.RData")

# # Calculate the upwelling event metrics
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
MUR_upwell_base <- upwelling_detect_event(df = Final)
# save(MUR_upwell_base, file = "Data/MUR_upwell_base.RData")

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
library(stringr)
library(doMC); doMC::registerDoMC(cores = 4)
library(fasttime)
library(xtable)
library(ncdf4) # library for processing netCDFs
library(data.table)
library(heatwaveR)

# ncDir <- "/home/amieroh/Documents/Data/Datasets/MUR/daily/Extract 9"
# csvDir <- "/home/amieroh/Documents/Data/Datasets/MUR/daily/Extract 9"
# 
# # #          1         2         3         4
# # # 12345678901234567890123456789012345678901
# # # 20020601-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc
# 
#   ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
#   ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   strtDate <- str_sub(ncFirst, start = 1, end = 8)
#   endDate <- str_sub(ncLast, start = 1, end = 8)
# 
# # # ncFile <- '/home/amieroh/Documents/Data/Datasets/MUR/daily/20020606-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc'
# 
#   ncFun <- function(ncFile = ncFile, csvDir = csvDir) {
#     nc <- nc_open(ncFile)
#     pathLen <- nchar(paste0(ncDir, "/")) + 1
# fNameStem <-
#   substr(basename(ncFile), 10, 38)
#     fDate <- substr(basename(ncFile), 1, 8)
#     sst <- ncvar_get(nc, varid = "analysed_sst") %>%
#       round(4)
#     dimnames(sst) <- list(lon = nc$dim$lon$vals,
#                           lat = nc$dim$lat$vals)
#     nc_close(nc)
#     sst <- as_tibble(melt(sst, value.name = "temp"))
#     sst$t <- ymd(fDate)
#     na.omit(sst)
#     fwrite(sst,
#            file = paste0(csvDir, "/", fNameStem, "-", strtDate, "-", endDate, ".csv"),
#            append = TRUE, col.names = FALSE)
#     rm(sst)
#   }
# 
# llply(ncList, ncFun, csvDir = csvDir, .parallel = TRUE)
# 
# MUR <- read_csv("/home/amieroh/Documents/Data/Datasets/MUR/daily/Extract 9/JPL-L4UHfnd-GLOB-v01-fv04-MUR-20021016-20021031.csv")
# names(MUR)<-c("lon","lat", "temp", "date")
# MUR <- MUR %>%
#   mutate(temp = temp - 273.15)
# 
# save(MUR, file = "/home/amieroh/Documents/Data/Datasets/MUR/daily/Extract 9/MUR_9.RData")
# rm(MUR)

load("Data/MUR_1.RData")
load("Data/MUR_2.RData")
MUR_1 <- MUR
MUR_2 <- MUR

MUR_prod <- MUR_1 %>%
  select(lon, lat) %>%
  unique() %>%
  mutate(product = "MUR")
MUR_prod2 <- MUR_2 %>%
  select(lon, lat) %>%
  unique() %>%
  mutate(product = "MUR")

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


pixel_match <- site_pixels %>%
  group_by(site) %>%
  group_modify(~match_func(.x))

MUR_fill <- right_join(MUR1, filter(pixel_match, product == "MUR"), by = c("lon", "lat"))
MUR2_fill <- right_join(MUR2, filter(pixel_match, product == "MUR"), by = c("lon", "lat"))

rm(MUR); gc()

selected_sites <- c("Port Nolloth", "Lamberts Bay", "Sea Point", "Saldanha Bay")
# 
MUR_fill <- MUR_fill %>%
  filter(site %in% selected_sites)

MUR2_fill <- MUR2_fill %>%
  filter(site %in% selected_sites)









# Extracting the data
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Find the different distances from the coastline
# 3: Extracting the MUR data
# 4: Extracting the CMC data
# 5: Extracting the OISST data
# 6: Extracting the G1SST data

# 1: Setup environment ----------------------------------------------------
library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
# library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 7)

# 2: Coastal distances ----------------------------------------------------

load("Data/site_list_sub.Rdata")
# xtable(site_list_sub, auto = TRUE)
west <- site_list_sub
west$coast <- "west" # Chnages wc to west

load("Data/africa_coast.RData")

# Downloading the bathy data from NOAA
# Download mid-res bathymetry data
sa_lat <- c(-38, -24.5); sa_lon <- c(11.5, 35.5)
sa_bathy <- as.xyz(getNOAA.bathy(lon1 = sa_lon[1], lon2 = sa_lon[2], lat1 = sa_lat[1], lat2 = sa_lat[2], resolution = 4))
colnames(sa_bathy) <- c("lon", "lat", "depth")
sa_bathy <- sa_bathy[sa_bathy$depth <= 0,]
save(sa_bathy, file = "Data_P1/bathy/sa_bathy.RData")

# # Loading in the newly downloaded bathymetry data
load("Data/bathy/sa_bathy.RData")

# # This function takes one site (e.g. one set of lon/lats) and calculates a shore normal transect
shore.normal.transect <- function(site, width = 2){
  # Find the site on the coastline and it's nearest neighbour points
  coords <- data.frame(lon = site$lon, lat = site$lat)
  coords2 <- knnx.index(africa_coast[,1:2], as.matrix(coords), k = 1)
  coords3 <- data.frame(site = site$site, africa_coast[c(coords2-width, coords2+width),])
  coords3 <- coords3[2:1,1:3]
  # Define the shore normal transect bearing
  heading <- earth.bear(coords3[1,2], coords3[1,3], coords3[2,2], coords3[2,3]) + 90
  if(heading >= 360){
    heading <- heading-360
  } else {
    heading <- heading
  }
  heading2 <- data.frame(site = site$site, lon = site$lon, lat = site$lat, heading)
  return(heading2)
}

# Creating the transects
site_transects <- data.frame()
for(i in 1:length(west$site)){
 site <- west[i,]
 site_transect <- shore.normal.transect(site, 2)
 site_transects <- rbind(site_transects, site_transect)
}

# # Manually correcting Sea Point and Kommetjie
site_transects$heading[4:5] <- 290
# save(site_transects, file = "Data/site_transects.RData")
load("Data/site_transects.RData")

# # This function takes one site (e.g. one set of lon/lats) and calculates a shore norm./subal transect
# # It then extracts a lat/ lon point every X kilometres until reaching a specified isobath
#
transect.pixel <- function(site, distances){
  # Extract coordinates
  coords <- data.frame(lon = site$lon, lat = site$lat)
  # Find lon/ lats every X metres
  pixels <- data.frame()
  # deep <- 999
  # distance_multiplier <- 1
  # while(deep > isobath){
  for(i in 1:length(distances)){
    coords2 <- as.data.frame(destPoint(p = coords, b = site$heading, d = distances[i]))
    sitesIdx <- knnx.index(sa_bathy[,1:2], as.matrix(coords2), k = 1)
    bathy2 <- sa_bathy[sitesIdx,]
    bathy2 <- bathy2[complete.cases(bathy2$depth),]
    bathy3 <- data.frame(site = site$site, lon = bathy2$lon, lat = bathy2$lat,
                         heading = site$heading,
                         distance = distances[i])
    pixels <- rbind(pixels, bathy3)
    coords <- coords2
  }
  if(nrow(pixels) < 1){
    pixels <- data.frame(site, depth = NA)
  }else{
    pixels <- pixels
  }

  return(pixels)
}

# # Pixel points
site_pixels <- data.frame()
for(i in 1:length(west$site)){
  site <- site_transects[i,]
  site_pixel <- transect.pixel(site, c(00000, 25000, 50000)) # RWS: fixed error
  site_pixels <- rbind(site_pixels, site_pixel)
}

# Bounding box
  # Only one is made in order to know how large the the geom_point() squares should be made to match
bbox <- data.frame(xmin = destPoint(p = site_pixels[1,2:3], b = 270, d = 12500)[1],
                   xmax = destPoint(p = site_pixels[1,2:3], b = 90, d = 12500)[1],
                   ymin = destPoint(p = site_pixels[1,2:3], b = 180, d = 12500)[2],
                   ymax = destPoint(p = site_pixels[1,2:3], b = 0, d = 12500)[2])

# Determining the temperature at the various distances from the coast

# save(site_pixels, file = "Data/site_pixels.RData")
load("Data/site_pixels.RData")
save(site_pixels, file = "Data_coast_angle/site_pixels.RData")

# 2: EXtracting the MUR data  ----------------------------------------------------

 
# ncDir <- "/home/amieroh/Documents/Data/Datasets/MUR/daily"
# csvDir <- "/media/amieroh/Seagate Expansion Drive/Extracted G1SST/MUR_extracted"
# 
# #          1         2         3         4
# # 12345678901234567890123456789012345678901
# # 20020601-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc
# 
# read_nc <- function(ncDir = ncDir, csvDir = csvDir)
#   ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
#   ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   strtDate <- str_sub(ncFirst, start = 1, end = 8)
#   endDate <- str_sub(ncLast, start = 1, end = 8)
# 
# # ncFile <- '/home/amieroh/Documents/Data/Datasets/MUR/daily/20020606-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc'
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

# MUR <- read_csv("/home/amieroh/Documents/Data/Datasets/MUR/Extracted_MUR/BC-JPL-L4UHfnd-GLOB-v01-fv04-MUR-20020601-20140727.csv")
# names(MUR)<-c("lon","lat", "temp", "date")
# MUR <- MUR %>% 
#   mutate(temp = temp - 273.15)
# 
# JPL_L4UHfnd_GLOB_v01_fv04_MUR_20020601_20140727 <- read_csv("~/Documents/JPL-L4UHfnd-GLOB-v01-fv04-MUR-20020601-20140727.csv")
# 
# ######################################### SUBSET VIA REGION #################
# 
# # bbox <- data.frame(BC = c(-35, -25, 15, 20), # Benguela Current
# library(stringr)
# library(tidyverse)
# library(reshape2)
# library(ncdf4) # library for processing netCDFs
# library(plyr)
# library(lubridate)
# library(data.table)
# library(doMC); doMC::registerDoMC(cores = 7)
# ncDir <- "/home/amieroh/Documents/Data/Datasets/MUR/daily"
# csvDir <- "/home/amieroh/Documents/Data/Datasets/MUR/Extracted_MUR"
# #          1         2         3         4
# # 12345678901234567890123456789012345678901
# # 20020601-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc
# 
# # ncFile <- '/home/amieroh/Documents/Data/Datasets/MUR/daily/20020606-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc'
#   ncFun <- function(ncFile = ncFile, region = region, csvDir = csvDir) {
#     coords <- bbox[, region]
#     nc <- nc_open(ncFile)
#     pathLen <- nchar(paste0(ncDir, "/")) + 1
#     fNameStem <-
#       substr(basename(ncFile), 10, 38)
#     fDate <- substr(basename(ncFile), 1, 8)
#     LatIdx <- which(nc$dim$lat$vals > coords[1] & nc$dim$lat$vals < coords[2])
#     LonIdx <- which(nc$dim$lon$vals > coords[3] & nc$dim$lon$vals < coords[4])
#     sst <- ncvar_get(nc, varid = "analysed_sst") %>%
#       round(4)
#     dimnames(sst) <- list(lon = nc$dim$lon$vals,
#                           lat = nc$dim$lat$vals)
#     nc_close(nc)
#     sst <- as_tibble(melt(sst, value.name = "temp"))
#     sst$t <- ymd(fDate)
#     na.omit(sst)
#     fwrite(sst,
#            file = paste0(csvDir, "/", region, "-", fNameStem, "-", strtDate, "-", endDate, ".csv"),
#            append = TRUE, col.names = FALSE)
#     rm(sst)
#   }
#   
#   
#   ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
#   ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   strtDate <- str_sub(ncFirst, start = 1, end = 8)
#   endDate <- str_sub(ncLast, start = 1, end = 8)  
#   
# llply(ncList, ncFun, region = "BC", csvDir = csvDir, .parallel = TRUE)




# 4: Extracting CMC dataset ----------------------------------------------------

# ncDir <- "/home/amieroh/Documents/Data/Datasets/CMC/CMC_BC"
# csvDir <- "/home/amieroh/Documents/Data/Datasets/CMC/CMC_extracted"
# 
# #          1         2         3         4         5         6
# # 123456789012345678901234567890123456789012345678901234567890
# # 20100609-JPL_OUROCEAN-L4UHfnd-GLOB-v01-fv01_0-G1SST_subset.nc
# read_nc <- function(ncDir = ncDir, csvDir = csvDir) 
#   ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
#   ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   strtDate <- str_sub(ncFirst, start = 1, end = 8)
#   endDate <- str_sub(ncLast, start = 1, end = 8)
# 
# # ncFile <- '/home/amieroh/Documents/Data/Datasets/CMC/CMC_BC/20100609-JPL_OUROCEAN-L4UHfnd-GLOB-v01-fv01_0-G1SST_subset.nc'
# 
#   ncFun <- function(ncFile = ncFile, csvDir = csvDir) {
#     nc <- nc_open(ncFile)
#     pathLen <- nchar(paste0(ncDir, "/")) + 1
#     fNameStem <-
#       substr(basename(ncFile), 10, 58)
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
# 
# CMC <- read_csv("/home/amieroh/Documents/Data/Datasets/CMC/CMC_extracted/Benguela_current/20000-CMC-L4_GHRSST-SSTfnd-CMC0.2deg-GLOB-v02.0-f-19910901-20170317.csv")
# names(CMC)<-c("lon","lat", "temp", "date")
# CMC <- CMC %>% 
#   mutate(temp = temp - 273.15)
# 
# # save(CMC, file = "Data/CMC.RData")


# 5: Extracting OISST dataset ----------------------------------------------------

# bbox <- data.frame(BC = c(-35, -25, 15, 20), # Benguela Current
#                    CC = c(25, 35, 340, 355), # Canary Current
#                    CalC = c(35, 45, 225, 240), # California Current
#                    HC = c(-17.5, -7.5, 275, 290), # Humboldt Current
#                    row.names = c("latmin", "latmax", "lonmin", "lonmax"))
# 
# OISST.dir <- "/home/amieroh/Documents/Data/Datasets/OISSTv2/daily/netCDF/avhrr-only"
# OISST.csv.dir <- "/home/amieroh/Documents/Data/Datasets/OISST_subset"
# 
# #          1         2
# # 1234567890123456789012345
# # avhrr-only-v2.19810901.nc
# 
# # function to extract the dims and data from OISST netCDFs
# read_nc <- function(ncFile, region = region, csvDir = csvDir) {
#   coords <- bbox[, region]
#   nc <- nc_open(ncFile)
#   pathLen <- nchar(OISST.dir) + 1 # to account for the "/" that needs to be inserted
#   fNameStem <-
#     substr(ncFile, pathLen + 1, pathLen + 13)
#   fDate <- substr(ncFile, pathLen + 15, pathLen + 22)
#   LatIdx <- which(nc$dim$lat$vals > coords[1] & nc$dim$lat$vals < coords[2])
#   LonIdx <- which(nc$dim$lon$vals > coords[3] & nc$dim$lon$vals < coords[4])
#   sst <- ncvar_get(nc,
#                    varid = "sst",
#                    start = c(LonIdx[1], LatIdx[1], 1, 1),
#                    count = c(length(LonIdx), length(LatIdx), 1, 1)) %>%
#     round(4)
#   dimnames(sst) <- list(lon = nc$dim$lon$vals[LonIdx],
#                         lat = nc$dim$lat$vals[LatIdx])
#   nc_close(nc)
#   sst <-
#     as.data.table(melt(sst, value.name = "temp"), row.names = NULL) %>%
#     mutate(t = ymd(fDate)) %>%
#     na.omit()
#   fwrite(sst,
#          file = paste(csvDir, "/", region, "-", fNameStem, ".", strtDate, "-", endDate, ".csv", sep = ""),
#          append = TRUE, col.names = FALSE)
#   rm(sst)
# }
# 
# # the list of files
# ncList <- list.files(path = OISST.dir, pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
# strtDate <- str_sub(ncList[1], start = 15, end = 22)
# endDate <- str_sub(ncList[length(ncList)], start = 15, end = 22)
# 
# # apply the function
# system.time(llply(ncList, read_nc, region = "BC", csvDir = OISST.csv.dir, .parallel = TRUE))
# # system.time(llply(ncList, read_nc, region = "CC", csvDir = OISST.csv.dir, .parallel = TRUE))
# # system.time(llply(ncList, read_nc, region = "CalC", csvDir = OISST.csv.dir, .parallel = TRUE))
# # system.time(llply(ncList, read_nc, region = "HC", csvDir = OISST.csv.dir, .parallel = TRUE))
# 
# # Loading the data
# BC_avhrr_only_v2_Document_Document <- read_csv("~/Documents/OISST_subset/BC-avhrr-only-v2.Document-Document.csv" )
# names(BC_avhrr_only_v2_Document_Document)<-c("lon","lat", "temp", "date")
# 
# # Saving the data
# save(BC_avhrr_only_v2_Document_Document, file = "Data/OISST.RData")



# 6: Extracting G1SST dataset ----------------------------------------------------

# bbox <- data.frame(BC = c(-35, -25, 15, 20)) # Benguela Current
# library(stringr)
# library(tidyverse)
# library(reshape2)
# library(ncdf4) # library for processing netCDFs
# library(plyr)
# library(lubridate)
# library(data.table)
# library(doMC); doMC::registerDoMC(cores = 7)
# ncDir <- "/home/amieroh/Documents/Data/Datasets/G1SST/daily"
# csvDir <- "/media/amieroh/Seagate Expansion Drive/Extracted G1SST"
# 
# # #          1         2         3         4
# # # 12345678901234567890123456789012345678901
# # # 20020601-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc
# #          1         2         3         4         5         6
# # 1234567890123456789012345678901234567890123456789012345678901
# # 20100609-JPL_OUROCEAN-L4UHfnd-GLOB-v01-fv01_0-G1SST_subset.nc
# 
# 
# # ncFile <- '/home/amieroh/Documents/Data/Datasets/MUR/daily/20020606-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc'
#   ncFun <- function(ncFile = ncFile, region = region, csvDir = csvDir) {
#     coords <- bbox[, region]
#     nc <- nc_open(ncFile)
#     pathLen <- nchar(paste0(ncDir, "/")) + 1
#     fNameStem <-
#       substr(basename(ncFile), 10, 58)
#     fDate <- substr(basename(ncFile), 1, 8)
#     LatIdx <- which(nc$dim$lat$vals > coords[1] & nc$dim$lat$vals < coords[2])
#     LonIdx <- which(nc$dim$lon$vals > coords[3] & nc$dim$lon$vals < coords[4])
#     sst <- ncvar_get(nc, varid = "analysed_sst") %>%
#       round(4)
#     dimnames(sst) <- list(lon = nc$dim$lon$vals,
#                           lat = nc$dim$lat$vals)
#     nc_close(nc)
#     sst <- as_tibble(melt(sst, value.name = "temp"))
#     sst$t <- ymd(fDate)
#     na.omit(sst)
#     fwrite(sst,
#            file = paste0(csvDir, "/", region, "-", fNameStem, "-", strtDate, "-", endDate, ".csv"),
#            append = TRUE, col.names = FALSE)
#     rm(sst)
#   }
# 
# 
#   ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
#   ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
#   strtDate <- str_sub(ncFirst, start = 1, end = 8)
#   endDate <- str_sub(ncLast, start = 1, end = 8)
# 
# llply(ncList, ncFun, region = "BC", csvDir = csvDir, .parallel = TRUE)













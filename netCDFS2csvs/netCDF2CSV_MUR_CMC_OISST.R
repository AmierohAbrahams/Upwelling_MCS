# Script which converts netCDF to CSV
# This script extracts MUR, CMC and OISST data

# the most important of which is that it is self-describing, meaning that software packages can directly read the data and determine its structure
# variable names and essential metadata such as the units
# the information needed to ensure accurate work (reduce the incidence of errors) is available with the data itself
# NetCDF (network Common Data Form) 
# is a file format for storing multidimensional scientific data (variables) such as temperature, humidity, pressure, wind speed, and direction. 


# Laoding Libraries
library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 7)

ncDir <- "/home/amieroh/Documents/Data/Datasets/MUR/daily"
csvDir <- "/home/amieroh/Documents/Data/Datasets/MUR/Extracted_MUR"

#          1         2         3         4
# 12345678901234567890123456789012345678901
# 20020601-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc

read_nc <- function(ncDir = ncDir, csvDir = csvDir)
  ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
  ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
  ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
  strtDate <- str_sub(ncFirst, start = 1, end = 8)
  endDate <- str_sub(ncLast, start = 1, end = 8)

# ncFile <- '/home/amieroh/Documents/Data/Datasets/MUR/daily/20020606-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc'

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


MUR <- read_csv("/home/amieroh/Documents/Data/Datasets/MUR/Extracted_MUR/BC-JPL-L4UHfnd-GLOB-v01-fv04-MUR-20020601-20140727.csv")
names(MUR)<-c("lon","lat", "temp", "date")
MUR <- MUR %>%
  mutate(temp = temp - 273.15)

JPL_L4UHfnd_GLOB_v01_fv04_MUR_20020601_20140727 <- read_csv("~/Documents/JPL-L4UHfnd-GLOB-v01-fv04-MUR-20020601-20140727.csv")

######################################### SUBSET VIA REGION #################

# bbox <- data.frame(BC = c(-35, -25, 15, 20), # Benguela Current
library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 7)
ncDir <- "/home/amieroh/Documents/Data/Datasets/MUR/daily"
csvDir <- "/home/amieroh/Documents/Data/Datasets/MUR/Extracted_MUR"
#          1         2         3         4
# 12345678901234567890123456789012345678901
# 20020601-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc

# ncFile <- '/home/amieroh/Documents/Data/Datasets/MUR/daily/20020606-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc'
  ncFun <- function(ncFile = ncFile, region = region, csvDir = csvDir) {
    coords <- bbox[, region]
    nc <- nc_open(ncFile)
    pathLen <- nchar(paste0(ncDir, "/")) + 1
    fNameStem <-
      substr(basename(ncFile), 10, 38)
    fDate <- substr(basename(ncFile), 1, 8)
    LatIdx <- which(nc$dim$lat$vals > coords[1] & nc$dim$lat$vals < coords[2])
    LonIdx <- which(nc$dim$lon$vals > coords[3] & nc$dim$lon$vals < coords[4])
    sst <- ncvar_get(nc, varid = "analysed_sst") %>%
      round(4)
    dimnames(sst) <- list(lon = nc$dim$lon$vals,
                          lat = nc$dim$lat$vals)
    nc_close(nc)
    sst <- as_tibble(melt(sst, value.name = "temp"))
    sst$t <- ymd(fDate)
    na.omit(sst)
    fwrite(sst,
           file = paste0(csvDir, "/", region, "-", fNameStem, "-", strtDate, "-", endDate, ".csv"),
           append = TRUE, col.names = FALSE)
    rm(sst)
  }


  ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
  ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
  ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
  strtDate <- str_sub(ncFirst, start = 1, end = 8)
  endDate <- str_sub(ncLast, start = 1, end = 8)

llply(ncList, ncFun, region = "BC", csvDir = csvDir, .parallel = TRUE)


# Extracting CMC

ncDir <- "/home/amieroh/Documents/Data/Datasets/CMC/CMC_BC"
csvDir <- "/home/amieroh/Documents/Data/Datasets/CMC/CMC_extracted"

#          1         2         3         4         5         6
# 123456789012345678901234567890123456789012345678901234567890
# 20100609-JPL_OUROCEAN-L4UHfnd-GLOB-v01-fv01_0-G1SST_subset.nc
read_nc <- function(ncDir = ncDir, csvDir = csvDir)
  ncList <- list.files(path = paste0(ncDir), pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
  ncFirst <- head(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
  ncLast <- tail(list.files(path = paste0(ncDir, "/"), pattern = "*.nc", full.names = FALSE), 1)
  strtDate <- str_sub(ncFirst, start = 1, end = 8)
  endDate <- str_sub(ncLast, start = 1, end = 8)

# ncFile <- '/home/amieroh/Documents/Data/Datasets/CMC/CMC_BC/20100609-JPL_OUROCEAN-L4UHfnd-GLOB-v01-fv01_0-G1SST_subset.nc'

  ncFun <- function(ncFile = ncFile, csvDir = csvDir) {
    nc <- nc_open(ncFile)
    pathLen <- nchar(paste0(ncDir, "/")) + 1
    fNameStem <-
      substr(basename(ncFile), 10, 58)
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

# Extracting OISST


bbox <- data.frame(BC = c(-35, -25, 15, 20), # Benguela Current
                   CC = c(25, 35, 340, 355), # Canary Current
                   CalC = c(35, 45, 225, 240), # California Current
                   HC = c(-17.5, -7.5, 275, 290), # Humboldt Current
                   row.names = c("latmin", "latmax", "lonmin", "lonmax"))

OISST.dir <- "/home/amieroh/Documents/Data/Datasets/OISSTv2/daily/netCDF/avhrr-only"
OISST.csv.dir <- "/home/amieroh/Documents/Data/Datasets/OISST_subset"

#          1         2
# 1234567890123456789012345
# avhrr-only-v2.19810901.nc

# function to extract the dims and data from OISST netCDFs
read_nc <- function(ncFile, region = region, csvDir = csvDir) {
  coords <- bbox[, region]
  nc <- nc_open(ncFile)
  pathLen <- nchar(OISST.dir) + 1 # to account for the "/" that needs to be inserted
  fNameStem <-
    substr(ncFile, pathLen + 1, pathLen + 13)
  fDate <- substr(ncFile, pathLen + 15, pathLen + 22)
  LatIdx <- which(nc$dim$lat$vals > coords[1] & nc$dim$lat$vals < coords[2])
  LonIdx <- which(nc$dim$lon$vals > coords[3] & nc$dim$lon$vals < coords[4])
  sst <- ncvar_get(nc,
                   varid = "sst",
                   start = c(LonIdx[1], LatIdx[1], 1, 1),
                   count = c(length(LonIdx), length(LatIdx), 1, 1)) %>%
    round(4)
  dimnames(sst) <- list(lon = nc$dim$lon$vals[LonIdx],
                        lat = nc$dim$lat$vals[LatIdx])
  nc_close(nc)
  sst <-
    as.data.table(melt(sst, value.name = "temp"), row.names = NULL) %>%
    mutate(t = ymd(fDate)) %>%
    na.omit()
  fwrite(sst,
         file = paste(csvDir, "/", region, "-", fNameStem, ".", strtDate, "-", endDate, ".csv", sep = ""),
         append = TRUE, col.names = FALSE)
  rm(sst)
}

# the list of files
ncList <- list.files(path = OISST.dir, pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
strtDate <- str_sub(ncList[1], start = 15, end = 22)
endDate <- str_sub(ncList[length(ncList)], start = 15, end = 22)

# apply the function
system.time(llply(ncList, read_nc, region = "BC", csvDir = OISST.csv.dir, .parallel = TRUE))
# system.time(llply(ncList, read_nc, region = "CC", csvDir = OISST.csv.dir, .parallel = TRUE))
# system.time(llply(ncList, read_nc, region = "CalC", csvDir = OISST.csv.dir, .parallel = TRUE))
# system.time(llply(ncList, read_nc, region = "HC", csvDir = OISST.csv.dir, .parallel = TRUE))


































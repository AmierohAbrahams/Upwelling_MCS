# Extracting ERA 5 wind u and v variables
# N/W/S/E coordinate format when downloading
# -26.00/15.00/-36.00/22.00

# Loading libraries
library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 7)

# ERA.dir <- "/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate"
# ERA.csv.dir <- "/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate/Extracted"

####### u-wind 2008-2019
# #          1         2         3         4
# # 12345678901234567890123456789012345678901
# # wu_daily08-19.nc

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate/wu_daily08-19.nc'  
nc <- nc_open(ncFile)
fNameStem <-
  substr(basename(ncFile), 1, 13)
u_10 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10) <- list(lon = nc$dim$lon$vals,
                         lat = nc$dim$lat$vals,
                         time = nc$dim$time$vals)
nc_close(nc)
u_10 <- as_tibble(melt(u_10, value.name = "u_10"))
u_10$time <- as.POSIXct(u_10$time, origin = "1970-01-01")



###### v-wind 2008-2019
# #          1         2         3         4
# # 12345678901234567890123456789012345678901
# # wu_daily08-19.nc

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate/wv_daily08-19.nc'
nc <- nc_open(ncFile)
fNameStem <-
  substr(basename(ncFile), 1, 13)
v_10 <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
v_10 <- as_tibble(melt(v_10, value.name = "v_10"))
v_10$time <- as.POSIXct(v_10$time, origin = "1970-01-01")

#####################################################################################
### Extracting 1981 - 2007 u10
# #          1         2         3         4
# # 12345678901234567890123456789012345678901
# # wind_daily81-99_165.nc

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/wind_daily81-99_165.nc'
nc <- nc_open(ncFile)
fNameStem <-
  substr(basename(ncFile), 1, 19)
u_10 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
u_10 <- as_tibble(melt(u_10, value.name = "u_10"))

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/wind_daily00-07_165.nc'
nc <- nc_open(ncFile)
fNameStem <-
  substr(basename(ncFile), 1, 13)
u_10 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
u_10 <- as_tibble(melt(u_10, value.name = "u_10"))


wind_u10_1999_2007 <- u10 %>% 
  cbind(u10_2)

#############################################################################3
### Extracting 1981 - 2007 u10
# #          1         2         3         4
# # 12345678901234567890123456789012345678901
# # wind_daily81-99_166.nc

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/wind_daily81-99_166.nc'
nc <- nc_open(ncFile)
fNameStem <-
  substr(basename(ncFile), 1, 19)
v_10 <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
v_10 <- as_tibble(melt(v_10, value.name = "v_10"))

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/wind_daily00-07_166.nc'
nc <- nc_open(ncFile)
fNameStem <-
  substr(basename(ncFile), 1, 13)
v_10 <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
v_10_2 <- as_tibble(melt(v_10, value.name = "v_10"))


wind_v10_1999_2007 <- v10 %>% 
  cbind(v10_2)




















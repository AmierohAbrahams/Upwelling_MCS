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


ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/wind_daily81-99_165.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 13)
u_10 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
u_10_df <- as_tibble(melt(u_10, value.name = "u_10"))
u_10_df$time <- as.POSIXct(u_10_df$time * 3600, origin = "1900-01-01")


ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/wind_daily81-99_166.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 19)
v_10.2_df <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10.2_df) <- list(lon = nc$dim$lon$vals,
                            lat = nc$dim$lat$vals,
                            time = nc$dim$time$vals)
nc_close(nc)
v_10.2_df <- as_tibble(melt(v_10.2, value.name = "v_10"))
v_10.2_df$time <- as.POSIXct(v_10.2_df$time * 3600, origin = "1900-01-01")


ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/wind_daily_165.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 14)
u_10.2 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10.2) <- list(lon = nc$dim$lon$vals,
                         lat = nc$dim$lat$vals,
                         time = nc$dim$time$vals)
nc_close(nc)
u_10.2_df <- as_tibble(melt(u_10.2, value.name = "u_10"))
u_10.2_df$time <- as.POSIXct(u_10.2_df$time * 3600, origin = "1900-01-01")
#######

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate/wu_daily08-19.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 14)
u_10.3_df <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10.3_df) <- list(lon = nc$dim$lon$vals,
                            lat = nc$dim$lat$vals,
                            time = nc$dim$time$vals)
nc_close(nc)
u_10.3_df <- as_tibble(melt(u_10.3_df, value.name = "u_10"))
u_10.3_df$time <- as.POSIXct(u_10.3_df$time * 3600, origin = "1900-01-01")
#####

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate/wv_daily08-19.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 14)
v_10.3_df <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10.3_df) <- list(lon = nc$dim$lon$vals,
                            lat = nc$dim$lat$vals,
                            time = nc$dim$time$vals)
nc_close(nc)
v_10.3_df <- as_tibble(melt(v_10.3_df, value.name = "u_10"))
v_10.3_df$time <- as.POSIXct(v_10.3_df$time * 3600, origin = "1900-01-01")


ERA_5_wind <- rbind(u_10_df, u_10.2_df, u_10.3_df)








library(tidyverse)
library(lubridate)
library(multcomp)
options(scipen = 999)

# Loading upwelling metrics
load("Data/OISST_final.RData")
load("Data/G1SST_final.RData")
load("Data/SACTN_upwell_base.RData")
load("Data/MUR_final.RData")
load("Data/CMC_final.RData")
SACTN_upwell_base <- SACTN_upwell_base %>% 
  mutate(product = "SACTN")

combined_products <- rbind(OISST_final,CMC_final,MUR_final,G1SST_final)
combined_products <- combined_products %>% 
  filter(site == "Saldanha Bay") %>% 
    filter(distance == "10000") %>% 
  filter(year(date_start) %in% 2011:2014)

### Loading the temperature products
load("~/Documents/Upwelling_MCS/Data/Final_G1SST.RData") #G1SST
load("~/Documents/Upwelling_MCS/Data/Final.RData") # MUR
load("~/Documents/Upwelling_MCS/Data/SACTN_US.RData") # SACTN
load("~/Documents/Upwelling_MCS/Data/OISST_fill.RData") # OISST
load("~/Documents/Upwelling_MCS/Data/CMC_fill.RData") # CMC temperature

###
# In Lamberts Bay in June 2012 signals detected below all at a distance of 10 km from the coastline
# CMC - 2012-06-12 signal lasted 3 days
#G1SST - 2012-06-10 signal lsted 36 days
#MUR - 2012-06-10 signal lsted 36 days
#OISST - 2012-06-10 signal lsted 18 days

combined_SST <- rbind(OISST_fill,CMC_fill,Final,Final_G1SST)
combined_SST <- combined_SST %>% 
  filter(year(date) == 2012) # In 2011 in 

# Plotting a time series
ggplot(combined_SST, aes(x = date, y = temp, colour = product)) +
  geom_line()

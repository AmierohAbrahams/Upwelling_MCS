# Analyses
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Loading all of the data 
# 3: Preparing data to plot the box plot
# 4: Correlations

# 1: Setup environment ----------------------------------------------------
# Loading libraries
library(tidyverse)
library(lubridate)
library(multcomp)
options(scipen = 999)
# devtools::install_github("twitter/AnomalyDetection")
#library(AnomalyDetection)
#devtools::install_github("easystats/correlation")
library(correlation)

# 2: Loading data ----------------------------------------------------
# load("Data/OISST_final.RData")
# load("Data/G1SST_final.RData")
# load("Data/SACTN_upwell_base.RData")
# load("Data/MUR_final.RData")
# load("Data/CMC_final.RData")

# Loading data: Created in SST_patterns.R and upwell_IDX.Rmd
load("Data_coast_angle/OISST_upwell_base.RData")
load("Data_coast_angle/CMC_upwell_base.RData")
load("Data_coast_angle/SACTN_upwell_base.RData")
load("Data_coast_angle/MUR_upwell_base.RData")
load("Data_coast_angle/G1SST_upwell_base.RData")

combined_products <- rbind(OISST_upwell_base,CMC_upwell_base,MUR_upwell_base, G1SST_upwell_base)
# save(combined_products, file = "Data_coast_angle/combined_products.RData")

# 2: Preparing box plot data ----------------------------------------------------

seasons_func <- function(df){
  BC_seaons <- df %>%
    mutate(month = month(as.Date(as.character(date_start)), abbr = T, label = T),
           year = year(date_start)) %>%
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",
                           ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Spring","Error")))))
}
 
combined_products <- seasons_func(combined_products)
SACTN <- seasons_func(df = SACTN_upwell_base)

metric_prods <- combined_products %>% 
  filter(year(date_start) %in% 2011:2014) %>% 
  filter(season == "Summer") %>% 
  dplyr::select(-heading,-distance)

metric_SACTN <- SACTN %>% 
  filter(year(date_start) %in% 2011:2014) %>% 
  mutate(product = "SACTN") 

metrics_func <- function(df){
  metrics <- df %>% 
  mutate(year = year(date_start)) %>% 
  group_by(product, site, duration) %>% 
  summarise(y = n())%>% 
  rename(count = y)
}

metrics <- metrics_func(df = metric_prods)
metrics_SACTN <- metrics_func(df = metric_SACTN)

final_combined <- rbind(metrics, metric_SACTN)
final_combined$count <- as.numeric(final_combined$count)

ggplot(data = final_combined, aes(x = product,y = duration)) +
  geom_boxplot(aes(fill = product)) +
  facet_wrap(~site)  +
  labs(y = "Duration (Days)", x = "SST products")+
  theme_update(plot.background = element_rect(colour = NA),
               panel.background = element_rect(colour = NA, fill = "grey95"),
               panel.border = element_rect(colour = NA, fill = NA, size = 0.5),
               panel.grid.major = element_line(colour = "lightblue", size = 0.2),
               panel.grid.minor = element_blank(),
               axis.title = element_text(face = "bold", size = 9),
               axis.text = element_text(size = 7),
               axis.ticks = element_line(size = 0.3),
               axis.ticks.length = unit(0.5, "mm"),
               legend.position = "none",
               legend.direction = "vertical",
               legend.text = element_text(size = 6),
               legend.title = element_text(size = 6),
               legend.key = element_blank(),
               legend.key.height = unit(.25, "cm"),
               legend.background = element_rect(fill = NA, colour = "black", size = 0.2),
               strip.background = element_rect(fill = "lightblue", colour = NA),
               strip.text = element_text(size = 8))

# 4: Correlation ----------------------------------------------------
# Doing a correlation to test if a signal present at 0 km is present at 25 km and the chances that it is present at 50 km

# Run a correlation of daily temperatures at 0 km against each of the different distances
# Run only for the dates where upwelling is occuring 

# Loading the daily temperatures
load("Data_coast_angle/OISST_upwell_clims.RData")
load("Data_coast_angle/CMC_upwell_clims.RData")
load("Data_coast_angle/MUR_upwell_clims.RData")
load("Data_coast_angle/G1SST_upwell_clims.RData")

SST_clims <- rbind(OISST_upwell_clims,G1SST_upwell_clims,CMC_upwell_clims,MUR_upwell_clims)

# Determining the anomaly temperature for SST products and SACTN
SST_anom <- SST_clims %>% 
  group_by(site, product, distance) %>% 
  mutate(anom = temp - seas) %>% 
  filter(year(t) %in% 2011:2014) %>% 
  filter(event_no > 0) %>% # Activate this line to only use dates during an upwelling event
  dplyr::select(site, t, distance, temp, anom, product) %>% 
  na.omit() %>% 
  unique()

# Create wide anomaly data.frame
sst_anom_wide <- SST_anom %>% 
  pivot_wider(id_cols = c(site, product, t), names_from = distance, values_from = c(temp, anom))

# Doing the correlation using the easystat correlation package
# https://rdrr.io/github/easystats/correlation/man/correlation.html
SST_corr <- sst_anom_wide %>% 
  group_by(site, product) %>% 
  correlation(method = "pearson") #%>% 
  # summary()

# anom_spread<- SST_anom %>%
#   spread(key = distance, value = temp)

SST_anom_spread <- pivot_wider(SST_anom, names_prefix = "dist_",
                               names_from = distance, values_from = temp)

slope_calc <- function(df){
  df %>% 
    do(mod1 = cor(.$dist_0, .$dist_25000, method = "pearson", use = "complete.obs"),
       mod2 = cor(.$dist_0, .$dist_50000, method = "pearson", use = "complete.obs")) %>%
    mutate(dist_0_vs_25_r = mod1[1],
           dist_0_vs_50_r = mod2[1]) %>% 
    dplyr::select(-mod1, -mod2) %>% 
    mutate_if(is.numeric, round, 2)
}

distance_corr <- SST_anom_spread %>% 
  group_by(site, product) %>% 
  slope_calc()


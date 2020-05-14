# Analyses
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Loading all of the data 
# 3: Preparing data to plot the box plot
# 4: Correlations
# 5: Testing the diffetrence in the anaomly temperatures compared to actual tenperatures

# 1: Setup environment ----------------------------------------------------
# Loading libraries
library(tidyverse)
library(lubridate)
library(multcomp)
options(scipen = 999)
# devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

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
  theme_bw()+
  theme(legend.position="none") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold", size = 12))

# 4: Correlation ----------------------------------------------------
# Doing a correlation to test if a signal present at 0 km is present at 25 km and the chances that it is present at 50 km

# Run a correlation of daily temperatures at 0 km against each of the different distances
# Run only for the dates where upwelling is occuring 

# Loading the daily temperatures
load("Data_coast_angle/OISST_fill.RData")
load("Data_coast_angle/CMC_fill.RData")
load("Data_coast_angle/MUR_fill.RData")
load("Data_coast_angle/G1SST_last.RData")
load("Data/SACTN_US.RData")

SST_products <- rbind(OISST_fill,G1SST_last,CMC_fill,MUR_fill)

# Determining the anomaly teperature for SST products and SACTN
SST_anaomaly <- SST_products %>% 
  group_by(site,product) %>% 
  mutate(anom = temp - mean(temp, na.rm = TRUE))

SACTN_anaomaly <- SACTN_US %>% 
  group_by(site) %>% 
  mutate(anom = temp - mean(temp, na.rm = TRUE))

SST_anaomaly <- SST_anaomaly %>% 
  dplyr::select(temp,anom, product,site,date, distance)
SST_anaomaly$distance <- as.numeric(SST_anaomaly$distance)


# LOading the upwelling metric data
load("Data_coast_angle/OISST_upwell_base.RData")
load("Data_coast_angle/CMC_upwell_base.RData")
load("Data_coast_angle/SACTN_upwell_base.RData")
load("Data_coast_angle/MUR_upwell_base.RData")
load("Data_coast_angle/G1SST_upwell_base.RData")

combined_products <- rbind(OISST_upwell_base,CMC_upwell_base,MUR_upwell_base, G1SST_upwell_base)
combined_products <- combined_products %>% 
  rename(date = date_start)

# Matching the temperature and anomaly column to the upwelling metrics
matched_anaomaly<- combined_products %>% 
  left_join(SST_anaomaly, by = c("date", "site", "product", "distance"))

#save(matched_anaomaly, file = "Data_coast_angle/matched_anaomaly.RData")

load("Data_coast_angle/matched_anaomaly.RData")

matched_anaomaly_sub <- matched_anaomaly %>% 
  filter(year(date) %in% 2011:2014) %>% 
  dplyr::select(product,distance,site,temp)

# slope_calc <- function(df){
#   df %>%
#     do(mod1 = cor(.$distance, .$temp, method = "pearson", use = "complete.obs")) %>%
#     mutate(dist_temp= mod1[1]) %>%
#     dplyr::select(-mod1) %>%
#     mutate_if(is.numeric, round, 2)
# }

slope_calc <- function(df){
  df %>%
    cor.test(x = .$distance, .$temp,
             use = "complete.obs", method = "pearson") %>% 
    mutate_if(is.numeric, round, 2)
}

distance_corr <- matched_anaomaly_sub %>%
 #group_by(site, product, distance) %>%
  group_by(site, product) %>%
  slope_calc()

# 5: Anomaly temperature ----------------------------------------------------
# How did the anaomaly temperatures vary compared to actual temps


# ggplot(data =matched_anaomaly, aes(x = date, y = anom, colour = product)) +
#   geom_line() +
#   facet_wrap(~site)

summary(aov(temp ~site + product + distance , data = matched_anaomaly_sub))
# summary(aov(temp ~ anom + site + product + distance , data = matched_anaomaly))

mode <- lm(temp ~ anom + site + product + distance, data = matched_anaomaly)
ANOVA <- aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'matched_anaomaly$temp', conf.level=0.95)


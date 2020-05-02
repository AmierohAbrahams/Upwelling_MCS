# Loading libraries
library(tidyverse)
library(lubridate)
library(multcomp)
options(scipen = 999)

# load("Data/OISST_final.RData")
# load("Data/G1SST_final.RData")
load("Data/SACTN_upwell_base.RData")
# load("Data/MUR_final.RData")
# load("Data/CMC_final.RData")

# Loading data: Created in SST_patterns.R and upwell_IDX.Rmd
# load("Data_coast_angle/OISST_upwell_base.RData")
# load("Data_coast_angle/CMC_upwell_base.RData")
# load("Data_coast_angle/SACTN_upwell_base.RData")
# load("Data_coast_angle/MUR_upwell_base.RData")
# load("Data/G1SST_upwell_base.RData")
# 
seasons_func <- function(df){
  BC_seaons <- df %>%
    mutate(month = month(as.Date(as.character(date_start)), abbr = T, label = T),
           year = year(date_start)) %>%
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",
                           ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Spring","Error")))))
}
# 
# combined_products <- rbind(OISST_upwell_base,CMC_upwell_base,MUR_upwell_base, G1SST_upwell_base.RData)
# #write.csv(combined_products,file = "combined_products.csv")

combined_products <- read_csv("Data_coast_angle/combined_products.csv")

combined_products <- seasons_func(df = combined_products)
SACTN <- seasons_func(df = SACTN_upwell_base)

metric_prods <- combined_products %>% 
  filter(year(date_start) %in% 2011:2014) %>% 
  filter(season == "Summer") %>% 
  dplyr::select(-heading,-distance)

metric_SACTN <- SACTN %>% 
  filter(year(date_start) %in% 2011:2014) %>% 
  mutate(product = "SACTN")
  filter(season == "Summer")

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

# ggplot(data = final_combined, aes(x = mean_dur, y = count)) +
#   geom_boxplot(aes(colour = product)) +
#   facet_wrap(~site) 
# 
# 
# ggplot(data = final_combined, aes(x = mean_dur)) +
#   geom_histogram(aes(fill = product),  
#                  position = "dodge", alpha=0.6) +
#   labs(x = "Duration (Days)", y = "Number of upwelling signals", colour = "Temperature products") +
#   facet_wrap(~site) 

ggplot(data = final_combined, aes(x = product,y = duration)) +
  geom_boxplot(aes(fill = product)) +
  facet_wrap(~site)  +
  labs(y = "Duration (Days)", x = "SST product")+
  theme_bw()+
  theme(legend.position="none") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold"))

#############################################
#Correlations

load("~/Documents/Upwelling_MCS/Data_coast_angle/combined_products.RData")

#Calculating the number of signals detected at different distances from the coastline
total_signals <- combined_products %>%
  mutate(year = year(date_start)) %>% 
  group_by(product, distance) %>% 
  summarise(y = n()) %>% 
  rename(count = y)
  
# Correlation comparing signals detected at 10km with signals detected at 30km and, signals detected at 10km and 50km
correlation_signals <- combined_products %>%
  mutate(year = year(date_start)) %>% 
  group_by(product, distance,site) %>% 
  summarise(y = n())

combined_freq_spread <- pivot_wider(Combined_frequency, names_prefix = "dist_",
                                    names_from = distance, values_from = number_of_signals)

slope_calc <- function(df){
  df %>% 
    # mutate(row_num = 1:n()) %>% 
    # do(mod1 = lm(number_of_signals ~ row_num, data = .),
    # mod2 = lm(distance ~ row_num, data = .),
    # mod3 = lm(distance ~ number_of_signals, data = .),
    # mod4 = cor(.$distance, .$number_of_signals, method = "pearson", use = "complete.obs")[1]) %>% 
    # mutate(distance_slope = summary(mod1)$coeff[2],
    #    signal_slope = summary(mod2)$coeff[2],
    #    signal_distance_slope = summary(mod3)$coeff[2],
    #    signal_distance_r = mod4[1],
    #    signal_distance_r2 = glance(mod3)$adj.r.squared) %>%
    # select(-mod1, -mod2, -mod3, -mod4) %>% 
  do(mod1 = cor(.$dist_10, .$dist_30, method = "pearson", use = "complete.obs"),
     mod2 = cor(.$dist_10, .$dist_50, method = "pearson", use = "complete.obs")) %>%
    mutate(dist_10_vs_30_r = mod1[1],
           dist_10_vs_50_r = mod2[1]) %>% 
    # select(dist_10_vs_30_r, dist_10_vs_50_r) %>% 
    dplyr::select(-mod1, -mod2) %>% 
    mutate_if(is.numeric, round, 2)
}

#### test <- cor(combined_freq_spread$`10`, combined_freq_spread$`30`)

distance_corr <- combined_freq_spread %>% 
  group_by(site, product) %>% 
  slope_calc()







# Loading libraries
library(tidyverse)
library(lubridate)
library(multcomp)
options(scipen = 999)

load("Data/OISST_final.RData")
load("Data/G1SST_final.RData")
load("Data/SACTN_upwell_base.RData")
load("Data/MUR_final.RData")
load("Data/CMC_final.RData")

combined_products <- rbind(OISST_final,CMC_final,MUR_final,G1SST_final)

seasons_func <- function(df){
  BC_seaons <- df %>% 
    mutate(month = month(as.Date(as.character(date_start)), abbr = T, label = T),
           year = year(date_start)) %>% 
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",        
                           ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Spring","Error")))))
}

combined_products <- seasons_func(df = combined_products)
SACTN <- seasons_func(df = SACTN_upwell_base)


metric_prods <- combined_products %>% 
  filter(year(date_start) %in% 2011:2014) %>% 
  filter(season == "Summer")

metric_SACTN <- SACTN %>% 
  filter(year(date_start) %in% 2011:2014) %>% 
  mutate(product = "SACTN")

combined <- rbind(metric_prods, metric_SACTN)
# write.csv(combined,'combined.csv')

metrics_func <- function(df){
  metrics <- df %>% 
  mutate(year = year(date_start)) %>% 
  group_by(product, site) %>% 
  summarise(y = n(),
            mean_intensity = mean(intensity_mean),
            mean_dur = mean(duration),
            mean_cumIn = mean(intensity_cumulative)) %>% 
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

counts <- read_csv("Data/counts.csv")

ggplot(data = combined, aes(x = product,y = duration)) +
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








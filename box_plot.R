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
library(lubridate)
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

Ordering <- c("OISST", "CMC","G1SST", "MUR", "SACTN")

ggplot(data = final_combined, aes(x = product,y = duration)) +
  geom_point(data = metric_prods,shape = 21, fill = "lightgray",
             color = "black", size = 3) +
  geom_point(data = metric_SACTN,shape = 21, fill = "lightgray",
             color = "black", size = 3) +
  geom_boxplot(aes()) +
  scale_x_discrete(limits = Ordering) +
  facet_wrap(~site)  +
  labs(y = "Duration (Days)", x = "SST products")+
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

# 4: Correlation ----------------------------------------------------
# Doing a correlation to test if a signal present at 0 km is present at 25 km and the chances that it is present at 50 km

# Run a correlation of daily temperatures at 0 km against each of the different distances
# Run only for the dates where upwelling is occuring 

# Loading the daily temperatures
load("Data_coast_angle/OISST_upwell_clims.RData")
load("Data_coast_angle/CMC_upwell_clims.RData")
load("Data_coast_angle/MUR_upwell_clims.RData")
load("Data_coast_angle/G1SST_upwell_clims.RData")

MUR_upwell_clims <- MUR_upwell_clims %>% 
  mutate(distance = as.numeric(as.character(distance)))

SST_clims <- rbind(OISST_upwell_clims,G1SST_upwell_clims,CMC_upwell_clims,MUR_upwell_clims)

# Determining the anomaly temperature for  SST products and SACTN
SST_anom <- SST_clims %>% 
  group_by(site, product, distance) %>% 
  mutate(anom = temp - seas) %>% 
  #filter(year(t) %in% 2011:2014) %>% 
  filter(event_no > 0) %>% # Activate this line to only use dates during an upwelling event
  dplyr::select(site, t, distance, temp, anom, product) %>% 
  na.omit() %>% 
  unique()

# Create wide anomaly data.frame
sst_anom_wide <- SST_anom %>% 
  pivot_wider(id_cols = c(site, product, t), names_from = distance, values_from = c(temp, anom))
sst_anom_wide <- as.data.frame(sst_anom_wide)
write_csv(sst_anom_wide, path = "sst_anom_wide.csv")

# Doing the correlation using the easystat correlation package
# https://rdrr.io/github/easystats/correlation/man/   correlation.html
SST_corr <- sst_anom_wide %>% 
  group_by(site, product) %>% 
  correlation(method = "pearson") #%>% 


  # summary()
save(sst_anom_wide, file = "sst_anom_wide.RData")
# anom_spread<- SST_anom %>%
#   spread(key = distance, value = temp)

SST_anom_spread <- pivot_wider(SST_anom, names_prefix = "dist_",
                               names_from = distance, values_from = temp)

slope_calc <- function(df){
  cor(df[5:6,], method="pearson")
}

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
  

## Bar graph plot
Ordering <- c("OISST", "CMC","G1SST", "MUR")
no_upwelling_sigs <- read_csv("Data_coast_angle/number_upwelling_sigs.csv")

test <- combined_products %>% 
  mutate(year = year(date_start)) %>% 
  #filter(year(date_start) %in% 2011:2014)
  filter(year %in% 2011:2016) %>% 
  group_by(year, distance, site, product) %>% 
  summarise(y = n())%>% 
  rename(count = y)
  
test1 <- test %>% 
 group_by(site,product,distance) %>% 
  summarise(mean_signals = mean(count),
              sd_signals = sd(count)) %>% 
  mutate(Distance = case_when(distance == "25000" ~ "25",
                            distance == "50000" ~ "50",
                            distance == "0" ~ "0",))

ggplot(data = test1, aes(x = product, y = mean_signals, group = factor(Distance), fill = factor(Distance))) +
  geom_bar(stat = "identity",position = position_dodge2(width =0.5), width = 0.5) +
  geom_errorbar(aes(ymin = mean_signals - sd_signals, ymax = mean_signals + sd_signals),width=0.4, alpha=0.9, size=0.5, position=position_dodge(.5))+ # Creates error bars for the graph from previously calculated data
 # scale_y_continuous(breaks = c(50,100,150,200,250,300,350,400))+
  scale_x_discrete(limits = Ordering) +
  scale_fill_grey(start = .1, end = .5, labels = c("0", "25", "50")) +
  scale_fill_discrete(labels = c("0", "25", "50")) +
    labs(x ="", y = "Upwelling events (count)") +
  labs(fill = "Distance (km)") + 
  facet_wrap(~site) +
  theme_set(theme_grey()) +
  theme_grey() +
  scale_fill_manual(values = c("grey79", "grey57", "grey40"))+
  theme(
    #panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=12, family = "Palatino"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.ticks = element_line(colour = "black"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18, family = "Palatino"),
    legend.text = element_text(size = 16, family = "Palatino"),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

#########################
load("Data/metric_ANOVA.RData") # Created in the ANOVA.R script

Ordering <- c("OISST", "CMC","G1SST", "MUR")
ggplot(data = metrics, aes(x = product, y = count, group = factor(distance), fill = factor(distance))) +
  #geom_bar(stat = "identity",position = position_dodge2(width =0.5), width = 0.5) +
  geom_boxplot()+
  scale_y_continuous(breaks = c(50,100,150,200,250,300,350,400))+
  scale_x_discrete(limits = Ordering) +
  scale_fill_grey(start = .1, end = .5, labels = c("0", "25", "50")) +
  labs(x ="SST product", y = "Number of upwelling signals") +
  labs(fill = "Distance (km)") + 
  facet_wrap(~site)+
  theme_set(theme_grey()) +
  theme_grey() +
  scale_fill_manual(values = c("grey79", "grey57", "grey40"))+
  theme(
    #panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=12, family = "Palatino"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.ticks = element_line(colour = "black"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18, family = "Palatino"),
    legend.text = element_text(size = 16, family = "Palatino"),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

plot8 <- ggplot(data = metric_ANOVA, aes(x = product, y = count)) +
  geom_boxplot() +
  facet_wrap(vars(site), ncol = 4) +
    xlab("Data product") + ylab("Number of upwelling signals ")+
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

# AJ requested plots

metrics_func <- function(df){
  metrics <- metric_prods %>% 
    mutate(year = year(date_start)) %>% 
    group_by(product, site, intensity_mean) %>% 
    summarise(y = n())
}


metrics <- metrics_func(df = metric_prods)
metrics_SACTN <- metrics_func(df = metric_SACTN)
final_combined <- rbind(metrics, metric_SACTN)

ggplot(data = final_combined, aes(x = product,y = intensity_mean)) +
  geom_point(data = metric_prods,shape = 21, fill = "lightgray",
             color = "black", size = 3) +
  geom_point(data = metric_SACTN,shape = 21, fill = "lightgray",
             color = "black", size = 3) +
  geom_boxplot(aes()) +
  scale_x_discrete(limits = Ordering) +
  facet_wrap(~site)  +
  labs(y = "Mean intensity (°C)", x = "SST products")+
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())


metrics_func <- function(df){
  metrics <- metric_prods %>% 
    mutate(year = year(date_start)) %>% 
    group_by(product, site, duration) %>% 
    summarise(y = n())
}


metrics <- metrics_func(df = metric_prods)
metrics_SACTN <- metrics_func(df = metric_SACTN)
final_combined <- rbind(metrics, metric_SACTN)

ggplot(data = test, aes(x = product,y = duration)) +
  geom_point(data = metric_prods,shape = 21, fill = "lightgray",
             color = "black", size = 3) +
  geom_point(data = metric_SACTN,shape = 21, fill = "lightgray",
             color = "black", size = 3) +
  geom_boxplot(aes()) +
  scale_x_discrete(limits = Ordering) +
  facet_wrap(~site)  +
  labs(y = "Duration (Days)", x = "SST products")+
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

metrics_func <- function(df){
  metrics <- metric_prods %>% 
    mutate(year = year(date_start)) %>% 
    group_by(product, site, intensity_cumulative) %>% 
    summarise(y = n())
}

metrics <- metrics_func(df = metric_prods)
metrics_SACTN <- metrics_func(df = metric_SACTN)
final_combined <- rbind(metrics, metric_SACTN)

ggplot(data = final_combined, aes(x = product,y = intensity_cumulative)) +
  geom_point(data = metric_prods,shape = 21, fill = "lightgray",
             color = "black", size = 3) +
  geom_point(data = metric_SACTN,shape = 21, fill = "lightgray",
             color = "black", size = 3) +
  geom_boxplot(aes()) +
  scale_x_discrete(limits = Ordering) +
  facet_wrap(~site)  +
  labs(y = "Cumulative intensity (°C.days)", x = "SST products")+
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())


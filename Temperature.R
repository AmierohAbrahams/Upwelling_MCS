# The intention of this script is to plot mean yearly temperature for summer seasons and
# the number of upwelling signals detected during this time from 2011-2016
# The main aim was to test whether an increase in the SST will rest in fewer upwelling signals being detected. 

# Loading libraries
library(lubridate)
library(tidyverse)

load("Data_coast_angle/UI_angle.RData") # Created in script 'upwell_IDX.Rmd'
# load("Data_coast_angle/G1SST_last.RData") # Created in Extracting folder : extract_tidync.R script"
load("Data_coast_angle/CMC_fill_2015_2016.RData")
load("Data_coast_angle/MUR_fill.RData")
load("Data_coast_angle/OISST_fill_2015_2016.RData")
load("Data_coast_angle/MUR_fill_1.RData")
load("Data_coast_angle/CMC_fill.RData")
load("Data_coast_angle/OISST_fill.RData")
# load("Data_coast_angle/MUR_updated.RData")

MUR_fill_1 <- MUR_fill_1 %>% 
  rename(date = Month_Yr) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(temp = temp - 273.15)

OISST <- rbind(OISST_fill_2015_2016, OISST_fill)
MUR <- rbind(MUR_fill_1, MUR_fill)
CMC <- rbind(CMC_fill, CMC_fill_2015_2016) 

seasons_func <- function(df){
  BC_seaons <- df %>%
    mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
           year = year(date)) %>%
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",
                           ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Spring","Error")))))
}

OISST_temp <- seasons_func(OISST)
CMC_temp <- seasons_func(CMC)
MUR_temp <- seasons_func(MUR)

temp_func <- function(df){
  yearly_temp <- df %>% 
   group_by(site, year) %>% 
   filter(season == "Summer") %>% 
   summarise(mean_temp = mean(temp)) %>% 
   filter(year %in% seq(2011, 2016, 1))
}

OISST_temp_season <- temp_func(df = OISST_temp) %>% 
  mutate(product = "OISST")
MUR_temp_season <- temp_func(df = MUR_temp) %>% 
  mutate(product = "MUR")
CMC_temp_season <- temp_func(df = CMC_temp) %>% 
  mutate(product = "CMC")

temps_prods <- rbind(CMC_temp_season, MUR_temp_season,OISST_temp_season)
write_csv(temps_prods, path = "data/temps_prods.csv")

ggplot(data = temps_prods, aes(x = year, y = mean_temp)) +
  # geom_line(aes(colour = product)) +
  geom_smooth(aes(colour = product), method = "lm") +
  facet_wrap(~site) +
  labs(x = "Year", y = "SST")+
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.4, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18, family = "Palatino"),
    legend.text = element_text(size = 16, family = "Palatino"),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())


##########################################################################################################################################
#Determining the mean Duration, intensity, count

load("Data_coast_angle/OISST_upwell_base.RData")
load("Data_coast_angle/CMC_upwell_base.RData")
load("Data_coast_angle/SACTN_upwell_base.RData")
load("Data_coast_angle/MUR_upwell_base.RData")
load("Data_coast_angle/G1SST_upwell_base.RData")
load("Data_coast_angle/CMC_2015_upwell_base.RData")
load("Data_coast_angle/OISST_2015_upwell_base.RData")
load("Data_coast_angle/MUR_upwell_base_2015.RData")

CMC_upwell_base <- rbind(CMC_2015_upwell_base, CMC_upwell_base)
OISST_upwell_base <- rbind(OISST_2015_upwell_base, OISST_upwell_base)
MUR_upwell_base <- rbind(MUR_upwell_base, MUR_upwell_base_2015)

seasons_func <- function(df){
  BC_seaons <- df %>%
    mutate(month = month(as.Date(as.character(date_start)), abbr = T, label = T),
           year = year(date_start)) %>%
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",
                           ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Spring","Error")))))
}

OISST_upwell <- seasons_func(OISST_upwell_base)
CMC_upwell <- seasons_func(CMC_upwell_base)
MUR_upwell <- seasons_func(MUR_upwell_base)

average_func <- function(df){
  yearly_average <- df %>% 
    group_by(site, year) %>% 
    filter(season == "Summer") %>% 
    summarise(mean_duration = mean(duration),
              mean_intensity = mean(intensity_mean),
              y = n()) %>% 
    filter(year %in% seq(2011, 2016, 1))
}

OISST_average_func <- average_func(df = OISST_upwell) %>% 
  mutate(product = "OISST")
MUR_average_func <- average_func(df = MUR_upwell) %>% 
  mutate(product = "MUR")
CMC_average_func <- average_func(df = CMC_upwell) %>% 
  mutate(product = "CMC")

complete_info <- rbind(OISST_average_func,CMC_average_func,MUR_average_func)

ggplot(data = complete_info, aes(x = year, y = y)) +
  geom_line(aes(colour = product)) +
  #geom_smooth(aes(colour = product), method = "lm") +
  facet_wrap(~site) +
  labs(x = "Year", y = "SST")+
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.4, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18, family = "Palatino"),
    legend.text = element_text(size = 16, family = "Palatino"),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

ggplot(data = complete_info, aes(x = year, y = y, group = factor(product), fill = factor(product))) +
  geom_bar(stat = "identity",position = position_dodge2(width =0.5), width = 0.5) +
 # ggplot(data = temps_prods, aes(x = year, y = mean_temp)) +
  #geom_line(aes(colour = product)) +
  geom_smooth(data = complete_info, aes(x = year, y = mean_duration), colour = "product", method = "lm") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Number of upwelling signals")) +
  facet_wrap(~site)

write_csv(complete_info, path = "data/complete_info.csv")







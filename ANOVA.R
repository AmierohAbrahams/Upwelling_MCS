## Using a series of general linear hypotheses to look for differences in upwelling signal metrics between datasets and between sites (?)
## Is there a significant difference in the duration/intensity etc between the datasets at a particular site?
  
  library(lubridate)
  library(multcomp)
  library(tidyverse)
  options(scipen = 999)
  
  load("Data_coast_angle/OISST_upwell_base.RData")
  load("Data_coast_angle/CMC_upwell_base.RData")
  load("Data_coast_angle/SACTN_upwell_base.RData")
  load("Data_coast_angle/MUR_upwell_base.RData")
  load("Data_coast_angle/G1SST_upwell_base.RData")

# # Removing the distance of 20 and 40kms
# library(dplyr)
# removing_distance_func <- function(df){
#   removing_dist_func<- df %>% 
#     filter(!distance %in% c(20000,40000))
# }
# 
# OISST_final <- removing_distance_func(df = OISST_upwell_base)
# G1SST_final <- removing_distance_func(df = G1SST_upwell_base)
# MUR_final <- removing_distance_func(df = MUR_upwell_base)
# CMC_final <- removing_distance_func(df = CMC_upwell_base)

# load("Data/OISST_final.RData")
# OISST_final$distance_km <- OISST_final$distance/1000 # This column is missing from this dataframe
# load("Data/G1SST_final.RData")
# #load("Data/SACTN_upwell_base.RData")
# load("Data/MUR_final.RData")
# load("Data/CMC_final.RData")

combined_products <- rbind(OISST_upwell_base,CMC_upwell_base,MUR_upwell_base,G1SST_upwell_base)

metric_4years <- combined_products %>% 
  filter(year(date_start) %in% 2011:2014) # only for the years 2011-2014 so 4 year period


metrics <- metric_4years %>% 
  # mutate(year = year(date_start)) %>% # Why is this here? It is removed in the summarise step.
  group_by(product, distance) %>% 
  summarise(y = n(),
            mean_intensity = mean(intensity_mean),
            mean_dur = mean(duration),
            mean_cumIn = mean(intensity_cumulative)) %>% 
  rename(count = y)

# Function for extracting slope from linear model
lm_coeff <- function(df){
  res <- lm(formula = val ~ date_peak, data = df)
  res_coeff <- as.numeric(res$coefficient[2])
}

# Calculate all of the linear models
lm_metrics <- metric_4years %>% 
  ungroup() %>% 
  dplyr::select(-heading, -c(lon:index_end)) %>% 
  pivot_longer(cols = c(duration, intensity_mean:rate_decline), 
               names_to = "var", values_to = "val") %>% 
  group_by(site, product, distance, var) %>% 
  nest() %>% 
  mutate(slope = purrr::map(data, lm_coeff)) %>% 
  dplyr::select(-data) %>% 
  unnest(cols = slope) %>% 
  # convert from daily to decadal values
  mutate(slope = round((slope*365.25*10), 2)) %>% 
  ungroup()

# Cast the results wide for easier use with ANOVA
lm_metrics_wide <- pivot_wider(lm_metrics, 
                               id_cols = site:distance, 
                               names_from = var, values_from = slope)

# # 3-way ANOVA tests to compare the upwelling metrics such as distance, cummulative intensity and intensity against the different sites, products and distances
# summary(aov(duration ~ site + product + distance , data = metric_4years))
# summary(aov(intensity_mean ~ site + product + distance, data = metric_4years))
# # summary(aov(intensity_max ~ site + product + distance, data = metric_4years))
# summary(aov(intensity_cumulative ~ site + product + distance, data = metric_4years))
# 
# ## Interaction?     
# summary(aov(duration ~ site * product * distance , data = metric_4years))
# summary(aov(intensity_mean ~ site * product * distance, data = metric_4years))
# # summary(aov(intensity_max ~ site + product + distance, data = metric_4years))
# summary(aov(intensity_cumulative ~ site * product * distance, data = metric_4years))



# A) DIFFERENCES BETWEEN SITES, PER PRODUCT -------------------------------

# First, we see, for each product, if there are differences between sites
# We use the measurements taken at different distances from the shore as the replicates

# names of sat products:
# unique(metric_4years$product)
# "OISST" "CMC"   "MUR"   "G1SST"

# H0: For OISST, there is no significant effect caused by between-site differences:
summary(aov(duration ~ site, data = metric_4years[metric_4years$product == "OISST", ]))
# see MS Word doc for ANOVA table and figure...

ggplot(data = metric_4years, aes(x = site, y = duration)) +
  geom_boxplot() +
  facet_wrap(vars(product), ncol = 2) +
  xlab("Site") + ylab("Duration (days)") +
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

# H0: For CMC, there is no significant effect caused by between-site differences:
summary(aov(duration ~ site, data = metric_4years[metric_4years$product == "CMC", ]))
# you need to make your own ANOVA table, but the figure made above covers this one too...

# H0: For MUR, there is no significant effect caused by between-site differences:
summary(aov(duration ~ site, data = metric_4years[metric_4years$product == "MUR", ]))
# you need to make your own ANOVA table, but the figure made above covers this one too...

# H0: For G1SST, there is no significant effect caused by between-site differences:
summary(aov(duration ~ site, data = metric_4years[metric_4years$product == "G1SST", ]))
# you need to make your own ANOVA table, but the figure made above covers this one too...

##### YOU NEED TO DO THIS FOR THE OTHER UPWELLING METRICS

summary(aov(intensity_mean ~ site, data = metric_4years[metric_4years$product == "OISST", ]))
# see MS Word doc for ANOVA table and figure...

ggplot(data = metric_4years, aes(x = site, y = intensity_mean)) +
  geom_boxplot() +
  facet_wrap(vars(product), ncol = 2) +
  xlab("Site") + ylab("Mean intensity (°C)") +
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


# H0: For CMC, there is no significant effect caused by between-site differences:
summary(aov(intensity_mean ~ site, data = metric_4years[metric_4years$product == "CMC", ]))
# you need to make your own ANOVA table, but the figure made above covers this one too...

# H0: For MUR, there is no significant effect caused by between-site differences:
summary(aov(intensity_mean ~ site, data = metric_4years[metric_4years$product == "MUR", ]))
# you need to make your own ANOVA table, but the figure made above covers this one too...

# H0: For G1SST, there is no significant effect caused by between-site differences:
summary(aov(intensity_mean ~ site, data = metric_4years[metric_4years$product == "G1SST", ]))
# you need to make your own ANOVA table, but the figure made above covers this one too...

# CUmulative intensity

summary(aov(intensity_cumulative ~ site, data = metric_4years[metric_4years$product == "OISST", ]))
# see MS Word doc for ANOVA table and figure...

ggplot(data = metric_4years, aes(x = site, y = intensity_cumulative)) +
  geom_boxplot() +
  facet_wrap(vars(product), ncol = 2) +
  xlab("Site") + ylab("Cumulative intensity (°C.days)") +
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


# H0: For CMC, there is no significant effect caused by between-site differences:
summary(aov(intensity_cumulative ~ site, data = metric_4years[metric_4years$product == "CMC", ]))
# you need to make your own ANOVA table, but the figure made above covers this one too...

# H0: For MUR, there is no significant effect caused by between-site differences:
summary(aov(intensity_cumulative ~ site, data = metric_4years[metric_4years$product == "MUR", ]))
# you need to make your own ANOVA table, but the figure made above covers this one too...

# H0: For G1SST, there is no significant effect caused by between-site differences:
summary(aov(intensity_cumulative ~ site, data = metric_4years[metric_4years$product == "G1SST", ]))
# you need to make your own ANOVA table, but the figure made above covers this one too...


# B) DIFFERENCES BETWEEN DISTANCES, PER PRODUCT ---------------------------

# Second, we see, for each product, of there are differences between distances
# We use the measurements taken at different sites as the replicates

# names of sat products:
# unique(metric_4years$product)
# "OISST" "CMC"   "MUR"   "G1SST"

# H0: For OISST, there is no significant effect caused by between-distance differences:
summary(aov(duration ~ distance, data = metric_4years[metric_4years$product == "OISST", ]))
# use above example and make your own ANOVA table, and include the figure below...

metric_4years <- metric_4years %>% 
  mutate(distance = case_when(distance == "25000" ~ "25",
                              distance == "50000" ~ "50",
                              distance == "0" ~ "0",))


ggplot(data = metric_4years, aes(x = as.factor(distance), y = duration)) +
  geom_boxplot() +
  facet_wrap(vars(product), ncol = 2) +
  xlab("Distance from shore (m)") + ylab("Duration (days)") +
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

  # H0: For CMC, there is no significant effect caused by between-distance differences:
summary(aov(duration ~ distance, data = metric_4years[metric_4years$product == "CMC", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For MUR, there is no significant effect caused by between-distance differences:
summary(aov(duration ~ distance, data = metric_4years[metric_4years$product == "MUR", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For G1SST, there is no significant effect caused by between-distance differences:
summary(aov(duration ~ distance, data = metric_4years[metric_4years$product == "G1SST", ]))
# use above example and make your own ANOVA table, and include the figure above...

##### YOU NEED TO DO THIS FOR THE OTHER UPWELLING METRICS


# H0: For OISST, there is no significant effect caused by between-distance differences:
summary(aov(intensity_mean ~ distance, data = metric_4years[metric_4years$product == "OISST", ]))

# use above example and make your own ANOVA table, and include the figure below...


# H0: For CMC, there is no significant effect caused by between-distance differences:
summary(aov(intensity_mean ~ distance, data = metric_4years[metric_4years$product == "CMC", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For MUR, there is no significant effect caused by between-distance differences:
summary(aov(intensity_mean ~ distance, data = metric_4years[metric_4years$product == "MUR", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For G1SST, there is no significant effect caused by between-distance differences:
summary(aov(intensity_mean ~ distance, data = metric_4years[metric_4years$product == "G1SST", ]))
# use above example and make your own ANOVA table, and include the figure above...

metric_4years <- metric_4years %>% 
  mutate(distance = case_when(distance == "25000" ~ "25",
                              distance == "50000" ~ "50",
                              distance == "0" ~ "0",))

ggplot(data = metric_4years, aes(x = as.factor(distance), y = intensity_mean)) +
  geom_boxplot() +
  facet_wrap(vars(product), ncol = 2) +
  xlab("Distance from shore (m)") + ylab("Mean intensity (°C)") +
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

###################3

# H0: For OISST, there is no significant effect caused by between-distance differences:
summary(aov(intensity_cumulative ~ distance, data = metric_4years[metric_4years$product == "OISST", ]))

# H0: For CMC, there is no significant effect caused by between-distance differences:
summary(aov(intensity_cumulative ~ distance, data = metric_4years[metric_4years$product == "CMC", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For MUR, there is no significant effect caused by between-distance differences:
summary(aov(intensity_cumulative ~ distance, data = metric_4years[metric_4years$product == "MUR", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For G1SST, there is no significant effect caused by between-distance differences:
summary(aov(intensity_cumulative ~ distance, data = metric_4years[metric_4years$product == "G1SST", ]))
# use above example and make your own ANOVA table, and include the figure above..

# use above example and make your own ANOVA table, and include the figure below...

metric_4years <- metric_4years %>% 
  mutate(distance = case_when(distance == "25000" ~ "25",
                              distance == "50000" ~ "50",
                              distance == "0" ~ "0",))
  

ggplot(data = metric_4years, aes(x = as.factor(distance), y = intensity_cumulative)) +
  geom_boxplot() +
  facet_wrap(vars(product), ncol = 2) +
  xlab("Distance from shore (m)") + ylab("Cumulative intensity (°C.days)") +
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


# C) ARE THERE DIFFERENCES BETWEEN THE PRODUCTS? --------------------------

# Third, we are interested in the differences between the satellites. The problem is we do
# not have replicates for satellite, so we have to use the spatial structure as the replicates.
# So, we can do one of two things: i) nest distance within site, or ii) nest site within distance.
# It's fine to report only one of them...

# i) nest distance within site
# H0: there are no differences in metrics between products and this does not interact with site
summary(aov(duration ~ product + site/distance, data = metric_4years))

ggplot(data = metric_4years, aes(x = product, y = duration)) +
  geom_boxplot() +
  xlab("Data product") + ylab("Upwelling duration (days)")+
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


summary(aov(intensity_mean ~ product + site/distance, data = metric_4years))

ggplot(data = metric_4years, aes(x = product, y = intensity_mean)) +
  geom_boxplot() +
  xlab("Data product") + ylab("Upwelling mean intensity (°C)")+
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

summary(aov(intensity_cumulative ~ product + site/distance, data = metric_4years))

ggplot(data = metric_4years, aes(x = product, y = intensity_cumulative)) +
  geom_boxplot() +
  xlab("Data product") + ylab("Upwelling umulative intensity (°C.days)")+
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

##### YOU NEED TO DO THIS FOR THE OTHER UPWELLING METRICS

# # This one isn't necessary...
# # i) site distance within distance
# # H0: there are no differences in metrics between products and this does not interact with site
# summary(aov(duration ~ product + distance/site, data = metric_4years))
# 
# ggplot(data = metric_4years, aes(x = product, y = duration, colour = as.factor(distance))) +
#   geom_boxplot(aes(colour = as.factor(distance)))





# AJ suggested ANOVA
# Here we only compare the 4 satellite analyses as the 5th data product is the SACTN data. The SACTN data does not have the distance variable

summary(aov(duration ~ site  , data = metric_4years))
summary(aov(intensity_mean ~ site  , data = metric_4years))
summary(aov(intensity_cumulative ~ site  , data = metric_4years))


summary(aov(duration ~ product +distance/site , data = metric_4years))
summary(aov(intensity_mean ~ product +distance/site  , data = metric_4years))
summary(aov(intensity_cumulative ~ product +distance/site , data = metric_4years))


summary(aov(duration ~ distance  , data = metric_4years))
summary(aov(intensity_mean ~ distance  , data = metric_4years))
summary(aov(intensity_cumulative ~ distance  , data = metric_4years))

# Here are ANOVAs on the linear model results
# NB: This is not advisable as these linear models are based on only 4 years of data.
# That is not enough to draw any conclusions about rates of change over time
# I suppose the arguments could be made that one is not measuring lon term change, 
# but rather than the data products should show reasonable agreement for events detected
# within any gicen year.
summary(aov(duration ~ site + product + distance, data = lm_metrics_wide))
summary(aov(intensity_mean ~ site + product + distance, data = lm_metrics_wide))
summary(aov(intensity_max ~ site + product + distance, data = lm_metrics_wide))
summary(aov(intensity_cumulative ~ site + product + distance, data = lm_metrics_wide))

### ANOVA suggested by AJ to compare the differences in number of signals detected between sites
metric_ANOVA <- combined_products %>% 
  filter(year(date_start) %in% 2011:2014) %>%  # only for the years 2011-2014 so 4 year period
  ungroup() %>%
  group_by(product, site, distance) %>% 
  summarise(y = n()) %>% 
  rename(count = y) %>% 
  mutate(distance = case_when(distance == "25000" ~ "25",
                                distance == "50000" ~ "50",
                              distance == "0" ~ "0",))


# save(metric_ANOVA, file = "Data/metric_ANOVA.RData")

summary(aov(count ~ site, data = metric_ANOVA))
summary(aov(count ~ product + distance/site, data = metric_ANOVA))
summary(aov(count ~ distance, data = metric_ANOVA))

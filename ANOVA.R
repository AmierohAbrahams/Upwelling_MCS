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
  load("Data_coast_angle/CMC_2015_upwell_base.RData")
  load("Data_coast_angle/OISST_2015_upwell_base.RData")
  load("Data_coast_angle/MUR_upwell_base_2015.RData")
  CMC_upwell_base <- rbind(CMC_2015_upwell_base, CMC_upwell_base)
  OISST_upwell_base <- rbind(OISST_2015_upwell_base, OISST_upwell_base)
  MUR_upwell_base <- rbind(MUR_upwell_base, MUR_upwell_base_2015)
  
# # Removing the distance of 20 and 40kms
# library(dplyr)
# removing_distance_func <- function(df){
#   removing_dist_func<- df %>% 
#     filter(!distance %in% c(20000,40000))gc
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

  
G1SST_upwell_base$distance <- as.numeric(G1SST_upwell_base$distance)
MUR_upwell_base_2015$distance <- as.numeric(MUR_upwell_base_2015$distance)

combined_products <- rbind(OISST_upwell_base,CMC_upwell_base,MUR_upwell_base_2015,G1SST_upwell_base)

metric_4years <- combined_products %>% 
  filter(year(date_start) %in% 2011:2016) # only for the years 2011-2014 so 4 year period


metrics <- metric_4years %>% 
  # mutate(year = year(date_start)) %>% # Why is this here? It is removed in the summarise step.
  group_by(product, distance, site) %>% 
  summarise(y = n(),
            mean_intensity = mean(intensity_mean),
            mean_dur = mean(duration),
            mean_cumIn = mean(intensity_cumulative)) %>% 
  rename(count = y) 

metrics <- metrics%>% 
  ungroup(distance) %>% 
  mutate(distance = case_when(distance == "25000" ~ "25",
                              distance == "50000" ~ "50",
                              distance == "0" ~ "0",))

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
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# ANOVA WITH SACTN
# A) DIFFERENCES BETWEEN SITES, PER PRODUCT -------------------------------
load("Data_coast_angle/OISST_upwell_base.RData")
load("Data_coast_angle/CMC_upwell_base.RData")
load("Data_coast_angle/SACTN_upwell_base.RData")
load("Data_coast_angle/MUR_upwell_base.RData")
load("Data_coast_angle/G1SST_upwell_base.RData")

MUR_upwell_base <- MUR_upwell_base_2015 %>% 
  mutate(distance = as.numeric(as.character(distance)))

G1SST_upwell_base_test <- G1SST_upwell_base %>% 
  mutate(distance = as.numeric(as.character(distance))) %>% 
  arrange(desc(duration))

G1SST_upwell_base_test = G1SST_upwell_base_test[-1,] #press 3 times
G1SST_upwell_base <- G1SST_upwell_base_test

combined_products <- rbind(OISST_upwell_base,CMC_upwell_base,MUR_upwell_base,G1SST_upwell_base)
# save(combined_products, file = "Data_coast_angle/combined_products.RData")
# Calculating the number of upwelling signals at the different sites for the different products

number_products <- SACTN_upwell_events %>% 
  filter(site == "Saldanha Bay")
# 2: Preparing box plot data ---------------------------------------------------------------------------------------------------------------------------------

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
load("~/Documents/Upwelling_MCS/SACTN_upwell_events.RData")
SACTN <- seasons_func(df = SACTN_upwell_events)

metrics <- combined_products %>% 
  # mutate(year = year(date_start)) %>% # Why is this here? It is removed in the summarise step.
  group_by(product, year, site) %>% 
  summarise(y = n(),
            mean_intensity = mean(intensity_mean),
            mean_dur = mean(duration),
            mean_cumIn = mean(intensity_cumulative)) %>% 
  rename(count = y)

metric_prods <- combined_products %>% 
  #filter(year(date_start) %in% 2011:2016) %>% 
  filter(season == "Summer") %>% 
  ungroup() %>% 
  dplyr::select(-heading,-distance)
metric_prods <- as.data.frame(metric_prods)

metric_SACTN <- SACTN %>% 
  #filter(year(date_start) %in% 2011:2016) %>% 
  mutate(product = "SACTN")
  #filter(season == "Summer")
metric_SACTN <- as.data.frame(metric_SACTN)

final <- rbind(metric_SACTN,metric_prods)

summary(aov(duration ~ site, data = final[final$product == "OISST", ]))
summary(aov(duration ~ site, data = final[final$product == "CMC", ]))
summary(aov(duration ~ site, data = final[final$product == "G1SST", ]))
summary(aov(duration ~ site, data = final[final$product == "MUR", ]))
summary(aov(duration ~ site, data = final[final$product == "SACTN", ]))

final$product = factor(final$product, levels = c("SACTN", "OISST", "CMC", "MUR","G1SST"))
plot1 <- ggplot(data = final, aes(x = site, y = duration)) +
  geom_boxplot(notch=TRUE) +
  facet_wrap(vars(product), ncol = 5) +
 # scale_x_discrete(limits = Ordering) +
  xlab("") + ylab("Duration (days)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(angle = 90),
    strip.placement = "outside",
    axis.text = element_text(size = 9, colour = "black", family = "Arial"),
    axis.title = element_text(size = 10, face = "bold", family = "Arial"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 8, family = "Arial"),
    legend.position = "right",
    legend.text = element_text(size = 7, family = "Arial"),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())
  # theme_set(theme_grey()) +
  # theme_grey() +
  # theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
  #   panel.grid.major = element_line(size = 0.2, linetype = 2),
  #   panel.grid.minor = element_line(colour = NA),
  #   strip.text = element_text(size=14, family = "Palatino"),
  #   axis.title = element_text(size = 15, face = "bold", family = "Palatino"),
  #   axis.ticks.length = unit(0.4, "cm"),
  #   axis.text = element_text(size = 10, colour = "black", family = "Palatino"),
  #   plot.title = element_text(size = 18, hjust = 0),
  #   legend.title = element_text(size = 18),
  #   legend.text = element_text(size = 12),
  #   legend.key = element_rect(size = 0.8, colour = NA),
  #   legend.background = element_blank())

summary(aov(intensity_mean ~ site, data = final[final$product == "OISST", ]))
summary(aov(intensity_mean ~ site, data = final[final$product == "CMC", ]))
summary(aov(intensity_mean ~ site, data = final[final$product == "G1SST", ]))
summary(aov(intensity_mean ~ site, data = final[final$product == "MUR", ]))
summary(aov(intensity_mean ~ site, data = final[final$product == "SACTN", ]))

plot2 <- ggplot(data = final, aes(x = site, y = intensity_mean)) +
  geom_boxplot(notch=TRUE) +
  #scale_x_discrete(limits = Ordering) +
  facet_wrap(vars(product), ncol = 5) +
  xlab("") + ylab("Mean intensity (°C)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(angle = 90),
    strip.placement = "outside",
    axis.text = element_text(size = 9, colour = "black", family = "Arial"),
    axis.title = element_text(size = 10, face = "bold", family = "Arial"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 8, family = "Arial"),
    legend.position = "right",
    legend.text = element_text(size = 7, family = "Arial"),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())
  # theme_set(theme_grey()) +
  # theme_grey() +
  # theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
  #   panel.grid.major = element_line(size = 0.2, linetype = 2),
  #   panel.grid.minor = element_line(colour = NA),
  #   strip.text = element_text(size=14, family = "Palatino"),
  #   axis.title = element_text(size = 15, face = "bold", family = "Palatino"),
  #   axis.ticks.length = unit(0.4, "cm"),
  #   axis.text = element_text(size = 10, colour = "black", family = "Palatino"),
  #   plot.title = element_text(size = 18, hjust = 0),
  #   legend.title = element_text(size = 18),
  #   legend.text = element_text(size = 1),
  #   legend.key = element_rect(size = 0.8, colour = NA),
  #   legend.background = element_blank())

(combined_plt1 <- ggarrange(plot1, plot2, plot3))

summary(aov(intensity_cumulative ~ site, data = final[final$product == "OISST", ]))
summary(aov(intensity_cumulative ~ site, data = final[final$product == "CMC", ]))
summary(aov(intensity_cumulative ~ site, data = final[final$product == "G1SST", ]))
summary(aov(intensity_cumulative ~ site, data = final[final$product == "MUR", ]))
summary(aov(intensity_cumulative ~ site, data = final[final$product == "SACTN", ]))

    plot3 <- ggplot(data = final, aes(x = site, y = intensity_cumulative)) +
  geom_boxplot(notch=TRUE) +
  facet_wrap(vars(product), ncol = 5) +
  #scale_x_discrete(limits = Ordering) +
  xlab("") + ylab("Cumulative intensity
  (°C.days)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(angle = 90),
    strip.placement = "outside",
    axis.text = element_text(size = 9, colour = "black", family = "Arial"),
    axis.title = element_text(size = 10, face = "bold", family = "Arial"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 8, family = "Arial"),
    legend.position = "right",
    legend.text = element_text(size = 7, family = "Arial"),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())
  # theme_set(theme_grey()) +
  # theme_grey() +
  # theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
  #   panel.grid.major = element_line(size = 0.2, linetype = 2),
  #   panel.grid.minor = element_line(colour = NA),
  #   strip.text = element_text(size=14, family = "Palatino"),
  #   axis.title = element_text(size = 15, face = "bold", family = "Palatino"),
  #   axis.ticks.length = unit(0.4, "cm"),
  #   axis.text = element_text(size = 10, colour = "black", family = "Palatino"),
  #   plot.title = element_text(size = 18, hjust = 0),
  #   legend.title = element_text(size = 18),
  #   legend.text = element_text(size = 16),
  #   legend.key = element_rect(size = 0.8, colour = NA),
  #   legend.background = element_blank())

Fig2 <- ggarrange(plot1, plot2, plot3, nrow = 3, ncol = 1, labels = c("A.", "B.", "C."))
ggsave(filename = "combined_plot1.jpg", plot = combined_plot1, width=180, height = 200, units = "mm",dpi = 300,  path = "figures/")
ggsave("Fig2.tiff", units="cm", width=19.05, height=22.23, dpi=600)

# # A) DIFFERENCES BETWEEN SITES, PER PRODUCT -------------------------------
# 
# # First, we see, for each product, if there are differences between sites
# # We use the measurements taken at different distances from the shore as the replicates
# 
# # names of sat products:
# # unique(metric_4years$product)
# # "OISST" "CMC"   "MUR"   "G1SST"   

# metric_prods <- combined_products %>% 
#   filter(year(date_start) %in% 2011:2014) %>% 
#   filter(season == "Summer") 
# 
# # H0: For OISST, there is no significant effect caused by between-site differences:
# summary(aov(duration ~ site, data = metric_prods[metric_prods$product == "OISST", ]))
# # see MS Word doc for ANOVA table and figure...

# plot4 <- ggplot(data = metric_prods, aes(x = product, y = duration)) +
#   geom_boxplot() +
#   facet_wrap(vars(site), ncol =4 ) +
#   xlab("SST product") + ylab("Duration (days)") +
#   theme_set(theme_grey()) +
#   theme_grey() +
#   theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
#     panel.grid.major = element_line(size = 0.2, linetype = 2),
#     panel.grid.minor = element_line(colour = NA),
#     strip.text = element_text(size=14, family = "Palatino"),
#     axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
#     axis.ticks.length = unit(0.4, "cm"),
#     axis.text = element_text(size = 10, colour = "black", family = "Palatino"),
#     plot.title = element_text(size = 18, hjust = 0),
#     legend.title = element_text(size = 18),
#     legend.text = element_text(size = 16),
#     legend.key = element_rect(size = 0.8, colour = NA),
#     legend.background = element_blank())
S
# # H0: For CMC, there is no significant effect caused by between-site differences:
# summary(aov(duration ~ site, data = metric_prods[metric_prods$product == "CMC", ]))
# # you need to make your own ANOVA table, but the figure made above covers this one too...
# 
# # H0: For MUR, there is no significant effect caused by between-site differences:
# summary(aov(duration ~ site, data = metric_prods[metric_prods$product == "MUR", ]))
# # you need to make your own ANOVA table, but the figure made above covers this one too...
# 
# # H0: For G1SST, there is no significant effect caused by between-site differences:
# summary(aov(duration ~ site, data = metric_prods[metric_prods$product == "G1SST", ]))
# # you need to make your own ANOVA table, but the figure made above covers this one too...
# 
# ##### YOU NEED TO DO THIS FOR THE OTHER UPWELLING METRICS
# 
# summary(aov(intensity_mean ~ site, data = metric_prods[metric_prods$product == "OISST", ]))
# # see MS Word doc for ANOVA table and figure...
# 
# plot5 <- ggplot(data = metric_prods, aes(x = product, y = intensity_mean)) +
#   geom_boxplot() +
#   facet_wrap(vars(site), ncol = 2) +
#   xlab("SST product") + ylab("Mean intensity (°C)") +
#   theme_grey() +
#   theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
#     panel.grid.major = element_line(size = 0.2, linetype = 2),
#     panel.grid.minor = element_line(colour = NA),
#     strip.text = element_text(size=14, family = "Palatino"),
#     axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
#     axis.ticks.length = unit(0.4, "cm"),
#     axis.text = element_text(size = 10, colour = "black", family = "Palatino"),
#     plot.title = element_text(size = 18, hjust = 0),
#     legend.title = element_text(size = 18),
#     legend.text = element_text(size = 16),
#     legend.key = element_rect(size = 0.8, colour = NA),
#     legend.background = element_blank())
# 
# (combined_plt2 <- ggarrange(plot4, plot5))
# 
# # H0: For CMC, there is no significant effect caused by between-site differences:
# summary(aov(intensity_mean ~ site, data = metric_prods[metric_prods$product == "CMC", ]))
# # you need to make your own ANOVA table, but the figure made above covers this one too...
# 
# # H0: For MUR, there is no significant effect caused by between-site differences:
# summary(aov(intensity_mean ~ site, data = metric_prods[metric_prods$product == "MUR", ]))
# # you need to make your own ANOVA table, but the figure made above covers this one too...
# 
# # H0: For G1SST, there is no significant effect caused by between-site differences:
# summary(aov(intensity_mean ~ site, data = metric_prods[metric_prods$product == "G1SST", ]))
# # you need to make your own ANOVA table, but the figure made above covers this one too...
# 
# # CUmulative intensity
# 
# summary(aov(intensity_cumulative ~ site, data = metric_prods[metric_prods$product == "OISST", ]))
# # see MS Word doc for ANOVA table and figure...
# 
# ggplot(data = metric_prods, aes(x = site, y = intensity_cumulative)) +
#   geom_boxplot() +
#   facet_wrap(vars(product), ncol = 2) +
#   xlab("Site") + ylab("Cumulative intensity (°C.days)") +
#   theme_grey() +
#   theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
#     panel.grid.major = element_line(size = 0.2, linetype = 2),
#     panel.grid.minor = element_line(colour = NA),
#     strip.text = element_text(size=14, family = "Palatino"),
#     axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
#     axis.ticks.length = unit(0.4, "cm"),
#     axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
#     plot.title = element_text(size = 18, hjust = 0),
#     legend.title = element_text(size = 18),
#     legend.text = element_text(size = 16),
#     legend.key = element_rect(size = 0.8, colour = NA),
#     legend.background = element_blank())
# 
# 
# # H0: For CMC, there is no significant effect caused by between-site differences:
# summary(aov(intensity_cumulative ~ site, data = metric_prods[metric_prods$product == "CMC", ]))
# # you need to make your own ANOVA table, but the figure made above covers this one too...
# 
# # H0: For MUR, there is no significant effect caused by between-site differences:
# summary(aov(intensity_cumulative ~ site, data = metric_prods[metric_prods$product == "MUR", ]))
# # you need to make your own ANOVA table, but the figure made above covers this one too...
# 
# # H0: For G1SST, there is no significant effect caused by between-site differences:
# summary(aov(intensity_cumulative ~ site, data = metric_prods[metric_prods$product == "G1SST", ]))
# # you need to make your own ANOVA table, but the figure made above covers this one too...


# B) DIFFERENCES BETWEEN DISTANCES, PER PRODUCT --------------------------------------------------------------------------------------------------------------------

# Second, we see, for each product, of there are differences between distances
# We use the measurements taken at different sites as the replicates

# names of sat products:
# unique(metric_prods$product)
# "OISST" "CMC"   "MUR"   "G1SST"

metric_prods <- combined_products %>% 
 # filter(year(date_start) %in% 2011:2016) %>% 
  filter(season == "Summer") 

metric_prods <- metric_prods %>% 
  mutate(distance = case_when(distance == "25000" ~ "25",
                              distance == "50000" ~ "50",
                              distance == "0" ~ "0"))
metric_prods$product <- as.factor(metric_prods$product)

metric_prods$product = factor(metric_prods$product, levels = c("OISST", "CMC", "MUR", "G1SST")) #Re ordering

# metric_prods$product <- factor(metric_prods$product,levels=c( "OISST", "CMC", "G1SST", "OISST"))
# H0: For OISST, there is no significant effect caused by between-distance differences:
summary(aov(duration ~ distance, data = metric_prods[metric_prods$product == "OISST", ]))
# use above example and make your own ANOVA table, and include the figure below...

plotA <- ggplot(data = metric_prods, aes(x = as.factor(distance), y = duration)) +
  geom_boxplot(notch = TRUE) +
  facet_wrap(vars(product), ncol = 4) +
  xlab("") + ylab("Duration (days)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, family = "Arial"),
    strip.placement = "outside",
    axis.text = element_text(size = 9, colour = "black", family = "Arial"),
    axis.title = element_text(size = 10, face = "bold", family = "Arial"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 8, family = "Arial"),
    legend.position = "right",
    legend.text = element_text(size = 7, family = "Arial"),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())
  # theme_grey() +
  # theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
  #   panel.grid.major = element_line(size = 0.2, linetype = 2),
  #   panel.grid.minor = element_line(colour = NA),
  #   strip.text = element_text(size=14, family = "Palatino"),
  #   axis.title = element_text(size = 15, face = "bold", family = "Palatino"),
  #   axis.ticks.length = unit(0.4, "cm"),
  #   axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
  #   plot.title = element_text(size = 18, hjust = 0),
  #   legend.title = element_text(size = 18),
  #   legend.text = element_text(size = 16),
  #   legend.key = element_rect(size = 0.8, colour = NA),
  #   legend.background = element_blank())

# H0: For CMC, there is no significant effect caused by between-distance differences:
summary(aov(duration ~ distance, data = metric_prods[metric_prods$product == "CMC", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For MUR, there is no significant effect caused by between-distance differences:
summary(aov(duration ~ distance, data = metric_prods[metric_prods$product == "MUR", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For G1SST, there is no significant effect caused by between-distance differences:
summary(aov(duration ~ distance, data = metric_prods[metric_prods$product == "G1SST", ]))
# use above example and make your own ANOVA table, and include the figure above...

##### YOU NEED TO DO THIS FOR THE OTHER UPWELLING METRICS
# H0: For OISST, there is no significant effect caused by between-distance differences:
summary(aov(intensity_mean ~ distance, data = metric_prods[metric_prods$product == "OISST", ]))

# use above example and make your own ANOVA table, and include the figure below...

# H0: For CMC, there is no significant effect caused by between-distance differences:
summary(aov(intensity_mean ~ distance, data = metric_prods[metric_prods$product == "CMC", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For MUR, there is no significant effect caused by between-distance differences:
summary(aov(intensity_mean ~ distance, data = metric_prods[metric_prods$product == "MUR", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For G1SST, there is no significant effect caused by between-distance differences:
summary(aov(intensity_mean ~ distance, data = metric_prods[metric_prods$product == "G1SST", ]))
# use above example and make your own ANOVA table, and include the figure above...

plotB <- ggplot(data = metric_prods, aes(x = as.factor(distance), y = intensity_mean)) +
  geom_boxplot(notch = TRUE) +
  facet_wrap(vars(product), ncol = 4) +
  xlab("") + ylab("Mean intensity (°C)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, family = "Arial"),
    strip.placement = "outside",
    axis.text = element_text(size = 9, colour = "black", family = "Arial"),
    axis.title = element_text(size = 10, face = "bold", family = "Arial"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 8, family = "Arial"),
    legend.position = "right",
    legend.text = element_text(size = 7, family = "Arial"),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())
  # theme_grey() +
  # theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
  #   panel.grid.major = element_line(size = 0.2, linetype = 2),
  #   panel.grid.minor = element_line(colour = NA),
  #   strip.text = element_text(size=14, family = "Palatino"),
  #   axis.title = element_text(size = 15, face = "bold", family = "Palatino"),
  #   axis.ticks.length = unit(0.4, "cm"),
  #   axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
  #   plot.title = element_text(size = 18, hjust = 0),
  #   legend.title = element_text(size = 18),
  #   legend.text = element_text(size = 16),
  #   legend.key = element_rect(size = 0.8, colour = NA),
  #   legend.background = element_blank())

###################

# H0: For OISST, there is no significant effect caused by between-distance differences:
summary(aov(intensity_cumulative ~ distance, data = metric_prods[metric_prods$product == "OISST", ]))

# H0: For CMC, there is no significant effect caused by between-distance differences:
summary(aov(intensity_cumulative ~ distance, data = metric_prods[metric_prods$product == "CMC", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For MUR, there is no significant effect caused by between-distance differences:
summary(aov(intensity_cumulative ~ distance, data = metric_prods[metric_prods$product == "MUR", ]))
# use above example and make your own ANOVA table, and include the figure above...

# H0: For G1SST, there is no significant effect caused by between-distance differences:
summary(aov(intensity_cumulative ~ distance, data = metric_prods[metric_prods$product == "G1SST", ]))
# use above example and make your own ANOVA table, and include the figure above..

# use above example and make your own ANOVA table, and include the figure below...

plotC <- ggplot(data = metric_prods, aes(x = as.factor(distance), y = intensity_cumulative)) +
  geom_boxplot(notch = TRUE) +
  facet_wrap(vars(product), ncol = 4) +
  xlab("") + ylab("Cumulative intensity 
(°C.days)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, family = "Arial"),
    strip.placement = "outside",
    axis.text = element_text(size = 9, colour = "black", family = "Arial"),
    axis.title = element_text(size = 10, face = "bold", family = "Arial"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 8, family = "Arial"),
    legend.position = "right",
    legend.text = element_text(size = 7, family = "Arial"),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())
  # theme_grey() +
  # theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
  #   panel.grid.major = element_line(size = 0.2, linetype = 2),
  #   panel.grid.minor = element_line(colour = NA),
  #   strip.text = element_text(size=14, family = "Palatino"),
  #   axis.title = element_text(size = 15, face = "bold", family = "Palatino"),
  #   axis.ticks.length = unit(0.4, "cm"),
  #   axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
  #   plot.title = element_text(size = 18, hjust = 0),
  #   legend.title = element_text(size = 18),
  #   legend.text = element_text(size = 16),
  #   legend.key = element_rect(size = 0.8, colour = NA),
  #   legend.background = element_blank())

Fig3 <- ggarrange(plotA, plotB, plotC, nrow = 3, ncol = 1, labels = c("A.", "B.", "C."))
ggsave(filename = "combined_dis.jpg", plot = combined_dis, width=180, height = 200, units = "mm",dpi = 300,  path = "figures/")
ggsave("Fig3.tiff", units="cm", width=19.05, height=22.23, dpi=600)


  # C) ARE THERE DIFFERENCES BETWEEN THE PRODUCTS? -------------------------------------------------------------------------------------------------------------------

# Third, we are interested in the differences between the satellites. The problem is we do
# not have replicates for satellite, so we have to use the spatial structure as the replicates.
# So, we can do one of two things: i) nest distance within site, or ii) nest site within distance.
# It's fine to report only one of them...

# i) nest distance within site
# H0: there are no differences in metrics between products and this does not interact with site
summary(aov(duration ~ product + site/distance, data = metric_prods))

plot6 <- ggplot(data = metric_prods, aes(x = product, y = duration)) +
  geom_boxplot() +
  xlab("Data product") + ylab("Upwelling duration (days)")+
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Arial"),
    axis.title = element_text(size = 18, face = "bold", family = "Arial"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.text = element_text(size = 18, colour = "black", family = "Arial"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

summary(aov(intensity_mean ~ product + site/distance, data = metric_prods))

plot7 <- ggplot(data = metric_prods, aes(x = product, y = intensity_mean)) +
  geom_boxplot() +
  xlab("Data product") + ylab("Upwelling mean intensity (°C)")+
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Arial"),
    axis.title = element_text(size = 18, face = "bold", family = "Arial"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.text = element_text(size = 18, colour = "black", family = "Arial"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

summary(aov(intensity_cumulative ~ product + site/distance, data = metric_prods))

plot8 <- ggplot(data = metric_prods, aes(x = product, y = intensity_cumulative)) +
  geom_boxplot() +
  xlab("Data product") + ylab("Upwelling cumulative intensity (°C.days)")+
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    panel.grid.major = element_line(size = 0.2, linetype = 2),
    panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Arial"),
    axis.title = element_text(size = 18, face = "bold", family = "Arial"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.text = element_text(size = 18, colour = "black", family = "Arial"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

(final_combined <- ggarrange(plot6, plot7, plot8))

##### YOU NEED TO DO THIS FOR THE OTHER UPWELLING METRICS
# # This one isn't necessary...
# # i) site distance within distance
# # H0: there are no differences in metrics between products and this does not interact with site
# summary(aov(duration ~ product + distance/site, data = final))
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
 # filter(year(date_start) %in% 2011:2014) %>%  # only for the years 2011-2014 so 4 year period
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

# Plot the count of SST against temperature and see if changes occured

# OISST_temp_match <- OISST_temp %>% 
#   mutate(year = year(date)) %>% 
#   group_by(year, site) %>% 
#   summarise(mean_temp = mean(temp))
#   
# 
# match_func <- function(df){
#   match <- OISST_temp_match  %>% 
#     left_join(df, by = c("site", "year")) %>% 
#     na.trim()
#   return(match)
# }
# 
# temp <- match_func(df = metrics)
# temp <- temp %>% 
#   filter(year %in% 2011:2016)
# 
# ggplot(data = temp, aes(x = year, y = mean_temp)) +
#   geom_line() + 
#   #geom_smooth(aes(colour = product), method = "lm") +
#   facet_wrap(~site) +
#   labs(x = "Year", y = "Upwelling cumulative intensity")+
#   theme_set(theme_grey()) +
#   theme_grey() +
#   theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
#     # panel.grid.major = element_line(size = 0.2, linetype = 2),
#     # panel.grid.minor = element_line(colour = NA),
#     strip.text = element_text(size=14, family = "Palatino"),
#     axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
#     axis.ticks.length = unit(0.4, "cm"),
#     panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
#     panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
#     axis.text = element_text(size = 18, colour = "black", family = "Palatino"),
#     plot.title = element_text(size = 18, hjust = 0),
#     legend.title = element_text(size = 18, family = "Palatino"),
#     legend.text = element_text(size = 16, family = "Palatino"),
#     legend.key = element_rect(size = 0.8, colour = NA),
#     legend.background = element_blank())

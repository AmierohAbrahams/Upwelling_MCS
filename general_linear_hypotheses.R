## Using a series of general linear hypotheses to look for differences in upwelling signal metrics between datasets and between sites (?)
## Is there a significant difference in the duration/intensity etc between the datasets at a particular site?

library(tidyverse)
library(lubridate)
library(multcomp)
options(scipen = 999)

load("Data/OISST_final.RData")
OISST_final$distance_km <- OISST_final$distance/1000 # This column is missing from this dataframe
load("Data/G1SST_final.RData")
#load("Data/SACTN_upwell_base.RData")
load("Data/MUR_final.RData")
load("Data/CMC_final.RData")

combined_products <- rbind(OISST_final,CMC_final,MUR_final,G1SST_final)

metric_4years <- combined_products %>% 
  filter(year(date_start) %in% 2011:2014) # only for the years 2011-2014 so 4 year period


metrics <- metric_4years %>% 
  # mutate(year = year(date_start)) %>% # Why is this here? It is removed in the summarise step.
  group_by(product, site) %>% 
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
  dplyr::select(-heading, -c(lon:index_end), -distance_km) %>% 
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

### test example
### define coefficients of linear function directly
K <- diag(length(coef(lmod)))[-1,]
rownames(K) <- names(coef(lmod))[-1]
K

### set up general linear hypothesis
summary(glht(lmod, linfct = K))

### Why not just perform a normal ANOVA?
# Why go to all this extra trouble?
# I would recommend reading up more on ANOVA
summary(aov(duration ~ site + product + distance, data = metric_4years))
summary(aov(intensity_mean ~ site + product + distance, data = metric_4years))
summary(aov(intensity_max ~ site + product + distance, data = metric_4years))
summary(aov(intensity_cumulative ~ site + product + distance, data = metric_4years))

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


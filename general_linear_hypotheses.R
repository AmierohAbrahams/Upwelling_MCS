## Using series of general linear hypothese to look for differences in upwelling signal metrics between datasets and between sites (?)
## Is there a significant difference in the duration/intensity etc between the datasets at a particular site

library(tidyverse)
library(lubridate)
library(multcomp)
options(scipen = 999)

load("Data/OISST_final.RData")
load("Data/G1SST_final.RData")
#load("Data/SACTN_upwell_base.RData")
load("Data/MUR_final.RData")
load("Data/CMC_final.RData")

combined_products <- rbind(OISST_final,CMC_final,MUR_final,G1SST_final)

metric_4years <- combined_products %>% 
  filter(year(date_start) %in% 2011:2014) # only for the years 2011-2014 so 4 year period


metrics <- metric_4years %>% 
  mutate(year = year(date_start)) %>% 
  group_by(product, site) %>% 
  summarise(y = n(),
            mean_intensity = mean(intensity_mean),
            mean_dur = mean(duration),
            mean_cumIn = mean(intensity_cumulative)) %>% 
  rename(count = y)

# anova_func <- function(df){
#   currents_aov <- aov(mean_intensity ~ mean_dur  * site, data = df)
#   return(currents_aov)
# }
# anova_metrics <- anova_func(df = metrics)
# summary(anova_metrics)
# 
# anova <- function(df){
#   currents_aov <- aov(mean_intensity ~ mean_dur  * product, data = df)
#   return(currents_aov)
# }
# anova_metrics <- anova(df = metrics)
# summary(anova_metrics)


lm_coeff <- function(df){
  res <- lm(formula = val ~  , data = df)
  res_coeff <- round(as.numeric(res$coefficient[2]), 2)
}

lm_metrics <- metrics %>% 
  gather(key = "var", value = "val", -c(product:site)) %>% 
  group_by(site, var, product) %>% 
  nest() %>% 
  mutate(slope = purrr::map(data, lm_coeff)) %>% 
  select(-data) %>% 
  unnest() 

### define coefficients of linear function directly
K <- diag(length(coef(lm_metrics)))[-1,]
rownames(K) <- names(coef(lm_metrics))[-1]
K

### set up general linear hypothesis
glht(lm_metrics, linfct = K)


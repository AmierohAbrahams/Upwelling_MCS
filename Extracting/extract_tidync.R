library(tidync)
# load("Data/OISST.RData") # 2 decimal places
# OISST <- BC_avhrr_only_v2_Document_Document 
# rm(BC_avhrr_only_v2_Document_Document ); gc()
# load("Data_coast_angle/site_pixels.RData")

temp_dat <- tidync("/home/amieroh/Downloads/data.nc") %>% 
  hyper_tibble() %>% 
  select(lon, lat, u, date) %>% 
  dplyr::rename(temp = analysed_sst) %>% 
  mutate(t ="2014-12-26")

# OISST_prod <- OISST %>%
#   dplyr::select(lon, lat) %>%
#   unique() %>%
#   mutate(product = "OISST")

G1SST_prod <- temp_dat %>%
  dplyr::select(lon, lat) %>%
  unique() %>%
  mutate(product = "G1SST")

sat_data <- rbind(G1SST_prod, OISST_prod) %>% 
  dplyr::select(product, lon, lat)

sat_pixels <- sat_data %>%
  dplyr::select(product, lon, lat) %>%
  unique()

match_func <- function(df){
  df <- df %>%
    dplyr::rename(lon_site = lon, lat_site = lat)
  OISST_index <- OISST_prod[as.vector(knnx.index(as.matrix(OISST_prod[,c("lon", "lat")]),
                                                 as.matrix(df[,c("lon_site", "lat_site")]), k = 1)),] %>%
    cbind(., df)
  G1SST_index <- G1SST_prod[as.vector(knnx.index(as.matrix(G1SST_prod[,c("lon", "lat")]),
                                                 as.matrix(df[,c("lon_site", "lat_site")]), k = 1)),] %>%
    cbind(., df)
  res <- rbind(OISST_index, G1SST_index)
  return(res)
}

pixel_match <- site_pixels %>%
  group_by(site) %>%
  group_modify(~match_func(.x))


extract <- right_join(temp_dat, filter(pixel_match, product == "G1SST"), by = c("lon", "lat"))


G1SST_fill <- extract %>% 
  mutate(temp = temp - 273.15) %>% 
  mutate(date = as.Date(t)) %>% 
  dplyr::select(-t)

# G1SST_official <- Final_G1SST %>% 
#       filter(!distance %in% c(20000,40000)) %>% 
#   mutate(distance = case_when(distance == "10000" ~ "0",
#                               distance == "30000" ~ "25000",
#                               distance == "50000" ~ "50000"))

G1SST_finally <- rbind(G1SST_fill,G1SST_finally)
# G1SST_finally_sub <- rbind(G1SST_fill,G1SST_fill2)
# G1SST_finally <- rbind(G1SST_finally_sub,G1SST_finally)

# save(G1SST_finally, file = "G1SST_finally.RData")
# write_csv(G1SST_finally, path = "G1SST_finally.csv")
# data3 <- arrange(G1SST_finally, date)
# write_csv(data3, path = "data3.csv")

save(G1SST_last, file = "G1SST_last.RData") ##Key distance of 0km
save(G1SST_last, file = "Data_coast_angle/G1SST_last ")

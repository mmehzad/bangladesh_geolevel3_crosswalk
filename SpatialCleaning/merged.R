library(sf)
library(dplyr)
library(tidyr)

load("./dataset/data.RData")
crosswalk <- st_read("./output/crosswalk_bdgeo3_91_11.shp", quiet=TRUE)
micro_data <- data #%>% slice_sample(n=10000)

crosswalk <- crosswalk %>% 
  mutate(across(c("ipum1991", "ipum2001", "ipum2011"), ~ as.numeric(.))) %>%
  rename(geo3_bd1991=ipum1991, geo3_bd2001=ipum2001, geo3_bd2011=ipum2011)

micro_data <- micro_data %>%
  mutate(across(starts_with("geo3_bd"), haven::zap_labels)) %>%
  mutate(across(starts_with("geo3_bd"), as.numeric))

final_data <- left_join(
  micro_data, crosswalk,
  by=c("geo3_bd1991"="geo3_bd1991", "geo3_bd2001"="geo3_bd2001", "geo3_bd2011"="geo3_bd2011")
)

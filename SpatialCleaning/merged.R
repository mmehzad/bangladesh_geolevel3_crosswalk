library(sf)
library(dplyr)
library(tidyr)

library(ggplot2)
library(patchwork)

####################
# 1. Load the Data #
####################

load("./dataset/data.RData")
# im using the crosswalk with original geometry, instead of the geometry of greater region
# one difference from a coding perspective is between "admin_name" in one and "upazilas" in another
crosswalk <- st_read("./output/crosswalk_bdgeo3_91_11_original_geometry.shp", quiet=TRUE)
crosswalk_gr <- st_read("./output/crosswalk_bdgeo3_91_11.shp", quiet=TRUE)

set.seed(42)
micro_data <- data %>% slice_sample(n=10000)

crosswalk <- crosswalk %>% 
  mutate(across(c("merged_id", "ipum1991", "ipum2001", "ipum2011"), ~ as.numeric(.))) %>%
  rename(geo3_bd1991=ipum1991, geo3_bd2001=ipum2001, geo3_bd2011=ipum2011)

micro_data <- micro_data %>%
  mutate(across(starts_with("geo3_bd"), haven::zap_labels)) %>%
  mutate(across(starts_with("geo3_bd"), as.numeric))

rm(data); gc();

final_data <- left_join(
  micro_data, crosswalk,
  by=c("geo3_bd1991"="geo3_bd1991", "geo3_bd2001"="geo3_bd2001", "geo3_bd2011"="geo3_bd2011")
)


############################
# 2. Demonstrate Necessity #
############################

#########################
# 2.1 Distinct Upazilas #
#########################

n_distinct(final_data$geo3_bd1991)
n_distinct(final_data$geo3_bd2001)
n_distinct(final_data$geo3_bd2011)

n_distinct(crosswalk$geo3_bd1991)
n_distinct(crosswalk$geo3_bd2001)
n_distinct(crosswalk$geo3_bd2011)

######################################################
# 2.2 Compare Harmonized and Non-Harmonized Upazilas #
######################################################

upz_id <- "30026095"
# we are assuming that all time variants of
# the same upazila will be in the same greater region
upz_gr_id <- (crosswalk %>% filter(geo3_bd1991==upz_id))$merged_id
upz_admin_name <- (crosswalk %>% filter(geo3_bd1991==upz_id))$admin_name

p22_91 <- ggplot(crosswalk %>% filter(geo3_bd1991==upz_id)) + geom_sf(aes(fill=as.factor(admin_name)))
p22_01 <- ggplot(crosswalk %>% filter(geo3_bd2001==upz_id)) + geom_sf(aes(fill=as.factor(admin_name)))
p22_11 <- ggplot(crosswalk %>% filter(geo3_bd2011==upz_id)) + geom_sf(aes(fill=as.factor(admin_name)))
p22_gr <- ggplot(crosswalk_gr %>% filter(merged_id==upz_gr_id)) + geom_sf(aes(fill=as.factor(upazilas)))

p22_91 / p22_01 / p22_11 / p22_gr

#############################################
# 2.3 Comparison Between Them by Population #
#############################################

pop_noharm_91 <- final_data %>%
  filter(!is.na(geo3_bd1991)) %>%
  group_by(geo3_bd1991) %>%
  summarise(pop = sum(perwt, na.rm=TRUE), .groups="drop") %>%
  left_join(crosswalk %>% select(geo3_bd1991, merged_id, geometry), by="geo3_bd1991") %>%
  mutate(year = 1991)
  
pop_noharm_01 <- final_data %>%
  filter(!is.na(geo3_bd2001)) %>%
  group_by(geo3_bd2001) %>%
  summarise(pop = sum(perwt, na.rm=TRUE), .groups="drop") %>%
  left_join(crosswalk %>% select(geo3_bd2001, merged_id, geometry), by="geo3_bd2001") %>%
  mutate(year = 2001)

pop_noharm_11 <- final_data %>%
  filter(!is.na(geo3_bd2011)) %>%
  group_by(geo3_bd2011) %>%
  summarise(pop = sum(perwt, na.rm=TRUE), .groups="drop") %>%
  left_join(crosswalk %>% select(geo3_bd2011, merged_id, geometry), by="geo3_bd2011") %>%
  mutate(year = 2011)

pop_noharm <- bind_rows(pop_noharm_91, pop_noharm_01, pop_noharm_11) %>%
  filter(geo3_bd1991==upz_id | geo3_bd2001==upz_id | geo3_bd2011==upz_id)

pop_harm <- final_data %>%
  group_by(year, merged_id) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups="drop") %>%
  left_join(crosswalk %>% select(merged_id, geometry), by="merged_id")


p23_noharm <- ggplot(pop_noharm, aes(x = factor(year), y = pop, fill = factor(year))) +
  geom_col() +
  labs(title = paste("Population of Upazila", toupper(upz_admin_name), "over Time"), x = "Year", y = "Population") +
  theme_minimal() +
  scale_fill_brewer(palette="Set2")

p23_harm <- ggplot(pop_harm, aes(x = factor(year), y = pop, fill = factor(year))) +
  geom_col() +
  labs(title = paste("Population of Greated Region ID:", upz_gr_id, "over Time"), x = "Year", y = "Population") +
  theme_minimal() +
  scale_fill_brewer(palette="Set2")

p23_noharm + p23_harm

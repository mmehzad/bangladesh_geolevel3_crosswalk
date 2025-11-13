library(sf)
library(dplyr)
library(tidyr)
library(ggrepel)
library(ggplot2)
library(patchwork)
library(scales)

####################
# 1. Load the Data #
####################

load("C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/data.RData")

micro_data <- data

rm(data)

# im using the crosswalk with original geometry, instead of the geometry of greater region
# one difference from a coding perspective is between "admin_name" in one and "upazilas" in another
crosswalk <- st_read("C:/Users/smshi/Dropbox/Research/bangladesh_geolevel3_crosswalk/SpatialCleaning/output/crosswalk_bdgeo3_91_11_original_geometry.shp", quiet=TRUE)
crosswalk_gr <- st_read("C:/Users/smshi/Dropbox/Research/bangladesh_geolevel3_crosswalk/SpatialCleaning/output/crosswalk_bdgeo3_91_11.shp", quiet=TRUE)


crosswalk <- crosswalk %>% 
  mutate(across(c("merged_id", "ipum1991", "ipum2001", "ipum2011"), ~ as.numeric(.))) %>%
  rename(geo3_bd1991=ipum1991, geo3_bd2001=ipum2001, geo3_bd2011=ipum2011)

crosswalk_gr <- crosswalk_gr %>%
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

final_data_gr <- left_join(
  micro_data, crosswalk_gr,
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
# 2.2a Compare Harmonized and Non-Harmonized Upazilas #
######################################################

p22a_91 <- ggplot(crosswalk %>% filter(!is.na(geo3_bd1991))) +
  geom_sf() +
  ggtitle("Unharmonized Upazilas (1991)")

p22a_01 <- ggplot(crosswalk %>% filter(!is.na(geo3_bd2001))) +
  geom_sf() +
  ggtitle("Unharmonized Upazilas (2001)")

p22a_11 <- ggplot(crosswalk %>% filter(!is.na(geo3_bd2011))) +
  geom_sf() +
  ggtitle("Unharmonized Upazilas (2011)")

p22a_gr <- ggplot(crosswalk_gr) + geom_sf() +
  geom_sf() +
  ggtitle("Harmonized Upazilas")

p22a_91 + p22a_01 + p22a_11 + p22a_gr

# sanity check to see if the values match the upazila count:

# length(unique((crosswalk %>% filter(!is.na(geo3_bd1991)))$geometry))
# length(unique((crosswalk %>% filter(!is.na(geo3_bd2001)))$geometry))
# length(unique((crosswalk %>% filter(!is.na(geo3_bd2011)))$geometry))

# length(unique((crosswalk_gr %>% filter(!is.na(ipum1991)))$geometry))
# length(unique((crosswalk_gr %>% filter(!is.na(ipum2001)))$geometry))
# length(unique((crosswalk_gr %>% filter(!is.na(ipum2011)))$geometry))

######################################################
# 2.2b Compare Harmonized and Non-Harmonized Upazilas #
######################################################

upz_id <- "30026095"
# we are assuming that all time variants of
# the same upazila will be in the same greater region
upz_gr_id <- (crosswalk %>% filter(geo3_bd1991==upz_id))$merged_id
upz_admin_name <- (crosswalk %>% filter(geo3_bd1991==upz_id))$admin_name

p22b_91 <- ggplot(crosswalk %>% filter(geo3_bd1991==upz_id)) + geom_sf(aes(fill="red"))+
  theme(legend.position = "none") + geom_sf_label(aes(label = "Uttara 1991"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) + # 2 decimal places for longitude
  scale_y_continuous(labels = label_number(accuracy = 0.1)) + labs(y = "", x = "")

p22b_01 <- ggplot(crosswalk %>% filter(geo3_bd2001==upz_id)) + geom_sf(aes(fill="red"))+
  theme(legend.position = "none") + geom_sf_label(aes(label = "Uttara, BimanBandar 2001"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) + # 2 decimal places for longitude
  scale_y_continuous(labels = label_number(accuracy = 0.1)) + labs(y = "", x = "")

p22b_11 <- ggplot(crosswalk %>% filter(geo3_bd2011==upz_id)) + geom_sf(aes(fill="red"))+
  theme(legend.position = "none") + geom_sf_label(aes(label = "Uttara, BimanBandar 2001"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) + # 2 decimal places for longitude
  scale_y_continuous(labels = label_number(accuracy = 0.1)) + labs(y = "", x = "")

p22b_gr <- ggplot(crosswalk_gr %>% filter(merged_id==upz_gr_id)) + geom_sf(aes(fill="red"))+
  theme(legend.position = "none") + geom_sf_label(aes(label = "Greater Region Containing Uttara"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) + # 2 decimal places for longitude
  scale_y_continuous(labels = label_number(accuracy = 0.1)) + labs(y = "", x = "")

p22b_91 / p22b_01 / p22b_11 / p22b_gr

p22b_91 + p22b_01 + p22b_11 + p22b_gr

###########################################
# 2.3 Comparison Between Them by Variable #
###########################################

plot_timeseries <- function(upz_id, summarise_expr, y_label, title_prefix) {
  summarise_expr <- enquo(summarise_expr)
  
  var_noharm <- list()
  
  years <- c(1991, 2001, 2011)
  geo3_bdIDs <- c("geo3_bd1991", "geo3_bd2001", "geo3_bd2011")
  
  for (i in seq_along(years)) {
    year <- years[i]
    geo3_bdID <- geo3_bdIDs[i]
    
    df <- final_data %>%
      filter(!is.na(.data[[geo3_bdID]])) %>%
      group_by(.data[[geo3_bdID]]) %>%
      summarise(result = (!!summarise_expr), .groups="drop") %>%
      left_join(crosswalk %>% select(all_of(geo3_bdID), merged_id, geometry), by = geo3_bdID) %>%
      mutate(year = year)
    
    var_noharm[[i]] <- df  
  }
  
  var_noharm <- bind_rows(var_noharm) %>%
    filter(geo3_bd1991==upz_id | geo3_bd2001==upz_id | geo3_bd2011==upz_id)
  
  var_harm <- final_data %>%
    group_by(year, merged_id) %>%
    summarise(result = (!!summarise_expr), .groups="drop") %>%
    left_join(crosswalk %>% select(merged_id, geometry), by="merged_id")
  
  #-# Now We Make Plots #-#
  p_noharm <- ggplot(var_noharm, aes(x = factor(year), y = result, fill = factor(year))) +
    geom_col() +
    labs(title = paste(y_label, "of Upazila ID:", upz_id, "Over Time (Un-Harmonized)"), x = "Year", y = y_label) +
    guides(fill = guide_legend(title = "Census Year"))+
    scale_y_continuous(labels = scales::comma)+
    theme_minimal() +
    scale_fill_brewer(palette="Set2")
  
  p_harm <- ggplot(var_harm, aes(x = factor(year), y = result, fill = factor(year))) +
    geom_col() +
    labs(title = paste(y_label, "of Greated Region ID:", upz_gr_id, "Over Time (Harmonized)"), x = "Year", y = y_label) +
    guides(fill = guide_legend(title = "Census Year"))+
    scale_y_continuous(labels = scales::comma)+
    theme_minimal() +
    scale_fill_brewer(palette="Set2")
  
  return(list(unharmonized = p_noharm, harmonized = p_harm))
}


plots <- plot_timeseries(upz_id, sum(perwt, na.rm=TRUE), y_label="Population")

plots$unharmonized + plots$harmonized

#####################################################
# 2.3a Application of This: Know The Bigger Picture #
#####################################################

plot_rate_map <- function(data, crosswalk, group_var, rate_condition, year_filter, threshold, title) {

  data_filtered <- data %>%
    filter(!is.na({{group_var}}), year == year_filter)
  
  summarized <- data_filtered %>%
    group_by({{group_var}}) %>%
    summarise(
      total_pop = sum(perwt, na.rm = TRUE),
      target_pop = sum(perwt * ({{rate_condition}}), na.rm = TRUE),
      rate = target_pop / total_pop,
      .groups = "drop"
    ) %>%
    mutate(above_threshold = rate >= threshold)
  
  # prevents blank regions (can happen when upazila/gr has no sample)
  all_geo <- crosswalk %>%
    select({{group_var}}, geometry) %>%
    distinct()
  
  summarized_complete <- all_geo %>%
    left_join(st_drop_geometry(summarized), by = as_label(enquo(group_var))) %>%
    st_as_sf()
  
  p <- ggplot(summarized_complete) +
    geom_sf(aes(geometry = geometry, fill = ifelse(above_threshold, rate, NA)), color = NA) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey", name = "Rate") +
    labs(title = title) +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.title = element_blank())
  
  return(p)
}

emp_unharm <- plot_rate_map(
  data = final_data,
  crosswalk = crosswalk,
  group_var = geo3_bd2011,
  rate_condition = (empstat == 1),
  year_filter = 2011,
  threshold = 0.5,
  title = "Employment Rate (Unharmonized, 2011)"
)

emp_harm <- plot_rate_map(
  data = final_data_gr,
  crosswalk = crosswalk,
  group_var = merged_id,
  rate_condition = (empstat == 1),
  year_filter = 2011,
  threshold = 0.5,
  title = "Employment Rate (Harmonized, 2011)"
)

yrschool_unharm <- plot_rate_map(
  data = final_data,
  crosswalk = crosswalk,
  group_var = geo3_bd2011,
  rate_condition = (yrschool > 10),
  year_filter = 2011,
  threshold = 0.5,
  title = "Education Rate (Unharmonized, 2011)"
)

yrschool_harm <- plot_rate_map(
  data = final_data_gr,
  crosswalk = crosswalk,
  group_var = merged_id,
  rate_condition = (yrschool > 10),
  year_filter = 2011,
  threshold = 0.5,
  title = "Education Rate (Harmonized, 2011)"
)

emp_unharm + emp_harm
yrschool_unharm + yrschool_harm

#####################################################
# 2.3b Application of This: Know The Bigger Picture #
#####################################################

compare_mean_region <- function(var, merged_id_val, year_val) {
  
  var <- ensym(var)
  data <- final_data %>%
    filter(year == year_val)
  
  merged_mean <- data %>%
    filter(merged_id == merged_id_val) %>%
    summarise(
      mean_merged = weighted.mean(!!var, w = perwt, na.rm = TRUE)
    ) %>%
    pull(mean_merged)
  
  constituent_mean <- data %>%
    filter(merged_id == merged_id_val) %>%
    group_by(admin_name) %>%
    summarise(upz_mean = weighted.mean(!!var, w = perwt, na.rm = TRUE)) %>%
    summarise(mean_constituent = mean(upz_mean, na.rm = TRUE)) %>%
    pull(mean_constituent)
  
  return(
    data.frame(
      merged_id = merged_id_val,
      year = year_val,
      merged_mean = merged_mean,
      mean_constituent = constituent_mean
    )
  )
}

# for merged_id = 432, the greater region constituents of many upazila. So, mean is different
compare_mean_region(empstat, 432, 2011)
# for merged_id = 1, the greater region is same as the upazila. So mean is same
compare_mean_region(empstat, 1, 2011)

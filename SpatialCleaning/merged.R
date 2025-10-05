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
    labs(title = paste(y_label, "of Upazila ID:", upz_id, "over Time (Un-Harmonized)"), x = "Year", y = y_label) +
    theme_minimal() +
    scale_fill_brewer(palette="Set2")
  
  p_harm <- ggplot(var_harm, aes(x = factor(year), y = result, fill = factor(year))) +
    geom_col() +
    labs(title = paste(y_label, "of Greated Region ID:", upz_gr_id, "over Time (Harmonized)"), x = "Year", y = y_label) +
    theme_minimal() +
    scale_fill_brewer(palette="Set2")
  
  return(list(unharmonized = p_noharm, harmonized = p_harm))
}


plots <- plot_timeseries(upz_id, sum(perwt, na.rm=TRUE), y_label="Population")

plots$unharmonized + plots$harmonized

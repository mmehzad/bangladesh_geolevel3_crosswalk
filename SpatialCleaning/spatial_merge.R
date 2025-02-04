# install.packages("pacman")

library(pacman)
p_load(tidyverse,
       readxl,
       units,
       knitr,
       sf,
       sp,
       janitor,
       labelled,
       # stringer,  # # Not avialable in this version of R
       fuzzyjoin)

p_load(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

## Preparing the data
upazillas91 <- st_read("./dataset/geo3_bd1991/geo3_bd1991.shp", quiet=TRUE) %>%
  st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character, tolower) %>%
  mutate(area91 = st_area(geometry) %>% drop_units())

upazillas01 <- st_read("./dataset/geo3_bd2001/geo3_bd2001.shp", quiet=TRUE) %>%
  st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character, tolower) %>%
  mutate(area01 = st_area(geometry) %>% drop_units())

upazillas11 <- st_read("./dataset/geo3_bd2011/geo3_bd2011.shp", quiet=TRUE) %>%
  st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character, tolower) %>%
  mutate(area11 = st_area(geometry) %>% drop_units())


## Adjust the coordinate system to that of 1991
upazillas91 <- st_transform(upazillas91, crs=st_crs(upazillas91))
upazillas01 <- st_transform(upazillas01, crs=st_crs(upazillas91))
upazillas11 <- st_transform(upazillas11, crs=st_crs(upazillas91))

## Intermediary datasets
factory_01_to_91 <- st_intersection(upazillas91, upazillas01) %>%
  mutate(overlap_ratio = drop_units(st_area(geometry))/area91) %>%
  rename(admin_name.91 = admin_name, admin_name.01 = admin_name.1) %>%
  filter(0.005 < overlap_ratio & overlap_ratio < 0.995)

factory_11_to_01 <- st_intersection(upazillas01, upazillas11) %>%
  mutate(overlap_ratio = drop_units(st_area(geometry))/area01) %>%
  rename(admin_name.01 = admin_name, admin_name.11= admin_name.1)
  # filter(0.005 < overlap_ratio & overlap_ratio < 0.995)


map_11_to_01 <- factory_11_to_01 %>%
  
  # Errors
  filter(0.005 < overlap_ratio) %>%

  group_by(ipum2011) %>%
  # Assign a 2011 upazilla to 2001 upazilla. If the 2011 map has segments in
  # multiple 2001 upazilla then pick one with the highest ratio.
  filter(overlap_ratio == max(overlap_ratio)) %>%
  ungroup()

  # Note:
  # map_11_to_01$admin_name.01 %>% unique() %>% length() == 489
  # upazillas01$admin_name %>% unique() %>% length() == 490
  # This is because there's no significant (> 0.005) intersection of 2011 upz.
  # with the 2001 upz. called "Karnafuli"

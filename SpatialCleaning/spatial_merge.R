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
up_11_to_01_inter <- st_intersection(upazillas01, upazillas11) %>%
  mutate(inter_area = drop_units(st_area(geometry)), overlap_ratio = 100*inter_area/area11) %>%
  rename(admin_name.01 = admin_name, admin_name.11= admin_name.1)


# sum(up_11_to_01_inter$inter_area)/sum(upazillas01$area01) = 1, so all good

map_11_to_01_per <- up_11_to_01_inter %>%
  
  # Errors
  filter(overlap_ratio >99.99)

map_11_to_01_imper <- up_11_to_01_inter %>%
  
  # Errors
  filter(overlap_ratio >0.5 & overlap_ratio <99.99)

# 543 = dim(map_11_to_01_per)[1] + map_11_to_01_imper$admin_name.11 %>% unique() %>% length(), so all good

# combining un-contained area to a greater 2001 area
# example: 2011 alfadanga = 62.72% 2001 alfadanga + 32.28% 2001 boalmari
#   so, we assign all the rows with admin_name.11 == "alfadanga" to a
#   greater geometry of 2001 alfadanga and 2001 boalmari
greater_region_11_to_01_imper <- left_join(map_11_to_01_imper %>% st_drop_geometry(), upazillas01) %>%
  st_as_sf() %>%
  group_by(admin_name.11) %>%
  mutate(greater_geometry = st_union(geometry), admin_name=NULL) %>%
  ungroup()

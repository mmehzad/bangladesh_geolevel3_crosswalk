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
upazillas91 <- st_read("./SpatialCleaning/dataset/geo3_bd1991/geo3_bd1991.shp", quiet=TRUE) %>%
  st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character, tolower) %>%
  mutate(area91 = st_area(geometry) %>% drop_units())

upazillas01 <- st_read("./SpatialCleaning/dataset/geo3_bd2001/geo3_bd2001.shp", quiet=TRUE) %>%
  st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character, tolower) %>%
  mutate(area01 = st_area(geometry) %>% drop_units())

upazillas11 <- st_read("./SpatialCleaning/dataset/geo3_bd2011/geo3_bd2011.shp", quiet=TRUE) %>%
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
<<<<<<< HEAD
factory_01_to_91 <- st_intersection(upazillas91, upazillas01) %>%
  mutate(overlap_ratio = drop_units(st_area(geometry))/area91) %>%
  rename(admin_name.91 = admin_name, admin_name.01 = admin_name.1) %>%
  filter(0.005 < overlap_ratio & overlap_ratio < 0.995)

up_11_to_01_inter <- st_intersection(upazillas01, upazillas11) %>%
  
  # Containment of 2011 is better captured when 100% of 2011 Up is 2001. To make discussions easiers, transformed ratio into %
  mutate(inter_area = drop_units(st_area(geometry)), overlap_ratio = 100*inter_area/area11) %>%
  rename(admin_name.01 = admin_name, admin_name.11= admin_name.1)
  # filter(0.005 < overlap_ratio & overlap_ratio < 0.995)

# sum(up_11_to_01_inter$inter_area)/sum(upazillas01$area01) = 1

map_11_to_01_per <- up_11_to_01_inter %>%
  
  # Errors
  filter(overlap_ratio >99.99)

map_11_to_01_imper <- up_11_to_01_inter %>%
  
  # Errors
  filter(overlap_ratio >0.5 & overlap_ratio <99.5)

# 543 = dim(map_11_to_01_per)[1] + map_11_to_01_imper$admin_name.11 %>% unique() %>% length(), so all good

# So, there are 15 2011 upazilas where they are not contained in any 2001. Rather, they are took from other Upazilas
# Next, find a way to unite them. If required create a ficticious geolevel3, call it region and define new regions. 




  group_by(ipum2011) %>%
  # Assign a 2011 upazilla to 2001 upazilla. If the 2011 map has segments in
  # multiple 2001 upazilla then pick one with the highest ratio.
  filter(overlap_ratio == max(overlap_ratio)) %>%
  ungroup()

map_11_to_01_test1 %>%
  group_by(ipum2001) %>%
  summarise(area_tot = sum(overlap_ratio)) -> test1

  # Note:
  # map_11_to_01$admin_name.01 %>% unique() %>% length() == 489
  # upazillas01$admin_name %>% unique() %>% length() == 490
  # This is because there's no significant (> 0.005) intersection of 2011 upz.
  # with the 2001 upz. called "Karnafuli"
=======
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
>>>>>>> c6cce65f5e4815c1d5ea7a1ad1d4fa589766d3d9

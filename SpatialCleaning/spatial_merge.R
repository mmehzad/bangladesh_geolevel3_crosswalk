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

#factory_01_to_91 <- st_intersection(upazillas91, upazillas01) %>%
#  mutate(overlap_ratio = drop_units(st_area(geometry))/area91) %>%
#  rename(admin_name.91 = admin_name, admin_name.01 = admin_name.1) %>%
#  filter(0.005 < overlap_ratio & overlap_ratio < 0.995)

up_11_to_01_inter <- st_intersection(upazillas01, upazillas11) %>%
  
  # Containment of 2011 is better captured when 100% of 2011 Up is 2001. To make discussions easier, transformed ratio into %
  mutate(inter_area = drop_units(st_area(geometry)), overlap_ratio = 100*inter_area/area11) %>%
  rename(admin_name.01 = admin_name, admin_name.11= admin_name.1)
  # filter(0.005 < overlap_ratio & overlap_ratio < 0.995)

## Chahchu, checks like this helps in quality control

# sum(up_11_to_01_inter$inter_area)/sum(upazillas01$area01) = 1

# Obtain perfect match
map_11_to_01_per <- up_11_to_01_inter %>%
  filter(overlap_ratio >99.99) %>%
  mutate(matched = 1)

## Chahchu, checks like this helps in quality control

map_11_to_01_per %>%
  ggplot() + geom_sf(aes(fill = as.factor(matched)))


map_11_to_01_imper <- up_11_to_01_inter %>%
  
  # True overlap where 2011 is not contained in a previous 2011
  filter(overlap_ratio >0.5 & overlap_ratio <99.5)

# 543 = dim(map_11_to_01_per)[1] + map_11_to_01_imper$admin_name.11 %>% unique() %>% length(), so all good

# So, there are 15 2011 upazilas where they are not contained in any 2001. Rather, they are took from other Upazilas
# Next, find a way to unite them. If required create a ficticious geolevel3, call it region and define new regions. 

  # Note:
  # map_11_to_01$admin_name.01 %>% unique() %>% length() == 489
  # upazillas01$admin_name %>% unique() %>% length() == 490
  # This is because there's no significant (> 0.005) intersection of 2011 upz.
  # with the 2001 upz. called "Karnafuli"

# combining un-contained area to a greater 2001 area
# example: 2011 alfadanga = 62.72% 2001 alfadanga + 32.28% 2001 boalmari
#   so, we assign all the rows with admin_name.11 == "alfadanga" to a
#   greater geometry of 2001 alfadanga and 2001 boalmari

# lets check if the imperfect match data is in the perfect match or not. Robustness check

inner_join(map_11_to_01_per %>% st_drop_geometry() %>% select(ipum2001, ipum2011), 
          map_11_to_01_imper %>% st_drop_geometry() %>% select(ipum2001, ipum2011)) %>% dim()

# Dim is 0, so the code below works!

greater_region_11_to_01_imper <- map_11_to_01_imper %>%
  
  group_by(ipum2011) %>% # Do not use name please, use ids..
  summarise(ipum2001 = paste0(ipum2001, collapse=""),
            geometry = st_union(geometry), admin_name=NULL) %>%
  ungroup() %>%
  mutate(matched = 0, new_matched = 1)

map_11_to_01_per %>%
  select(ipum2011, ipum2001, matched) %>% mutate(new_matched = 0) %>%
  rbind(greater_region_11_to_01_imper) -> map_check

map_check %>%
  group_by(ipum2001) %>%
  summarise(geometry = st_union(geometry), matched = mean(matched), new_matched = mean(new_matched)) -> new_map

new_map %>%
  ggplot() + 
  geom_sf(aes(fill = as.factor(matched)))

new_map %>%
  ggplot() + 
  geom_sf(aes(fill = as.factor(new_matched)))
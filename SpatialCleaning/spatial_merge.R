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
  
  # Containment of 2011 is better captured when 100% of 2011 Up is 2001. To make discussions easier, transformed ratio into %
  mutate(inter_area = drop_units(st_area(geometry)), overlap_ratio = 100*inter_area/area11) %>%
  rename(admin_name.01 = admin_name, admin_name.11= admin_name.1)

## Quality Assurance Test (QAT, sounds a lot like CAT =^. .^=)
sum(up_11_to_01_inter$inter_area)/sum(upazillas01$area01)  ## = 1, so it's all good

# Obtain perfect match
map_11_to_01_per <- up_11_to_01_inter %>%
  filter(overlap_ratio >99.99) %>%
  mutate(matched = 1)

## QAT
map_11_to_01_per %>%
  ggplot() + geom_sf(aes(fill = as.factor(matched)))

# Obtain imperfect match
map_11_to_01_imper <- up_11_to_01_inter %>%
  # True overlap where 2011 is not contained in a previous 2011
  filter(overlap_ratio >0.5 & overlap_ratio <99.99)

## QAT

dim(map_11_to_01_per)[1] + map_11_to_01_imper$admin_name.11 %>% unique() %>% length()  ## = 543, so all good


## QAT: lets check if the imperfect match data is in the perfect match or not. Robustness check
inner_join(map_11_to_01_per %>% st_drop_geometry() %>% select(ipum2001, ipum2011), 
           map_11_to_01_imper %>% st_drop_geometry() %>% select(ipum2001, ipum2011)) %>% dim()

## Dim is 0, so the code below works!

######################### Something is going wrong between here, start

## The problem was noticed when I see that even in 2011 -> 2011 reshaping, we have sum or areas over sum of areas > 1. 
## This and plots should be a gut check all the time

map_11_to_01_per$ipum2011 %>% unique() %>% length() - map_11_to_01_per$ipum2001 %>% unique() %>% length() != 0
map_11_to_01_imper$ipum2011 %>% unique() %>% length() - map_11_to_01_imper$ipum2001 %>% unique() %>% length() != 0

# I think we need to remove the duplicates very carefully to create the geo3level and some how keep track of which 2001/2011 impum code belong to which greater region code
# We want to keep track of names of upazilas as well. not needed right now, but we want them in final product
## Below is an attempt, but note that the plot(geolevel3) below reveals unmatched areas.

# We want to keep 2001's geometry. As we'll need to union them and remove duplcates
greater_region_11_to_01_imper <- left_join(map_11_to_01_imper %>% st_drop_geometry(), upazillas01) %>%
  st_as_sf() %>% 
  group_by(ipum2011) %>%
  mutate(geometry = st_union(geometry), admin_name=NULL) %>%
  ungroup() %>%
  select(ipum2001, ipum2011, geometry) %>%
  # filter(duplicated(ipum2011) == FALSE) %>%
  # distinct(ipum2011, .keep_all = TRUE) %>%
  mutate(matched = 0, new_matched = 1)


greater_region_11_to_01 <- 
  left_join(map_11_to_01_per %>% st_drop_geometry(), upazillas01) %>%
  st_as_sf() %>%
  group_by(ipum2011) %>%
  mutate(geometry = st_union(geometry), admin_name = NULL) %>%
  ungroup() %>%
  select(ipum2001, ipum2011, geometry) %>%
  # distinct(ipum2011, .keep_all = TRUE) %>%
  mutate(matched = 1, new_matched = 0) %>%
  rbind(greater_region_11_to_01_imper)
  
  # map_11_to_01_per %>%
  # select(ipum2001, ipum2011, geometry) %>%
  # group_by(ipum2001) %>%
  # mutate(geometry = st_union(geometry)) %>%
  # # filter(duplicated(ipum2001) == FALSE) %>% 
  # distinct(ipum2001, .keep_all = TRUE) %>%
  # mutate(matched = 1, new_matched = 0) %>%
  # rbind(greater_region_11_to_01_imper) %>%
  # mutate(area1101 = drop_units(st_area(geometry)))

geolevel3 <- greater_region_11_to_01 %>%
  group_by(ipum2011) %>%
  summarise(ipum2001 = paste0(ipum2001, collapse="_")) %>%
  ungroup() %>%
  # filter(duplicated(ipum2001) == FALSE) %>%
  distinct(ipum2001, .keep_all = TRUE) %>%
  mutate(area1101 = drop_units(st_area(geometry)), matched = 1)

sum(geolevel3$area1101)/sum(upazillas01$area01)

plot(geolevel3)
######################### Something is going wrong between here, end

## QAT
greater_region_11_to_01$ipum2001 %>% unique() %>% length()  # = 507, looks good
greater_region_11_to_01$ipum2011 %>% unique() %>% length()  # = 543, looks good
greater_region_11_to_01$geometry %>% unique() %>% length()  # = 508

# border test with one sample from imperfect and perfect match respectively
plot((greater_region_11_to_01 %>% filter(ipum2011=="030029003"))$geometry)  # matches the one below
plot((upazillas01 %>% filter(admin_name=="boalmari" | admin_name=="alfadanga"))$geometry)

plot((map_11_to_01_per %>% filter(ipum2001=="040001008"))$geometry)  # matches the one below
plot((greater_region_11_to_01 %>% filter(ipum2001=="040001008"))$geometry)

greater_region_11_to_01 %>%
  ggplot() + geom_sf(aes(fill = as.factor(matched)))


## 2001 -> 1991
up_01_to_91_inter <- st_intersection(upazillas91, geolevel3) %>%
  mutate(inter_area = drop_units(st_area(geometry)), overlap_ratio = 100*inter_area/area1101)

up_01_to_91_inter$ipum2001 %>% unique() %>% length()  ## = 508 = geolevel3$ipum2001, all good
sum(up_01_to_91_inter$inter_area)/sum(upazillas91$area91)  ## = 1.0367 != 1, sus

map_01_to_91_per <- up_01_to_91_inter %>%
  filter(overlap_ratio >99.99) %>%
  mutate(matched = 1)

map_01_to_91_per$ipum1991 %>% unique() %>% length()  # = 463 < 485, so looks ok

map_01_to_91_imper <- up_01_to_91_inter %>%
  filter(overlap_ratio >0.5 & overlap_ratio <99.99)

## QAT
map_01_to_91_per$ipum2001 %>% unique() %>% length()  # = 480
map_01_to_91_imper$ipum2001 %>% unique() %>% length() # = 28
# sum = 508. Perfect match

# intersect(map_01_to_91_per$ipum2001, map_01_to_91_imper$ipum2001)
#=> Previously: "020012013" "020030014" "030026095"; Now: character(0). Good sign
# upazillas01 %>% filter(ipum2001=="020012013" | ipum2001=="020030014" | ipum2001=="030026095")

# # if you check those areas, you'll see that the sum of their overlap_ratio doesn't add up to 100.
# sus_regions <- up_01_to_91_inter %>%
#   filter(ipum2001=="020012013" | ipum2001=="020030014" | ipum2001=="030026095") %>%
#   filter(overlap_ratio >0.5)

# # will ignore them and wait for chachu's review
# map_01_to_91_per <- map_01_to_91_per %>%
#   filter(!(ipum2001=="020012013" | ipum2001=="020030014" | ipum2001=="030026095"))
# 
# map_01_to_91_imper <- map_01_to_91_imper %>%
#   filter(!(ipum2001=="020012013" | ipum2001=="020030014" | ipum2001=="030026095"))

map_01_to_91_per %>%
  ggplot() + geom_sf(aes(fill = as.factor(matched)))

greater_region_01_to_91_imper <- left_join(map_01_to_91_imper %>% st_drop_geometry(), upazillas91) %>%
  st_as_sf() %>%
  group_by(ipum2001) %>%
  mutate(geometry = st_union(geometry), admin_name=NULL) %>%
  ungroup() %>%
  select(ipum1991, ipum2001, geometry) %>%
  mutate(matched = 0, new_matched = 1)

greater_region_01_to_91 <- map_01_to_91_per %>%
  select(ipum1991, ipum2001, geometry) %>%
  mutate(matched = 1, new_matched = 0) %>%
  rbind(greater_region_01_to_91_imper) %>%
  mutate(area110191 = drop_units(st_area(geometry)))

geolevel3 <- greater_region_01_to_91 %>%
  group_by(ipum2001) %>%
  summarise(ipum1991 = paste0(ipum1991, collapse="_")) %>%
  ungroup() %>%
  distinct(ipum1991, .keep_all = TRUE) %>%
  mutate(area110191 = drop_units(st_area(geometry)))

greater_region_01_to_91 %>%
  ggplot() + geom_sf(aes(fill = as.factor(matched)))

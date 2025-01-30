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


# Adjust the coordinate system to that of 1991
upazillas91 <- st_transform(upazillas91, crs=st_crs(upazillas91))
upazillas01 <- st_transform(upazillas01, crs=st_crs(upazillas91))
upazillas11 <- st_transform(upazillas11, crs=st_crs(upazillas91))

## Mapping of 2011 to 2001
factory_11 <- st_intersection(upazillas01, upazillas11) %>%
  
  # Ignore overlaps with ratio below 0.25. Likely, just errors
  mutate(overlap_ratio = drop_units(st_area(geometry))/area11) %>%
  filter(overlap_ratio > 0.25) %>%
  
  # We'll use deviation for decision making purpose
  group_by(admin_name.1) %>%
  mutate(deviation = abs(overlap_ratio - 0.5)) %>%
  
  # We don't need to work with all the columns
  select(admin_name.01 = admin_name,
         admin_name.11 = admin_name.1,
         geometry, overlap_ratio, deviation)

map_11_to_01 <- factory_11 %>%
  filter(deviation > 0.25
        | deviation < 0.25 & (admin_name.01 == admin_name.11
        | (admin_name.01 == "comilla sadar (kotwali)" & admin_name.11 == "comilla sadar dakhin")
        # Not exactly sure about this one
        | (admin_name.01 == "kulaura" & admin_name.11 == "juri"))) %>%
  
  # we don't need geometry and overlap info for the map
  st_drop_geometry() %>%
  select(admin_name.01, admin_name.11)
  


## Mapping of 2001 to 1991
factory_01 <- st_intersection(upazillas91, upazillas01) %>%

  # Ignore overlaps with ratio below 0.25. Likely, just errors
  mutate(overlap_ratio = drop_units(st_area(geometry))/area01) %>%
  filter(overlap_ratio > 0.25) %>%

  # We'll use deviation for decision making purpose
  group_by(admin_name.1) %>%
  mutate(deviation = abs(overlap_ratio - 0.5)) %>%

  # We don't need to work with all the columns
  select(admin_name.91 = admin_name,
         admin_name.01 = admin_name.1,
         geometry, overlap_ratio, deviation)


map_01_to_91 <- factory_01 %>%
  # Anything above 0.25 deviation is highly likely to be the actual map
  filter(deviation > 0.25
         # under 0.25 dev., we're first assigning by name
         # then, we assign case by case
         | deviation < 0.25 & (admin_name.91 == admin_name.01
         # Not sure about the one in the next line
         | (admin_name.91 == "brahmanbaria sadar" & admin_name.01 == "ashuganj")
         | (admin_name.91 == "daudkandi" & admin_name.01 == "meghna")
         | (admin_name.91 == "anowara" & admin_name.01 == "karnafuli"))) %>%

  # we don't need geometry and overlap info for the map
  st_drop_geometry() %>%
  select(admin_name.91, admin_name.01)

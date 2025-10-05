library(pacman)

options("install.lock"=FALSE)
p_load(tidyverse, readxl, units, knitr, sf, sp, janitor, labelled, stringr, fuzzyjoin)

p_load(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

## Loading data

# working with the matched data

crosswalk <- st_read("./output/crosswalk_bdgeo3_91_11.shp") %>%
  mutate(upazilas = tolower(upazilas))

clean_name_to_match_crosswalk <- function(data) {
  data$upazila[data$upazila == "biman bandar"] <- "uttara, bimanbandar thana"
  data$upazila[data$upazila == "kadamtali"] <- "kadam tali"
  data$upazila[data$upazila == "tejgaon ind. area"] <- "tejgaon industrial area"
  data$upazila[data$upazila == "tejgoan ind. area"] <- "tejgaon industrial area"
  data$upazila[data$upazila == "pallbi"] <- "pallabi"
  data$upazila[data$upazila == "shahbagh"] <- "shahbag"
  # there is none NA left, I don't know what to do with that
  
  return(data)
}

enrich_with_crosswalk <- function(data) {
  # 1. Match by upazila name (partial match)
  data_named <- data %>%
    filter(!is.na(upazila) & upazila != "") %>%
    rowwise() %>%
    mutate(
      match_row = list(
        crosswalk %>%
          st_drop_geometry() %>%
          filter(str_detect(upazilas, fixed(upazila))) %>%
          slice(1)
      ),
      upazila_merged_id = as.character(if (nrow(match_row) > 0) match_row$merged_id[[1]] else NA_character_),
      upazila_merged_name = as.character(if (nrow(match_row) > 0) match_row$upazilas[[1]] else NA_character_)
    ) %>%
    ungroup() %>%
    select(-match_row)
  
  # 2. Spatial join for unmatched rows with lat/lon
  data_unmatched <- data %>%
    filter(is.na(upazila) | upazila == "") %>%
    filter(!is.na(Longitude) & !is.na(Latitude)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(crs = st_crs(crosswalk))
  
  data_spatial_joined <- st_join(data_unmatched, crosswalk) %>%
    rename(upazila_merged_id = merged_id, upazila_merged_name = upazilas) %>%
    mutate(
      upazila_merged_id = as.character(upazila_merged_id),
      upazila_merged_name = as.character(upazila_merged_name)
    ) %>%
    st_drop_geometry()
  
  # 3. Combine both results (types now match)
  result <- bind_rows(data_named, data_spatial_joined)
  
  return(result)
}

matched_data_01 <- read.csv("./dataset/03_all_matched_data_01.csv") %>%
  mutate(upazila = tolower(upazila)) %>%
  select(-c(name, fac_ad, product_type)) %>%
  clean_name_to_match_crosswalk() %>%
  enrich_with_crosswalk()


matched_data_09 <- read.csv("./dataset/03_all_matched_data_09.csv") %>%
  mutate(upazila = tolower(upazila)) %>%
  select(-c(name, fac_ad, product_type)) %>%
  clean_name_to_match_crosswalk() %>%
  enrich_with_crosswalk()

matched_data_11 <- read_excel("./dataset/00_bgmea_factory_data_final.xlsx", sheet = "bgmea_2015_final") %>%
  mutate(name = tolower(name), upazila = tolower(upazila)) %>%
  filter(date_est <= 2011) %>%
  clean_name_to_match_crosswalk() %>%
  enrich_with_crosswalk()



# Convert to Sf and transform to Mercator Gulshan
merged_sf <- merged_sf %>% st_transform(3106)

# split matched datasets into ones with and without missing upaz

missing_upz01 <- matched_data_01 %>% filter(is.na(upazila))

missing_upz09 <- matched_data_09 %>% filter(is.na(upazila))


# geo-coding missing upzaila data for 01 and combining them to original

coordinates(matched_data_01) <- c("Longitude", "Latitude")

# Set projection string
proj4string(missing_upz01) <- CRS("+init=epsg:4326") 

# Convert to Sf and transform to Mercator Gulshan
missing_upz01 <- st_as_sf(missing_upz01) %>% st_transform(3106)


missing_upz01 %>%
  mutate(id = as.integer(st_intersects(geometry, upazilas11)),
         upazila = upazilas11$admin_name[id]) %>%
  as.data.frame() %>%
  select(-c(geometry, id)) -> missing_upz01


matched_data_01 %>%
  filter(!is.na(upazila)) %>%
  select(-c(Longitude, Latitude)) %>%
  rbind(missing_upz01) -> matched_data_01

rm(missing_upz01)

# geo-coding missing upzaila data for 09 and combining them to original

coordinates(missing_upz09) <- c("Longitude", "Latitude")

# Set projection string
proj4string(missing_upz09) <- CRS("+init=epsg:4326") 

# Convert to Sf and transform to Mercator Gulshan
missing_upz09 <- st_as_sf(missing_upz09) %>% st_transform(3106)


missing_upz09 %>%
  mutate(id = as.integer(st_intersects(geometry, upazilas11)),
         upazila = upazilas11$admin_name[id]) %>%
  as.data.frame() %>%
  select(-c(geometry, id)) -> missing_upz09

matched_data_09 %>%
  filter(!is.na(upazila)) %>%
  select(-c(Longitude, Latitude)) %>%
  rbind(missing_upz09) -> matched_data_09

rm(missing_upz09)

## Three unmatched facs are removed

matched_data_01 %>%
  filter(!is.na(upazila)) -> matched_data_01

matched_data_09 %>%
  filter(!is.na(upazila)) -> matched_data_09


# Next, fix upazila to have congruence with the 2011 names

## Finding issues

matched_data_01 %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz01

matched_data_09 %>%
  filter(exist06 == 1) %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz91


matched_data_11 %>%
  mutate(upazila = tolower(upazila)) %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz11

rbind(upaz01, upaz91, upaz11) %>% unique() -> fac_upaz


# we find the following issues and solve them

## We change, all (i) Uttara and Biman Bandar to uttara, bimanbandar, (ii) pallbi to pallabi

matched_data_01$upazila[matched_data_01$upazila %in% c("uttara", "biman bandar")] <- "uttara, bimanbandar"

matched_data_01$upazila[matched_data_01$upazila == "pallbi"] <- "pallabi"

## (iii) Shabagh to shahbag, (iv) tejgaon ind. area to tejgaon industrial area, and

## (v) kadamtali to kadam tali

matched_data_01$upazila[matched_data_01$upazila == "shahbagh"] <- "shahbag"

matched_data_01$upazila[matched_data_01$upazila == "tejgaon ind. area"] <- "tejgaon industrial area"
matched_data_01$upazila[matched_data_01$upazila == "tejgoan ind. area"] <- "tejgaon industrial area"

matched_data_01$upazila[matched_data_01$upazila == "kadamtali"] <- "kadam tali"



# we find the following issues and solve them

## We change, all (i) Uttara and Biman Bandar to uttara, bimanbandar, (ii) pallbi to pallabi

matched_data_09$upazila[matched_data_09$upazila %in% c("uttara", "biman bandar")] <- "uttara, bimanbandar"

matched_data_09$upazila[matched_data_09$upazila == "pallbi"] <- "pallabi"

## (iii) Shabagh to shahbag, (iv) tejgaon ind. area to tejgaon industrial area, and

## (v) kadamtali to kadam tali

matched_data_09$upazila[matched_data_09$upazila == "shahbagh"] <- "shahbag"

matched_data_09$upazila[matched_data_09$upazila == "tejgaon ind. area"] <- "tejgaon industrial area"
matched_data_09$upazila[matched_data_09$upazila == "tejgoan ind. area"] <- "tejgaon industrial area"

matched_data_09$upazila[matched_data_09$upazila == "kadamtali"] <- "kadam tali"
matched_data_09$upazila[matched_data_09$upazila == "shabujbagh"] <- "sabujbagh"


# Changing names to make merging possible

matched_data_11$upazila[matched_data_11$upazila == "kadamtali"] <- "kadam tali"
matched_data_11$upazila[matched_data_11$upazila == "uttara"] <- "uttara, bimanbandar"
matched_data_11$upazila[matched_data_11$upazila == "biman bandar"] <- "uttara, bimanbandar"
matched_data_11$upazila[matched_data_11$upazila == "tejgaon ind. area"] <- "tejgaon industrial area"
matched_data_11$upazila[matched_data_11$upazila == "comilla sadar dakshin"] <- "comilla sadar dakhin"


matched_data_01 %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz01

matched_data_09 %>%
  filter(exist06 == 1) %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz91


matched_data_11 %>%
  mutate(upazila = tolower(upazila)) %>%
  select(upazila) %>% unique() %>%
  left_join(upazilas11 %>% as.data.frame() %>%
              select(admin_name, upaz2011, parent) %>%
              rename(upazila = admin_name)) -> upaz11

rbind(upaz01, upaz91, upaz11) %>% unique() -> fac_upaz

fac_upaz %>% filter(!is.na(upazila)) -> fac_upaz

rm("upaz01", "upaz91", "upaz11")

## Removing upazilas from upazilas11 (all upazila file) that generates duplicates
# There are multiple Kaliganj, remove all but the one in Gazipur
## 406552 is lohagara not in chit, 405094 is mirpur not in Dhaka,
## 405566 is mohammadpur not in dhaka, 405595 is not in gazipur

fac_upaz %>%
  filter(!upaz2011 %in% c("555239", "552769", "408747", "404433", "406552", "405094", "405566", "405595")) -> fac_upaz

left_join(matched_data_01, fac_upaz, by = "upazila") %>%
  mutate(across(c(upaz2011, parent), as.integer)) -> matched_data_01

# only keeping data for 2006 and before facs

matched_data_09 %>%
  filter(date_est <= 2006) %>%
  left_join(fac_upaz, by = "upazila") %>%
  mutate(across(c(upaz2011, parent), as.integer)) -> matched_data_09_06

### Writing 91,01 data, then the 06 data, then the factory list

write.csv(matched_data_01, "~/large_datasets/BD Garments/04_final_data_01.csv", row.names = F)
write.csv(matched_data_09, "~/large_datasets/BD Garments/04_final_data_09.csv", row.names = F)
write.csv(matched_data_09, "~/large_datasets/BD Garments/04_final_data_09_06.csv", row.names = F)
write.csv(matched_data_11, "~/large_datasets/BD Garments/04_final_data_11.csv", row.names = F)

save(list = c("matched_data_01","matched_data_09", "matched_data_11"),
     file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/factories.Rdata")


## Matching with 2011

fac_upaz %>% 
  left_join(factory_upazilas11 %>% select(-admin_name)) %>%
  st_as_sf() -> factory_upazilas

### Creating a template of factory upazila matching over time.

factory_upazilas11  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd2011/geo3_bd2011.shp"

factory_upazilas11 <- st_read(factory_upazilas11, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas11 <- st_as_sf(factory_upazilas11) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), contains("IP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area11 = st_area(geometry) %>% drop_units())

rm("fac_upaz")

factory_upazilas01  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd2001/geo3_bd2001.shp"

factory_upazilas01 <- st_read(factory_upazilas01, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas01 <- st_as_sf(factory_upazilas01) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), contains("IP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  rename(upazila01 = admin_name) %>%
  mutate(area01 = st_area(geometry) %>% drop_units())


factory_upazilas <- st_intersection(factory_upazilas, factory_upazilas01) %>%
  mutate(overlap_size = drop_units(st_area(geometry))) %>%
  filter(overlap_size > 100) %>%
  mutate(overlap_ratio = overlap_size/area11) %>% filter(overlap_ratio > 0.25)

factory_upazilas <- factory_upazilas %>% as.data.frame() %>%
  select(-c(parent.1, overlap_size, geometry)) %>%
  rename(overlap_ratio_1101 = overlap_ratio)

factory_upazilas <- left_join(factory_upazilas,
                              factory_upazilas01 %>%
                                select(upaz2001)) %>% st_as_sf()


# 1991 dataset

factory_upazilas91  <- "C:/Users/smshi/OneDrive/Documents/large_datasets/BD HH or Micro data/IPUMS-I/geo3_bd1991/geo3_bd1991.shp"

factory_upazilas91 <- st_read(factory_upazilas91, quiet = TRUE)

# Convert to Sf and transform to Mercator Gulshan
factory_upazilas91 <- st_as_sf(factory_upazilas91) %>% st_transform(3106) %>%
  select(ADMIN_NAME, contains("UP"), contains("IP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character,tolower) %>%
  mutate(area91 = st_area(geometry) %>% drop_units())


5%>%
  as.data.frame() %>%
  select(-c(geometry, overlap_size, parent.1, upazila, area11, area01, upazila01,
            area91, overlap_ratio, overlap_ratio_1101)) %>%
  rename(upazila = admin_name)

save(factory_upazilas, file = "C:/Users/smshi/Dropbox/Research/BD-RMG-Women/data/fac_upazilas.RData")


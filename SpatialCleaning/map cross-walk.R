library(sf)
library(dplyr)
library(igraph)
library(units)
library(janitor)
library(tidyverse)


threshold <- 0.001  # 0.1% overlap

# Function to get filtered overlaps between two sf layers
get_overlap_pairs <- function(mapA, mapB, idA, idB, prefixA, prefixB, threshold) {
  inter <- st_intersection(mapA, mapB)
  if (nrow(inter) == 0) return(data.frame())
  
  inter <- inter %>%
    mutate(
      area_intersection = st_area(geometry),
      areaA = st_area(mapA[match(.[[idA]], mapA[[idA]]), ]),
      areaB = st_area(mapB[match(.[[idB]], mapB[[idB]]), ]),
      propA = as.numeric(area_intersection / areaA),
      propB = as.numeric(area_intersection / areaB)
    ) %>%
    filter(propA > threshold | propB > threshold) %>%
    st_drop_geometry() %>%
    mutate(
      nodeA = paste0(prefixA, "_", .[[idA]]),
      nodeB = paste0(prefixB, "_", .[[idB]])
    ) %>%
    select(nodeA, nodeB)
  
  return(inter)
}

# Step 0: Load all the upazillas
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

upazillas91 <- st_transform(upazillas91, crs=st_crs(upazillas91))
upazillas01 <- st_transform(upazillas01, crs=st_crs(upazillas91))
upazillas11 <- st_transform(upazillas11, crs=st_crs(upazillas91))


# Step 1: Get overlap edges
edges_91_01 <- get_overlap_pairs(upazillas91, upazillas01, "ipum1991", "ipum2001", "u91", "u01", threshold)
edges_01_11 <- get_overlap_pairs(upazillas01, upazillas11, "ipum2001", "ipum2011", "u01", "u11", threshold)
edges_91_11 <- get_overlap_pairs(upazillas91, upazillas11, "ipum1991", "ipum2011", "u91", "u11", threshold)

# Step 2: Combine edges and build graph
all_edges <- bind_rows(edges_91_01, edges_01_11, edges_91_11) %>% as.matrix()
g <- graph_from_edgelist(all_edges, directed = FALSE)
groups <- components(g)$membership
group_map <- data.frame(node = names(groups), merged_id = groups)

# Step 3: Map original polygons to merged groups
xwalk91 <- data.frame(node = paste0("u91_", upazillas91$ipum1991), ipum1991 = upazillas91$ipum1991) %>%
  left_join(group_map, by = "node") %>%
  select(ipum1991, merged_id)

xwalk01 <- data.frame(node = paste0("u01_", upazillas01$ipum2001), ipum2001 = upazillas01$ipum2001) %>%
  left_join(group_map, by = "node") %>%
  select(ipum2001, merged_id)

xwalk11 <- data.frame(node = paste0("u11_", upazillas11$ipum2011), ipum2011 = upazillas11$ipum2011) %>%
  left_join(group_map, by = "node") %>%
  select(ipum2011, merged_id)

# Step 4: Assign unique merged_ids to unmatched polygons
next_id <- max(group_map$merged_id, na.rm = TRUE) + 1
xwalk91$merged_id[is.na(xwalk91$merged_id)] <- next_id + seq_len(sum(is.na(xwalk91$merged_id))) - 1
xwalk01$merged_id[is.na(xwalk01$merged_id)] <- max(xwalk91$merged_id) + seq_len(sum(is.na(xwalk01$merged_id)))
xwalk11$merged_id[is.na(xwalk11$merged_id)] <- max(xwalk01$merged_id) + seq_len(sum(is.na(xwalk11$merged_id)))

# Step 5: Join merged_id back to sf layers
upazillas91 <- left_join(upazillas91, xwalk91, by = "ipum1991")
upazillas01 <- left_join(upazillas01, xwalk01, by = "ipum2001")
upazillas11 <- left_join(upazillas11, xwalk11, by = "ipum2011")

# Step 6: Combine all and merge by merged_id
combined <- bind_rows(upazillas91, upazillas01, upazillas11)

merged_sf <- combined %>%
  group_by(merged_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Step 7: Final crosswalk
final_crosswalk <- bind_rows(
  xwalk91 %>% mutate(source = "upazillas91"),
  xwalk01 %>% mutate(source = "upazillas01"),
  xwalk11 %>% mutate(source = "upazillas11")
)

# write.csv(final_crosswalk, file = "crosswalk.csv", row.names = FALSE)

# Step 8: Build mapping from ipumXXXX -> actual upazilla names
names91 <- upazillas91 %>% st_drop_geometry() %>% select(ipum1991, name91 = admin_name)
names01 <- upazillas01 %>% st_drop_geometry() %>% select(ipum2001, name01 = admin_name)
names11 <- upazillas11 %>% st_drop_geometry() %>% select(ipum2011, name11 = admin_name)

# Step 9: Add names to final_crosswalk
final_crosswalk_named <- final_crosswalk %>%
  left_join(names91, by = "ipum1991") %>%
  left_join(names01, by = "ipum2001") %>%
  left_join(names11, by = "ipum2011")

# Step 10: Build the 'upazillas' column from names
final_crosswalk_named <- final_crosswalk_named %>%
  mutate(
    upazillas = paste(
      na.omit(c(name91, name01, name11)), collapse = "_"
    )
  ) %>%
  select(-name91, -name01, -name11)

# Step 11: Add geometry
final_crosswalk_geo <- final_crosswalk_named %>%
  left_join(merged_sf, by = "merged_id")

# Optional: select and order relevant columns
final_crosswalk_geo <- final_crosswalk_geo %>%
  select(merged_id, upazillas, geometry, everything())

st_write(final_crosswalk_geo, "./SpatialCleaning/output/crosswalk_bdgeo3_91_11.shp")

write.csv(final_crosswalk_geo, "./SpatialCleaning/output/crosswalk_bdgeo3_91_11.csv")

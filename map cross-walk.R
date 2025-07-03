library(sf)
library(dplyr)
library(igraph)

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

write.csv(final_crosswalk, file = "crosswalk.csv", row.names = FALSE)
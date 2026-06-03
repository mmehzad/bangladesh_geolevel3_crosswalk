library(sf)
library(units)
library(dplyr)
library(igraph)
library(mclust)
library(janitor)
library(tidyverse)
library(patchwork)



threshold <- 0.001  # 0.1% overlap
# threshold <- 0.9    # P(real | x) > 0.9

# Function to get filtered overlaps between two sf layers

# get_overlap_pairs <- function(mapA, mapB, idA, idB, prefixA, prefixB, threshold) {
#   inter <- st_intersection(mapA, mapB)
#   if (nrow(inter) == 0) return(data.frame())
# 
#   inter <- inter %>%
#     mutate(
#       area_intersection = st_area(geometry),
#       areaA = st_area(mapA[match(.[[idA]], mapA[[idA]]), ]),
#       areaB = st_area(mapB[match(.[[idB]], mapB[[idB]]), ]),
#       propA = as.numeric(area_intersection / areaA),
#       propB = as.numeric(area_intersection / areaB)
#     ) %>%
#     filter(propA > threshold | propB > threshold) %>%
#     st_drop_geometry() %>%
#     mutate(
#       nodeA = paste0(prefixA, "_", .[[idA]]),
#       nodeB = paste0(prefixB, "_", .[[idB]])
#     ) %>%
#     select(nodeA, nodeB)
# 
#   return(inter)
# }
# get_overlap_pairs <- function(mapA, mapB, idA, idB, prefixA, prefixB, prob_cut = 0.9) {
# 
#   inter <- st_intersection(mapA, mapB)
#   if (nrow(inter) == 0) return(data.frame())
# 
#   inter <- inter %>%
#     mutate(
#       area_intersection = st_area(geometry),
#       areaA = st_area(mapA[match(.[[idA]], mapA[[idA]]), ]),
#       areaB = st_area(mapB[match(.[[idB]], mapB[[idB]]), ]),
#       propA = as.numeric(area_intersection / areaA),
#       propB = as.numeric(area_intersection / areaB),
# 
#       # single overlap metric (very important)
#       overlap_metric = pmin(propA, propB)
#     )
# 
#   # log-transform to reduce skew
#   # 1e-10 is to avoid log10(0) without significantly affecting the value
#   x <- log10(inter$overlap_metric + 1e-10)
# 
#   # fit 2-component Gaussian mixture
#   fit <- Mclust(x, G = 2)
# 
#   # posterior probabilities
#   post <- fit$z
# 
#   # determine which component represents real overlaps
#   comp_means <- fit$parameters$mean
#   real_comp <- which.max(comp_means)
# 
#   prob_real <- post[, real_comp]
# 
#   inter <- inter %>%
#     mutate(prob_real = prob_real) %>%
#     filter(prob_real >= prob_cut) %>%
#     st_drop_geometry() %>%
#     mutate(
#       nodeA = paste0(prefixA, "_", .[[idA]]),
#       nodeB = paste0(prefixB, "_", .[[idB]])
#     ) %>%
#     select(nodeA, nodeB)
# 
#   return(inter)
# }
get_overlap_pairs <- function(mapA, mapB, idA, idB, prefixA, prefixB) {
  inter <- st_intersection(mapA, mapB)
  if (nrow(inter) == 0) return(data.frame())

  inter <- inter %>%
    mutate(
      area_intersection = st_area(geometry),
      areaA = st_area(mapA[match(.[[idA]], mapA[[idA]]), ]),
      areaB = st_area(mapB[match(.[[idB]], mapB[[idB]]), ]),
      propA = as.numeric(area_intersection / areaA),
      propB = as.numeric(area_intersection / areaB),
    ) %>%
    filter((as.numeric(area_intersection) / (1000 ^ 2)) > ((0.313472877 + 0.000962589) / 2)) %>%
    st_drop_geometry() %>%
    mutate(
      nodeA = paste0(prefixA, "_", .[[idA]]),
      nodeB = paste0(prefixB, "_", .[[idB]])
    ) %>%
    select(nodeA, nodeB)

  return(inter)
}


compute_overlap_metrics <- function(mapA, mapB, idA, idB) {
  
  # area > 1sqkm, area < 1sqkm or perimeter over (highest jump)
  
  mapA <- mapA %>% mutate(A = as.numeric(st_area(geometry)))
  mapB <- mapB %>% mutate(B = as.numeric(st_area(geometry)))
  
  inter <- st_intersection(mapA, mapB)
  if (nrow(inter) == 0) return(data.frame())
  
  inter <- inter %>%
    mutate(
      A_intersect_B = as.numeric(st_area(geometry)),
      A_intersect_B_perimeter = as.numeric(st_perimeter(geometry))^2,
      A_union_B = A + B - A_intersect_B,
      
      
      metric_1 = A_intersect_B,  # inter_area
      metric_2 = A_intersect_B / A_union_B,  # inter_area/A_union_B
      metric_3 = A_intersect_B / pmin(A, B),  # inter_area/min
      metric_4 = A_intersect_B / pmax(A, B),  # inter_area/max
      metric_5 = A_intersect_B / A_intersect_B_perimeter  # inter_area/peri_sq
    ) %>%
    st_drop_geometry()
  
  return(inter)
}


# Step 0: Load all the upazilas
upazilas91 <- st_read("./dataset/geo3_bd1991/geo3_bd1991.shp", quiet=TRUE) %>%
  st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character, tolower) %>%
  mutate(area91 = st_area(geometry) %>% drop_units())

upazilas01 <- st_read("./dataset/geo3_bd2001/geo3_bd2001.shp", quiet=TRUE) %>%
  st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character, tolower) %>%
  mutate(area01 = st_area(geometry) %>% drop_units())

upazilas11 <- st_read("./dataset/geo3_bd2011/geo3_bd2011.shp", quiet=TRUE) %>%
  st_transform(3106) %>%
  select(ADMIN_NAME, contains("IP"), contains("UP"), PARENT) %>%
  clean_names() %>%
  mutate_if(is.character, tolower) %>%
  mutate(area11 = st_area(geometry) %>% drop_units())

upazilas91 <- st_transform(upazilas91, crs=st_crs(upazilas91))
upazilas01 <- st_transform(upazilas01, crs=st_crs(upazilas91))
upazilas11 <- st_transform(upazilas11, crs=st_crs(upazilas91))

# Threshold Sensitivity Analysis
df_91_01 <- compute_overlap_metrics(upazilas91, upazilas01, "ipum1991", "ipum2001") %>% mutate(pair="91_01")
df_01_11 <- compute_overlap_metrics(upazilas01, upazilas11, "ipum2001", "ipum2011") %>% mutate(pair="01_11")
df_91_11 <- compute_overlap_metrics(upazilas91, upazilas11, "ipum1991", "ipum2011") %>% mutate(pair="91_11")

metric_df <- bind_rows(df_91_01, df_01_11, df_91_11)

# plot_df <- bind_rows(df_91_01, df_01_11, df_91_11) %>%
#   select(pair, metric_1, metric_2, metric_3, metric_4) %>%
#   pivot_longer(cols = starts_with("metric"),
#                names_to = "metric",
#                values_to = "value") %>%
#   group_by(metric) %>%
#   arrange(value, .by_group=TRUE) %>%
#   mutate(rank = row_number()) %>%
#   ungroup()
# 
# pmetric_1 <- ggplot(filter(plot_df, metric == "metric_1"),
#        aes(x = rank, y = value, color = pair)) +
#   geom_line() +
#   labs(title = "A ∩ B across all year pairs",
#        x = "Sorted Index",
#        y = "A ∩ B") +
#   theme_minimal()
# 
# pmetric_2 <- ggplot(filter(plot_df, metric == "metric_2"),
#        aes(x = rank, y = value, color = pair)) +
#   geom_line() +
#   labs(title = "A ∩ B / A U B (IoU) across all year pairs",
#        x = "Sorted Index",
#        y = "IoU") +
#   theme_minimal()
# 
# pmetric_3 <- ggplot(filter(plot_df, metric == "metric_3"),
#        aes(x = rank, y = value, color = pair)) +
#   geom_line() +
#   labs(title = "A ∩ B / min(A, B) across all year pairs",
#        x = "Sorted Index",
#        y = "A ∩ B / min(A, B") +
#   theme_minimal()
# 
# 
# pmetric_4 <- ggplot(filter(plot_df, metric == "metric_4"),
#        aes(x = rank, y = value, color = pair)) +
#   geom_line() +
#   labs(title = "A ∩ B / max(A, B) across all year pairs",
#        x = "Sorted Index",
#        y = "A ∩ B / min(A, B)") +
#   theme_minimal()
# 
# (pmetric_1 | pmetric_2) / (pmetric_3 | pmetric_4)

# Step 1: Get overlap edges
edges_91_01 <- get_overlap_pairs(upazilas91, upazilas01, "ipum1991", "ipum2001", "u91", "u01")
edges_01_11 <- get_overlap_pairs(upazilas01, upazilas11, "ipum2001", "ipum2011", "u01", "u11")
edges_91_11 <- get_overlap_pairs(upazilas91, upazilas11, "ipum1991", "ipum2011", "u91", "u11")


# Step 2: Combine edges and build graph
all_edges <- bind_rows(edges_91_01, edges_01_11, edges_91_11) %>% as.matrix()
g <- graph_from_edgelist(all_edges, directed = FALSE)
groups <- components(g)$membership
group_map <- data.frame(node = names(groups), merged_id = groups)

# Step 3: Map original polygons to merged groups
xwalk91 <- data.frame(node = paste0("u91_", upazilas91$ipum1991), ipum1991 = upazilas91$ipum1991) %>%
  left_join(group_map, by = "node") %>%
  select(ipum1991, merged_id)

xwalk01 <- data.frame(node = paste0("u01_", upazilas01$ipum2001), ipum2001 = upazilas01$ipum2001) %>%
  left_join(group_map, by = "node") %>%
  select(ipum2001, merged_id)

xwalk11 <- data.frame(node = paste0("u11_", upazilas11$ipum2011), ipum2011 = upazilas11$ipum2011) %>%
  left_join(group_map, by = "node") %>%
  select(ipum2011, merged_id)

# Step 4: Assign unique merged_ids to unmatched polygons
next_id <- max(group_map$merged_id, na.rm = TRUE) + 1
xwalk91$merged_id[is.na(xwalk91$merged_id)] <- next_id + seq_len(sum(is.na(xwalk91$merged_id))) - 1
xwalk01$merged_id[is.na(xwalk01$merged_id)] <- max(xwalk91$merged_id) + seq_len(sum(is.na(xwalk01$merged_id)))
xwalk11$merged_id[is.na(xwalk11$merged_id)] <- max(xwalk01$merged_id) + seq_len(sum(is.na(xwalk11$merged_id)))

# Step 5: Join merged_id back to sf layers
upazilas91 <- left_join(upazilas91, xwalk91, by = "ipum1991")
upazilas01 <- left_join(upazilas01, xwalk01, by = "ipum2001")
upazilas11 <- left_join(upazilas11, xwalk11, by = "ipum2011")

# Step 6: Combine all and merge by merged_id
combined <- bind_rows(upazilas91, upazilas01, upazilas11)

merged_sf <- combined %>%
  group_by(merged_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Step 7: Final crosswalk
final_crosswalk <- bind_rows(
  xwalk91 %>% mutate(source = "upazilas91"),
  xwalk01 %>% mutate(source = "upazilas01"),
  xwalk11 %>% mutate(source = "upazilas11")
)

# write.csv(final_crosswalk, file = "crosswalk.csv", row.names = FALSE)

# Step 8: Build mapping from ipumXXXX -> actual upazilla names
names91 <- upazilas91 %>% st_drop_geometry() %>% select(ipum1991, name91 = admin_name)
names01 <- upazilas01 %>% st_drop_geometry() %>% select(ipum2001, name01 = admin_name)
names11 <- upazilas11 %>% st_drop_geometry() %>% select(ipum2011, name11 = admin_name)

# Step 9: Add names to final_crosswalk
final_crosswalk_named <- final_crosswalk %>%
  left_join(names91, by = "ipum1991") %>%
  left_join(names01, by = "ipum2001") %>%
  left_join(names11, by = "ipum2011")

# Step 10: Build the 'upazilas' column from names
final_crosswalk_named <- final_crosswalk_named %>%
  group_by(merged_id) %>%
  mutate(
    upazilas = {
      names_vec <- c(name91, name01, name11)
      names_vec <- names_vec[!is.na(names_vec)]
      names_vec <- unique(names_vec)
      paste(names_vec, collapse = "_")
    }
  ) %>%
  select(-name91, -name01, -name11) %>%
  ungroup()

# Step 11: Add geometry
final_crosswalk_geo <- final_crosswalk_named %>%
  left_join(merged_sf, by = "merged_id")

# Optional: select and order relevant columns
final_crosswalk_geo <- final_crosswalk_geo %>%
  select(merged_id, upazilas, geometry, everything())

final_crosswalk_original_geo <- combined %>%
  select(merged_id, admin_name, geometry, ipum1991, ipum2001, ipum2011)

st_write(final_crosswalk_geo, "./output/crosswalk_bdgeo3_91_11.shp")
st_write(final_crosswalk_original_geo, "./output/crosswalk_bdgeo3_91_11_original_geometry.shp")

write.csv(final_crosswalk_geo, "./output/crosswalk_bdgeo3_91_11.csv")
write.csv(final_crosswalk_original_geo, "./output/crosswalk_bdgeo3_91_11_original_geometry.csv")


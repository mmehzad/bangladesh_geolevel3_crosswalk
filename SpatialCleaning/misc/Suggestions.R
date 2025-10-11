library(sf)
library(dplyr)
library(igraph)
library(units)
library(janitor)
library(tidyverse)

options(warn = 1) # surface warnings early

# ---- Parameters ----
threshold <- 0.001  # 0.1% overlap on either side

# ---- Helpers ----

# Ensure consistent projected CRS and valid polygons
prep_sf <- function(x, target_crs = 3106) {
  x %>%
    st_make_valid() %>%
    st_transform(target_crs) %>%
    st_collection_extract("POLYGON", warn = FALSE)
}

# Robust overlap extractor with bbox prefilter and validity handling
get_overlap_pairs <- function(mapA, mapB, idA, idB, prefixA, prefixB, threshold) {
  stopifnot(idA %in% names(mapA), idB %in% names(mapB))
  
  # Quick prefilter with bbox intersects to shrink candidate set
  cand <- st_intersects(st_make_valid(mapA), st_make_valid(mapB), sparse = TRUE)
  
  if (lengths(cand) |> sum() == 0) return(tibble(nodeA = character(), nodeB = character()))
  
  # Build pair index
  pairs <- map2_df(
    .x = seq_len(nrow(mapA)),
    .y = cand,
    ~ if (length(.y) == 0) tibble() else tibble(i = .x, j = .y)
  )
  
  if (nrow(pairs) == 0) return(tibble(nodeA = character(), nodeB = character()))
  
  # Compute intersections only for candidate pairs
  inter <- st_intersection(
    mapA[pairs$i, ] |> st_make_valid(),
    mapB[pairs$j, ] |> st_make_valid()
  )
  
  if (nrow(inter) == 0) return(tibble(nodeA = character(), nodeB = character()))
  
  # Keep polygonal area; drop lines/points
  inter <- inter |> st_collection_extract("POLYGON", warn = FALSE)
  if (nrow(inter) == 0) return(tibble(nodeA = character(), nodeB = character()))
  
  # Areas (numeric)
  inter <- inter |>
    mutate(
      area_intersection = as.numeric(st_area(geometry)),
      # Pull the original IDs; ensure they exist
      idA_val = .data[[idA]],
      idB_val = .data[[idB]]
    ) |>
    # Join back to get full areaA/areaB once per feature
    left_join(
      mapA |>
        mutate(areaA = as.numeric(st_area(geometry))) |>
        st_drop_geometry() |>
        select(!!idA, areaA),
      by = setNames(idA, idA)
    ) |>
    left_join(
      mapB |>
        mutate(areaB = as.numeric(st_area(geometry))) |>
        st_drop_geometry() |>
        select(!!idB, areaB),
      by = setNames(idB, idB)
    ) |>
    mutate(
      propA = ifelse(areaA > 0, area_intersection / areaA, 0),
      propB = ifelse(areaB > 0, area_intersection / areaB, 0)
    ) |>
    filter(propA > threshold | propB > threshold) |>
    mutate(
      nodeA = paste0(prefixA, "_", .data[[idA]]),
      nodeB = paste0(prefixB, "_", .data[[idB]])
    ) |>
    st_drop_geometry() |>
    distinct(nodeA, nodeB)
  
  inter[, c("nodeA", "nodeB")]
}

# Assign fresh merged ids for NAs without collisions
assign_missing_ids <- function(xwalk, start_id) {
  n_miss <- sum(is.na(xwalk$merged_id))
  if (n_miss == 0) return(xwalk)
  xwalk$merged_id[is.na(xwalk$merged_id)] <- seq.int(from = start_id, length.out = n_miss)
  xwalk
}

# ---- Load & standardize ----

u91 <- st_read("./dataset/geo3_bd1991/geo3_bd1991.shp", quiet=TRUE) |>
  clean_names() |>
  prep_sf(3106)
u01 <- st_read("./dataset/geo3_bd2001/geo3_bd2001.shp", quiet=TRUE) |>
  clean_names() |>
  prep_sf(3106)
u11 <- st_read("./dataset/geo3_bd2011/geo3_bd2011.shp", quiet=TRUE) |>
  clean_names() |>
  prep_sf(3106)

# ---- Ensure we know the ID columns ----
# Adjust these if your actual column names differ
id91 <- (names(u91) |> keep(~ grepl("ipum.*1991", .x)) |> first()) %||% "ipum1991"
id01 <- (names(u01) |> keep(~ grepl("ipum.*2001", .x)) |> first()) %||% "ipum2001"
id11 <- (names(u11) |> keep(~ grepl("ipum.*2011", .x)) |> first()) %||% "ipum2011"

stopifnot(id91 %in% names(u91), id01 %in% names(u01), id11 %in% names(u11))

# Optional: unify simple name column if present (admin_name sometimes differs)
nm91 <- (names(u91) |> keep(~ grepl("admin", .x))) |> first()
nm01 <- (names(u01) |> keep(~ grepl("admin", .x))) |> first()
nm11 <- (names(u11) |> keep(~ grepl("admin", .x))) |> first()

# ---- Edges ----
edges_91_01 <- get_overlap_pairs(u91, u01, id91, id01, "u91", "u01", threshold)
edges_01_11 <- get_overlap_pairs(u01, u11, id01, id11, "u01", "u11", threshold)
edges_91_11 <- get_overlap_pairs(u91, u11, id91, id11, "u91", "u11", threshold)

all_edges <- bind_rows(edges_91_01, edges_01_11, edges_91_11) |> distinct()

if (nrow(all_edges) == 0) stop("No overlaps found above threshold. Try lowering `threshold` or checking IDs/CRS.")

# ---- Graph ----
edge_mat <- as.matrix(all_edges[, c("nodeA", "nodeB")])
g <- graph_from_edgelist(edge_mat, directed = FALSE)

groups <- components(g)$membership
group_map <- tibble(node = names(groups), merged_id = as.integer(groups))

# ---- Crosswalks ----
xwalk91 <- tibble(node = paste0("u91_", u91[[id91]]), !!id91 := u91[[id91]]) |>
  left_join(group_map, by = "node") |>
  select(all_of(id91), merged_id)

xwalk01 <- tibble(node = paste0("u01_", u01[[id01]]), !!id01 := u01[[id01]]) |>
  left_join(group_map, by = "node") |>
  select(all_of(id01), merged_id)

xwalk11 <- tibble(node = paste0("u11_", u11[[id11]]), !!id11 := u11[[id11]]) |>
  left_join(group_map, by = "node") |>
  select(all_of(id11), merged_id)

# Fill missing merged_ids without collisions
max_id <- max(group_map$merged_id, na.rm = TRUE)
xwalk91 <- assign_missing_ids(xwalk91, max_id + 1)
xwalk01 <- assign_missing_ids(xwalk01, max(xwalk91$merged_id) + 1)
xwalk11 <- assign_missing_ids(xwalk11, max(xwalk01$merged_id) + 1)

# ---- Attach IDs back to SF ----
u91 <- u91 |> left_join(xwalk91, by = setNames(id91, id91))
u01 <- u01 |> left_join(xwalk01, by = setNames(id01, id01))
u11 <- u11 |> left_join(xwalk11, by = setNames(id11, id11))

# ---- Combine & dissolve ----
combined <- bind_rows(
  u91 |> mutate(src_year = 1991),
  u01 |> mutate(src_year = 2001),
  u11 |> mutate(src_year = 2011)
)

# Dissolve by merged_id (make valid first to avoid topology errors)
merged_sf <- combined |>
  st_make_valid() |>
  group_by(merged_id) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid()

# ---- Final crosswalk with names ----
names91 <- if (!is.null(nm91)) u91 |> st_drop_geometry() |> select(!!id91, name91 = all_of(nm91)) else NULL
names01 <- if (!is.null(nm01)) u01 |> st_drop_geometry() |> select(!!id01, name01 = all_of(nm01)) else NULL
names11 <- if (!is.null(nm11)) u11 |> st_drop_geometry() |> select(!!id11, name11 = all_of(nm11)) else NULL

final_crosswalk <- bind_rows(
  xwalk91 |> mutate(source = "upazilas1991"),
  xwalk01 |> mutate(source = "upazilas2001"),
  xwalk11 |> mutate(source = "upazilas2011")
)

final_crosswalk_named <- final_crosswalk |>
  { if (!is.null(names91)) left_join(., names91, by = setNames(id91, id91)) else . } |>
  { if (!is.null(names01)) left_join(., names01, by = setNames(id01, id01)) else . } |>
  { if (!is.null(names11)) left_join(., names11, by = setNames(id11, id11)) else . } |>
  group_by(merged_id) |>
  mutate(
    upazilas = {
      nms <- c(cur_data_all()$name91, cur_data_all()$name01, cur_data_all()$name11)
      nms <- unique(nms[!is.na(nms) & nchar(nms) > 0])
      if (length(nms) == 0) NA_character_ else paste(nms, collapse = "_")
    }
  ) |>
  ungroup() |>
  select(merged_id, upazilas, everything(), -matches("^name(91|01|11)$"))

final_crosswalk_geo <- final_crosswalk_named |>
  distinct(merged_id, .keep_all = TRUE) |>  # one row per merged_id for the geometry table
  left_join(merged_sf, by = "merged_id")

# Optional writes:
# st_write(merged_sf, "bd_upazilas_harmonized.gpkg", layer = "harmonized", delete_dsn = TRUE)
# write.csv(final_crosswalk_named, "bd_upazila_crosswalk.csv", row.names = FALSE)

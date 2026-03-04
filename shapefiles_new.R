#r script for figuring out data analysis
library(here)
library(ggplot2)
library(sf)
library(dplyr)

folder_path <- "Shapefiles"

#fix da code - footprints wasnt working here before
#which(!sf::st_is_valid(footprints))
#fix_footprints <- st_make_valid(footprints)
#dedup_footprints <- distinct(fix_footprints, bank_id, .keep_all = T)
#dedup_footprints <- dedup_footprints %>% 
#  select(-bank_name)

#st_write(footprints, dsn="bank_footprints.gpkg", layer='bank_footprints')

foot_gkpg <- here("raw_data/bank_footprints.gpkg")
footprints <- st_read(foot_gkpg, layer = "bank_footprints", quiet = TRUE)

ribits_data_simplified <- read.csv("raw_data/ribits_data_simplified.csv")
dedup_ribits <- distinct(ribits_data_simplified, bank_id, .keep_all = T)


#split it to just look at one state - South Dakota - and its banks
SD_ribits <- dedup_ribits %>%
  filter(state_list == "SD") %>%
  #filter so that only banks with known footprints are provided 
  inner_join(footprints, SD_ribits, by = "bank_id")

#read this as an sf object  
SD_ribits <- st_as_sf(SD_ribits)

# Required packages
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
options(scipen = 999)  # nicer numeric printing

# -------------------------
# Assumptions / inputs
# -------------------------
# - SD_ribits is available in your workspace as an sf object (polygons) called SD_ribits.
#   If not, replace the object with st_read("path/to/SD_ribits.shp") accordingly.
# - Fish polygons: use the first 20 shapefiles from "Shapefiles" folder (as in your earlier workflow).
#   If you already have a combined fish sf object, skip the reading step and set fish_combined <- <your object>.

# -------------------------
# 0. Quick safety checks
# -------------------------
if (!exists("SD_ribits") || !inherits(SD_ribits, "sf")) {
  stop("SD_ribits not found as an sf object. Load it first, e.g. SD_ribits <- st_read('path/to/SD_ribits.shp').")
}
# Ensure SD_ribits is polygons
geom_types <- unique(st_geometry_type(SD_ribits))
if (!any(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
  stop("SD_ribits does not appear to contain POLYGON / MULTIPOLYGON geometries.")
}

# -------------------------
# 1. Read fish shapefiles (first 20) if needed
# -------------------------
shp_files <- list.files("Shapefiles", pattern = "\\.shp$", full.names = TRUE)
if (length(shp_files) == 0) stop("No .shp files found in 'Shapefiles' directory.")
files20 <- head(shp_files, 20)

fish_list <- map(files20, ~ st_read(.x, quiet = TRUE) %>%
                   mutate(species = tools::file_path_sans_ext(basename(.x))))
fish_combined <- bind_rows(fish_list)

# If you already have a fish object, comment out above and set:
# fish_combined <- your_fish_sf

# -------------------------
# 2. CRS: make sure everything matches (project to a metric CRS for area)
# -------------------------
# We want an equal-area or metric CRS (units in metres) for area calculations.
# If SD_ribits has a CRS, use that; otherwise set one (user must know correct CRS).
if (is.na(st_crs(SD_ribits))) stop("SD_ribits has no CRS. Set it with st_set_crs(...).")
if (is.na(st_crs(fish_combined))) stop("fish_combined has no CRS. Set it with st_set_crs(...).")

# Choose target CRS: use SD_ribits CRS if it's metric, otherwise transform to a suitable metric CRS.
# A simple heuristic: if SD_ribits is geographic (EPSG:4326), transform to World Mercator (EPSG:3857)
target_crs <- st_crs(SD_ribits)
if (target_crs$epsg == 4326 || st_is_longlat(SD_ribits)) {
  target_crs <- st_crs(3857)   # metric; you can choose a more local projection for better area accuracy
}

SD_ribits <- st_transform(SD_ribits, target_crs)
fish_combined <- st_transform(fish_combined, target_crs)

# -------------------------
# 3. Union fish polygons by species (prevents double counting / speeds up intersection)
# -------------------------
# Some shapefiles might have multiple polygons per species; we create one MULTIPOLYGON per species.
fish_by_species <- fish_combined %>%
  group_by(species) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()

# Optionally make valid (fix geometry issues)
fish_by_species$geom <- st_make_valid(fish_by_species$geometry)
SD_ribits$geom <- st_make_valid(SD_ribits$geometry)

# -------------------------
# 4. Compute intersections (bank x species)
# -------------------------
# Approach: intersect SD_ribits with each species polygon, compute area of intersection.
# We'll create a long table: each row = (bank_id, species, area_intersection_m2)
# Ensure an identifier column exists for banks
if (!"bank_id" %in% names(SD_ribits)) {
  SD_ribits <- SD_ribits %>% mutate(bank_id = row_number())
}

# Compute total bank area once
SD_ribits <- SD_ribits %>% mutate(bank_area_m2 = as.numeric(st_area(st_geometry(.))))

# Function: intersect one species with all banks (returns tibble)
intersect_species_with_banks <- function(spec_row) {
  sp_name <- spec_row$species
  sp_geom <- spec_row$geometry[[1]]
  # if species geometry is empty, return nothing
  if (is.null(sp_geom) || length(sp_geom) == 0) return(tibble())
  # Intersect (this returns geometries where they overlap)
  inter <- st_intersection(SD_ribits, st_sf(species = sp_name, geometry = st_sfc(sp_geom, crs = st_crs(SD_ribits))))
  if (nrow(inter) == 0) return(tibble())
  inter <- inter %>%
    mutate(inter_area_m2 = as.numeric(st_area(st_geometry(.)))) %>%
    st_drop_geometry() %>%
    select(bank_id, species, inter_area_m2)
  return(inter)
}

# Apply for all species and row-bind results
inter_list <- map(seq_len(nrow(fish_by_species)), function(i) {
  intersect_species_with_banks(fish_by_species[i, ])
})
inter_df <- bind_rows(inter_list)

# -------------------------
# 5. Summaries: per-bank, per-species, and overall
# -------------------------
# a) Per-bank: total intersect area across all species & percent of bank overlapped
bank_summary <- inter_df %>%
  group_by(bank_id) %>%
  summarise(total_inter_area_m2 = sum(inter_area_m2), .groups = "drop") %>%
  right_join(SD_ribits %>% st_drop_geometry() %>% select(bank_id, bank_area_m2), by = "bank_id") %>%
  mutate(total_inter_area_m2 = replace_na(total_inter_area_m2, 0),
         pct_bank_overlapped = 100 * total_inter_area_m2 / bank_area_m2)

# b) Per-bank-per-species table (how much of bank is overlapped by each species)
bank_species_table <- inter_df %>%
  left_join(SD_ribits %>% st_drop_geometry() %>% select(bank_id, bank_area_m2), by = "bank_id") %>%
  mutate(pct_of_bank_by_species = 100 * inter_area_m2 / bank_area_m2)

# c) Per-species: how many m2 of fish range intersects any bank and how many banks touched
species_summary <- inter_df %>%
  group_by(species) %>%
  summarise(area_intersecting_banks_m2 = sum(inter_area_m2),
            n_banks = n_distinct(bank_id),
            .groups = "drop") %>%
  arrange(desc(area_intersecting_banks_m2))

# Quick prints
cat("Per-bank summary (first 10 rows):\n")
print(head(bank_summary, 10))
cat("\nPer-species summary (top 10 species):\n")
print(head(species_summary, 10))

# -------------------------
# 6. Map: banks colored by percent overlapped (any species)
# -------------------------
# Join percent back to SD_ribits for plotting
SD_ribits_plot <- SD_ribits %>%
  left_join(bank_summary %>% select(bank_id, pct_bank_overlapped), by = "bank_id") %>%
  mutate(pct_bank_overlapped = replace_na(pct_bank_overlapped, 0))

p1 <- ggplot() +
  geom_sf(data = fish_by_species, aes(fill = species), alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_sf(data = SD_ribits_plot, fill = "black", alpha = 1, size = 10) +
  
  labs(title = "SD_ribits banks colored by percent area overlapped by fish distributions",
       subtitle = "Fish polygons shown faintly in background (unioned by species)") +
  theme_minimal()

print(p1)

ggplot() +
  geom_sf(data = fish_by_species, aes(fill = species), alpha = 0.25, color = NA, show.legend = F) +
  geom_sf(data = SD_ribits, fill = "white", color = "black", size = 1) +
  coord_sf(xlim = c(-100, -90),
                     ylim = c(40, 45),
                     expand = FALSE) +
  scale_fill_viridis_d() +
  theme_minimal()


# -------------------------
# 7. Optional: map intersections for selected species (faceted)
# -------------------------
# Build an intersection sf object for mapping: SD_ribits intersected with all species (only non-empty)
# We'll reconstruct geometries by doing st_intersection between SD_ribits and fish_by_species, keeping area info.
intersections_sf_list <- map(seq_len(nrow(fish_by_species)), function(i) {
  sp_name <- fish_by_species$species[i]
  sp_geom <- fish_by_species$geometry[i]
  inter_sf <- st_intersection(SD_ribits %>% select(bank_id), st_sf(species = sp_name, geometry = st_sfc(sp_geom, crs = st_crs(SD_ribits))))
  if (nrow(inter_sf) == 0) return(NULL)
  inter_sf <- inter_sf %>%
    mutate(inter_area_m2 = as.numeric(st_area(st_geometry(.))))
  return(inter_sf)
})
intersections_sf <- bind_rows(intersections_sf_list)

# If you want a faceted map showing intersection geometries by species:
if (nrow(intersections_sf) > 0) {
  p2 <- ggplot() +
    geom_sf(data = SD_ribits, fill = NA, color = "grey60") +
    geom_sf(data = intersections_sf, aes(fill = species), color = NA, alpha = 0.6) +
    facet_wrap(~ species, ncol = 3) +
    labs(title = "Bank × Species intersection areas (each panel = species)") +
    theme_minimal() +
    theme(legend.position = "none")
  print(p2)
} else {
  message("No intersection geometries to map (intersections_sf is empty).")
}

# -------------------------
# 8. Save results (optional)
# -------------------------
# write.csv(bank_summary, "bank_overlap_summary.csv", row.names = FALSE)
# write.csv(species_summary, "species_overlap_summary.csv", row.names = FALSE)
# st_write(intersections_sf, "bank_species_intersections.geojson", delete_dsn = TRUE)

# -------------------------
# Notes / tips
# -------------------------
# - We union fish polygons by species to avoid double-counting overlaps where multiple fish polygons for the same species overlap the same bank area.
# - If two species ranges overlap the same bank area, that area will be counted separately for each species (i.e., overlapping by species is not exclusive).
# - For more accurate area calculations in large extents, pick a locally appropriate equal-area projection (e.g., an Albers or UTM zone). Replace EPSG:3857 with a better EPSG for your region.
# - For extremely large datasets, consider simplifying geometries (st_simplify) or processing species one-by-one and writing results out to disk.
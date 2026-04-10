#r script for figuring out data analysis
library(here)
library(ggplot2)
library(sf)
library(dplyr)

folder_path <- "Shapefiles"

#fix da code - footprints wasnt working here before
which(!sf::st_is_valid(footprints))
fix_footprints <- st_make_valid(footprints)
dedup_footprints <- distinct(fix_footprints, bank_id, .keep_all = T)
dedup_footprints <- dedup_footprints %>% 
  select(-bank_name)

#st_write(dedup_footprints, dsn="bank_footprints.gpkg", layer='bank_footprints')



ggplot(combined) +
  geom_sf(aes(fill = species), color = NA) +
  theme_minimal() +
  theme(legend.position = "none")



# Required packages
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)   # for map functions

# -------------------------
# 1. Read dedup_centroids
# -------------------------
# Try common options: if it's already an sf object in the global environment, use it.
if (exists("dedup_centroids") && inherits(dedup_centroids, "sf")) {
  banks <- dedup_centroids
} else {
  # Otherwise, try reading from likely file names. Update path/filename if needed.
  # Try shapefile or geopackage
  possible_files <- c("dedup_centroids.shp", "dedup_centroids.geojson", "dedup_centroids.gpkg", "dedup_centroids.gpkg")
  found <- NULL
  for (f in possible_files) {
    if (file.exists(f)) { found <- f; break }
  }
  if (!is.null(found)) {
    banks <- st_read(found, quiet = TRUE)
  } else if (file.exists("dedup_centroids.csv")) {
    # If a CSV with WKT column named 'geom'
    df <- read.csv("dedup_centroids.csv", stringsAsFactors = FALSE)
    if ("geom" %in% names(df)) {
      banks <- st_as_sfc(df$geom, crs = 4326) %>% st_sf(df %>% select(-geom), geometry = .)
    } else stop("Couldn't find 'dedup_centroids' file. If it's a CSV provide a 'geom' WKT column or load it as an sf object named dedup_centroids.")
  } else {
    stop("dedup_centroids not found in workspace nor common files. Either load it as an sf object named 'dedup_centroids' or place the file in working directory.")
  }
}

# Ensure banks has an id column for reporting (create if missing)
if (!"bank_id" %in% names(banks)) {
  banks$bank_id <- seq_len(nrow(banks))
}

# -------------------------
# 2. Read first 20 fish shapefiles
# -------------------------
shp_files <- list.files("Shapefiles", pattern = "\\.shp$", full.names = TRUE)
if (length(shp_files) == 0) stop("No .shp files found in 'Shapefiles' directory.")

files20 <- head(shp_files, 20)

# Read and tag with species (basename without extension)
fish_list <- map(files20, ~ st_read(.x, quiet = TRUE) %>%
                   mutate(species = tools::file_path_sans_ext(basename(.x))))
# Optionally inspect geometry types: st_geometry_type(fish_list[[1]])

# Bind rows (works best when attribute names are similar; bind_rows will fill missing cols with NA)
fish_combined <- bind_rows(fish_list)

# -------------------------
# 3. Ensure matching CRS
# -------------------------
# If one of them has NA CRS, warn.
if (is.na(st_crs(banks))) stop("banks has no CRS defined. Set it with st_set_crs(...).")
if (is.na(st_crs(fish_combined))) stop("fish_combined has no CRS defined. Set it with st_set_crs(...).")

# Transform fish to banks CRS (safer than transforming points to polygon CRS)
if (st_crs(banks) != st_crs(fish_combined)) {
  fish_combined <- st_transform(fish_combined, st_crs(banks))
}

# -------------------------
# 4. Test overlaps / intersections
# -------------------------
# Use st_intersects which works for points/polygons/polygons vs polygons.
# Result: an integer list where element i lists indices of fish_combined that intersect banks[i, ]
ix <- st_intersects(banks, fish_combined, sparse = TRUE)

# Add boolean flag: does this bank intersect any fish polygon?
banks$overlaps_any_fish <- lengths(ix) > 0

# Add a column with the species names (comma-separated) that overlap each bank
banks$overlapping_species <- sapply(ix, function(idx) {
  if (length(idx) == 0) return(NA_character_)
  unique_species <- unique(fish_combined$species[idx])
  paste(unique_species, collapse = ", ")
})

# -------------------------
# 5. Summary output
# -------------------------
# Print a small summary table
summary_table <- banks %>%
  st_drop_geometry() %>%
  select(bank_id, overlaps_any_fish, overlapping_species)

print(head(summary_table, 20))

# Count how many banks overlap at least one fish polygon
cat("Banks overlapping any fish polygon:", sum(banks$overlaps_any_fish), "of", nrow(banks), "\n")

# If you want counts by species (how many banks intersect each species):
# For that we turn the ix list into a long table:
if (length(ix) > 0) {
  bank_to_species <- tibble(
    bank_id = rep(banks$bank_id, times = lengths(ix)),
    fish_idx = unlist(ix)
  ) %>%
    left_join(
      tibble(fish_idx = seq_len(nrow(fish_combined)), species = fish_combined$species),
      by = "fish_idx"
    ) %>%
    distinct(bank_id, species)   # ensure unique bank-species pairs
  
  species_counts <- bank_to_species %>% count(species, name = "n_banks") %>% arrange(desc(n_banks))
  print(species_counts)
} else {
  message("No intersections found; ix is empty.")
}

# -------------------------
# 6. Plot a map
# -------------------------
# Base map: fish polygons in light fill, banks as points colored by overlap.
# If banks are polygons, geom_sf will plot them as polygons; this still works.

ggplot() +
  geom_sf(data = fish_combined, aes(fill = species), alpha = 0.25, color = NA, show.legend = FALSE) +
  geom_sf(data = banks, aes(color = overlaps_any_fish), size = 1.5) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Banks and whether they overlap any fish distribution (first 20 fish layers)",
       color = "Overlaps fish?") +
  theme_minimal()

# -------------------------
# Notes / tips
# -------------------------
# - If banks are POINTS, st_intersects is equivalent to testing if point is inside polygon.
# - If banks are POLYGONS and you want only 'true overlaps' (not touching at boundary), consider st_overlaps or st_relate with pattern.
# - For large datasets, use st_join(banks, fish_combined, join = st_intersects) or spatial indexing via sf; st_intersects uses an index internally.
# - If fish_combined is very large, consider simplifying polygons (st_simplify) or using an R-tree spatial index (sf does that automatically).


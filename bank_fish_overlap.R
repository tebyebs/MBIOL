# Bank-Fish Overlap Example code
# =============================================================================

# For each fish species, we need to check which mitigation banks
# fall within its range. We can do this two ways:
# 1. Centroid: does the bank's center point fall in the fish range? (yes/no)
# 2. Footprint:  what % of the bank's area overlaps the fish range? (0-100%)
#
# There are tons of shapefiles, so it is important that we load one fish
# species at a time to avoid running out of memory.
#
# Outputs (saved to output/ folder):
#   - centroid_fish_overlap.csv    every bank-species pair where they overlap
#   - footprint_fish_overlap.csv   same, but with % area overlap
#   - bank_species_summary.csv     one row per bank: how many species + which ones
#
# =============================================================================

# Load packages
library(sf)

# Turn off spherical geometry, as some bank shapes have errors that
# cause crashes with the S2 engine.
sf_use_s2(FALSE)
options(warn = -1)  # suppress "planar coordinates" messages

# Set up file paths, change for your own files
geom_path <- "raw_data/bank_locations.gpkg"
gpkg_path <- "raw_data/bank_footprints.gpkg"
fish_dir  <- "Shapefiles"
out_dir   <- "output"

# Create output directory
dir.create(out_dir, showWarnings = FALSE)

# Load bank data

# Centroids
centroids  <- st_read(geom_path, layer = "bank_geoms", quiet = TRUE)
centroids <- distinct(centroids, bank_id, .keep_all = T)
# Footprints
footprints <- st_read(gpkg_path, layer = "bank_footprints", quiet = TRUE)

# Count how many centroids and footprint files we have
cat(sprintf("  %d centroids, %d footprints\n", nrow(centroids), nrow(footprints)))

# The fish data is in NAD83, banks are in WGS84, so need to match
centroids  <- st_transform(centroids, 4269)
footprints <- st_transform(footprints, 4269)

# Some footprint shapes have minor geometry errors, we fix here
footprints <- st_make_valid(footprints)

# Calculate each footprint's area (in m2) using an equal-area projection
# We need this later to compute % overlap
footprints$area_m2 <- as.numeric(st_area(st_transform(footprints, 5070)))
#filter out larger banks (>50km2)
footprints <- footprints %>%
  filter(area_m2 <= 50 * 1e6)

# Get list of all fish species files and print how many
shp_files <- list.files(fish_dir, pattern = "[.]shp$", full.names = TRUE)
cat(sprintf("Found %d fish species shapefiles\n\n", length(shp_files)))


# ANALYSIS 1: Centroid overlap
# For each species: load its range, check which bank center points fall inside.

# We store results in a list and combine at the end
centroid_results <- list()

# Then create the loop: go through each fish species one at a time, load its shapefile, and check for overlaps
for (i in seq_along(shp_files)) {

  # Load one species
  fish <- st_read(shp_files[i], quiet = TRUE)
  species     <- fish$hold_SCIEN[1]
  common_name <- fish$hold_G_PRI[1]

  # Progress update every 50 species
  if (i %% 50 == 1) cat(sprintf("  [%d/%d] %s\n", i, length(shp_files), species))

  # Only keep native occurrences (check if there are any exotic/introduced?)
  fish <- fish[fish$hold_ORIGI == "Native", ]
  if (nrow(fish) == 0) next

  # Merge all the HUC polygons into one shape for the whole species range
  fish_range <- st_union(fish)

  # Which bank centroids fall inside this range?
  overlap <- st_intersects(centroids, fish_range, sparse = TRUE)
  hit_banks <- which(lengths(overlap) > 0)

  if (length(hit_banks) > 0) {
    centroid_results[[length(centroid_results) + 1]] <- data.frame(
      bank_id     = centroids$bank_id[hit_banks],

      species     = species,
      common_name = common_name,
      stringsAsFactors = FALSE
    )
  }
}

#############################################



# combine results into one data.frame (or empty df if none)
if (length(centroid_results) > 0) {
  centroid_results_df <- bind_rows(centroid_results)
} else {
  centroid_results_df <- data.frame(
    bank_id = character(0),
    bank_name = character(0),
    state = character(0),
    district = character(0),
    species = character(0),
    common_name = character(0),
    stringsAsFactors = FALSE
  )
}

# inspect
print(head(centroid_results_df))








# bind together
centroid_df <- do.call(rbind, centroid_results)

cat(sprintf("\n  %d bank-species pairs found\n", nrow(centroid_df)))
cat(sprintf("  %d banks overlap with at least one species\n", length(unique(centroid_df$bank_id))))
cat(sprintf("  %d species overlap with at least one bank\n", length(unique(centroid_df$species))))

write.csv(centroid_df, file.path(out_dir, "centroid_fish_overlap.csv"), row.names = FALSE)

# =============================================================================
# ANALYSIS 2: Footprint overlap (% area)

# Same loop, but now we compute what fraction of each bank's footprint
# falls within the fish range. This is slower because it involves
# computing geometry intersections and areas.

# store list
footprint_results <- list()

for (i in seq_along(shp_files)) {

  fish <- st_read(shp_files[i], quiet = TRUE)
  species     <- fish$hold_SCIEN[1]
  common_name <- fish$hold_G_PRI[1]

  if (i %% 50 == 1) cat(sprintf("  [%d/%d] %s\n", i, length(shp_files), species))

  fish <- fish[fish$hold_ORIGI == "Native", ]
  if (nrow(fish) == 0) next

  fish_range <- st_union(fish)

  # Quick filter: only check footprints whose bounding box overlaps this species.
  # This avoids expensive geometry calculations for banks far from the range.
  in_bbox <- st_intersects(footprints, st_as_sfc(st_bbox(fish_range)), sparse = TRUE)
  nearby <- which(lengths(in_bbox) > 0)
  if (length(nearby) == 0) next

  nearby_fp <- footprints[nearby, ]

  # Now check actual geometric overlap (not just bounding box)
  overlap <- st_intersects(nearby_fp, fish_range, sparse = TRUE)
  hit_banks <- which(lengths(overlap) > 0)
  if (length(hit_banks) == 0) next

  hit_fp <- nearby_fp[hit_banks, ]

  # Calculate the overlapping area
  intersection <- suppressWarnings(st_intersection(hit_fp, fish_range))
  overlap_area <- as.numeric(st_area(st_transform(intersection, 5070)))
  pct <- round(100 * overlap_area / hit_fp$area_m2, 2)

  footprint_results[[length(footprint_results) + 1]] <- data.frame(
    bank_id     = hit_fp$BANK_ID,
    bank_name   = hit_fp$BANK_NAME,
    species     = species,
    common_name = common_name,
    pct_overlap = pct,
    stringsAsFactors = FALSE
  )
}

# combine results
footprint_df <- do.call(rbind, footprint_results)

cat(sprintf("\n  %d bank-species pairs found\n", nrow(footprint_df)))
cat(sprintf("  %d banks overlap with at least one species\n", length(unique(footprint_df$bank_id))))

write.csv(footprint_df, file.path(out_dir, "footprint_fish_overlap.csv"), row.names = FALSE)

# =============================================================================
# BANK-LEVEL SUMMARY
# One row per bank: how many species does it overlap with, and which ones?

# Group centroid data by bank ID
banks <- split(centroid_df, centroid_df$bank_id)

# For each bank, create a summary row with:
#   - bank_id, bank_name, state, district (basic info)
#   - n_species: count of species that overlap this bank
#   - species_list: all species names, sorted and separated by semicolons
#   - common_name_list: all species common names, sorted and separated by semicolons
bank_summary <- do.call(rbind, lapply(banks, function(b) {
  data.frame(
    bank_id          = b$bank_id[1],           # Take first row (all same bank)
    bank_name        = b$bank_name[1],
    state            = b$state[1],
    district         = b$district[1],
    n_species        = nrow(b),                # How many species rows for this bank?
    species_list     = paste(sort(b$species), collapse = "; "),     # Join species with ";"
    common_name_list = paste(sort(b$common_name), collapse = "; "), # Join names with ";"
    stringsAsFactors = FALSE
  )
}))

# Sort by number of species (most first)
bank_summary <- bank_summary[order(-bank_summary$n_species), ]
rownames(bank_summary) <- NULL

# Add mean % overlap from footprint data (where available)
if (nrow(footprint_df) > 0) {
  mean_overlap <- aggregate(pct_overlap ~ bank_id, data = footprint_df, FUN = mean)
  names(mean_overlap)[2] <- "mean_pct_overlap"
  mean_overlap$mean_pct_overlap <- round(mean_overlap$mean_pct_overlap, 1)
  bank_summary <- merge(bank_summary, mean_overlap, by = "bank_id", all.x = TRUE)
  bank_summary <- bank_summary[order(-bank_summary$n_species), ]
}

write.csv(bank_summary, file.path(out_dir, "bank_species_summary.csv"), row.names = FALSE)

cat(sprintf("  Saved: output/bank_species_summary.csv (%d banks)\n\n", nrow(bank_summary)))

# Print top banks
print(
  head(bank_summary[, c("bank_id", "bank_name", "state", "n_species")], 10),
  row.names = FALSE
)

options(warn = 0)

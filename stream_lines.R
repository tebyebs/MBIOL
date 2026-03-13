#stream lines script
#goal - map the fish biodiversity polygons such that they correspond to a hydrological stream

library(nhdplusTools)
library(sf)
library(dplyr)
library(ggplot2)
library(here)
library(terra)    # raster operations
library(stars) 

# ---- USER SETTINGS ----
fish_shp <- "Shapefiles/Acantharchus_pomotis.shp"  # might need to change this to be shapefile specific
outdir <- "nhd_download"                             # where nhdplusTools will cache/download
buffer_m <- 100                                              # buffer around flowlines in meters
plot_crs <- 5070                                             # US Albers (EPSG:5070) - metric
# ------------------------

# read stream network (you must download NHD/NHDPlus flowlines beforehand)
#this might break r

#flowlines <- st_read("nhdplus/a0000001d.gdbtable")
flowfile <- "nhdplus/a0000001d.gdbtable"
flowlines <- terra::vect(flowfile, proxy = T)

#converts the fish data into terra for use in analysis
shp_files <- list.files("Shapefiles", pattern="\\.shp$", full.names=TRUE)
fish20 <- head(shp_files, 20)
fish_list <- lapply(fish20, vect)

plot(fish_combined, "hold_SCIEN")

fish_combined <- do.call(rbind, fish_list)

# if you run this as is will crash R
# going to have to subset this 
#query(flowlines, filter=fish_list[[1]], what="")


#trying with sf
nhd_dsn <- "nhdplus/a0000001d.gdbtable" 
st_layers(nhd_dsn)
st_read(nhd_dsn, layer = "NetworkNHDFlowline", n_max = 1)

      
nhd_layer <- "NetworkNHDFlowline"         # set based on st_layers output
stream_order_field <- "StreamOrder" # replace with the actual field name you found
min_order <- 3                      # drop small headwater streams (tweak as needed)

# your fish polygon (an sf object). If you have many species, put this inside a loop and replace fish_sf per species.
fish_sf <- st_read("Shapefiles/Lepomis_gulosus.shp")   # must be an sf POLYGON or MULTIPOLYGON

# get bbox and expand a bit (5 km buffer) to be safe when reading
bbox <- st_as_sfc(st_bbox(fish_sf))
bbox_expanded <- st_buffer(st_transform(bbox, 3857), 5000) |> st_transform(st_crs(fish_sf))

# Convert bbox to WKT for the GDAL filter
bbox_wkt <- st_as_text(st_geometry(bbox_expanded)[[1]])

# Try reading only the flowlines intersecting the bbox (uses GDAL spatial filter).
# NOTE: argument name differs across versions; sf supports 'wkt_filter' in many versions.
nhd_chunk <- tryCatch({
  st_read(dsn = nhd_dsn, layer = nhd_layer, wkt_filter = bbox_wkt, quiet = TRUE)
}, error = function(e) {
  message("wkt_filter not supported or failed — falling back to reading minimal header and filtering after read.")
  # fallback: read entire layer? Better: read with st_read(..., query = ...) if GDAL SQL available for your driver.
  NULL
})

if (is.null(nhd_chunk)) {
  stop("Could not apply spatial filter via st_read. Consider using a database (GPKG) + SQL or terra::vect with ext, or ask for help with your driver.")
}

# If the stream order field exists, filter to remove tiny streams:
if (stream_order_field %in% names(nhd_chunk)) {
  nhd_chunk <- nhd_chunk %>% filter(.data[[stream_order_field]] >= min_order)
} else {
  message("Stream order field not found in the chunk — consider filtering by FCODE/FTYPE or other attribute after inspecting names(nhd_chunk).")
}

# If you want to drastically reduce geometry complexity, project to metric CRS and simplify
target_crs <- 3857   # or choose a local UTM/Albers for better area/distance accuracy
nhd_chunk <- st_transform(nhd_chunk, crs = target_crs)
fish_sf_proj <- st_transform(fish_sf, crs = target_crs)

# simplify geometry (tolerance in meters). 50–200 m are reasonable values for "rough overlap".
nhd_chunk_simpl <- st_simplify(nhd_chunk, dTolerance = 50)  # tune tolerance

# optionally drop attributes you don't need to reduce memory
nhd_chunk_simpl <- nhd_chunk_simpl %>% select(geometry)

# Now you can test intersection (fast, since dataset is small)
intersects <- st_intersects(fish_sf_proj, nhd_chunk_simpl, sparse = TRUE)
hit_lines <- nhd_chunk_simpl[unique(unlist(intersects)), ]

# OR compute intersection geometry if needed (beware of complexity)
inter_geom <- st_intersection(st_union(fish_sf_proj), st_union(hit_lines))

# Plot quick check
plot(st_geometry(st_transform(nhd_chunk_simpl, 4326)), col = "blue")
plot(st_geometry(fish_sf), add = TRUE, border = "red")
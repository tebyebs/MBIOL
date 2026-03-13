#stream lines script
#goal - map the fish biodiversity polygons such that they correspond to a hydrological stream

library(nhdplusTools)
library(sf)
library(dplyr)
library(ggplot2)
library(here)

# ---- USER SETTINGS ----
fish_shp <- "Shapefiles/Acantharchus_pomotis.shp"  # might need to change this to be shapefile specific
outdir <- "nhd_download"                             # where nhdplusTools will cache/download
buffer_m <- 100                                              # buffer around flowlines in meters
plot_crs <- 5070                                             # US Albers (EPSG:5070) - metric
# ------------------------

# read stream network (you must download NHD/NHDPlus flowlines beforehand)
flowlines <- st_read("C:/path/to/Flowlines/flowlines.shp")

# ensure same CRS
flowlines <- st_transform(flowlines, st_crs(fish_poly))

# select only those flowlines that intersect the polygon
streams_in_poly <- st_intersection(st_make_valid(flowlines), st_union(st_make_valid(fish_poly)))

# quick plot
ggplot() +
  geom_sf(data = fish_poly, fill = "lightblue", alpha = 0.3) +
  geom_sf(data = streams_in_poly, color = "blue", size = 0.5) +
  labs(title = "Fish polygon with clipped stream network")
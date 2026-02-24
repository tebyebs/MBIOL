library(sf)
library(ggplot2)

shapefiles_path <- "/Users/mutayyeb/Downloads/Shapefiles"


shp_files <- list.files(
  path = shapefiles_path,
  pattern = "\\.shp$",
  full.names = TRUE
)

shp_list <- lapply(shp_files, st_read)
combined <- do.call(rbind, shp_list)

shp <- st_read("/Users/mutayyeb/Downloads/Shapefiles/Acantharchus_pomotis.shp")

plot(shp)

ggplot(shp) +
  geom_sf() +
  theme_minimal()



library(sf)
library(dplyr)
library(ggplot2)
library(tigris)
library(purrr)
library(stringr)
library(patchwork)

options(tigris_use_cache = TRUE)

splot <- read.csv("full_sorted_data/all_sponsors.csv") %>%
  filter(bank_status %in% c("Approved", "Sold-Out"))

bank_sf <- st_read("raw_data/bank_locations.gpkg")

bank_sf <- st_make_valid(bank_sf)

bank_sf <- bank_sf %>%
  mutate(centroid = st_centroid(geom)) %>%
  distinct(bank_id, .keep_all = T)

centroids_df <- bank_sf %>%
  mutate(
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  ) %>%
  st_drop_geometry()

merged_data <- splot %>%
  inner_join(centroids_df, by = "bank_id")

sponsor_cols <- c(
  "PE" = "darkred",
  "Government" = "lightgreen",
  "Nonprofit" = "violet",
  "Listed" = "orange",
  "Private" = "cyan"
)

us_states <- states(cb = TRUE, class = "sf") %>%
  filter(!STUSPS %in% c("AK", "HI", "PR"))


merged_data <- merged_data %>%
  mutate(
    state_list = str_trim(state_list),                 # remove whitespace
    state_list = str_split(state_list, ",") %>%        # split multiple states
      map_chr(~ .x[1]),                                # keep only first entry
    state_list = str_trim(state_list)                  # trim again after split
  ) %>%
  filter(!is.na(state_list), state_list != "")

state_totals <- merged_data %>%
  count(state_list, name = "total_banks")

state_sponsor <- merged_data %>%
  count(state_list, sponsor_type, name = "n_banks")

state_props <- state_sponsor %>%
  left_join(state_totals, by = "state_list") %>%
  mutate(prop = n_banks / total_banks)

us_states <- us_states %>%
  left_join(state_props, by = c("STUSPS" = "state_list"))

plot_sponsor_maps <- function(s_type) {
  
  col <- sponsor_cols[[s_type]]
  
  # Filter data
  data_filtered <- merged_data %>%
    filter(sponsor_type == s_type)
  
  state_filtered <- us_states %>%
    filter(sponsor_type == s_type)
  
  # --- Map 1: Centroid locations ---
  map_points <- ggplot() +
    geom_sf(data = us_states, fill = "grey95", colour = "grey30", linewidth = 0.3) +
    geom_point(
      data = data_filtered,
      aes(x = lon, y = lat),
      colour = col,
      alpha = 0.7,
      size = 1.5
    ) +
    coord_sf(xlim = c(-125, -66), ylim = c(25, 50), expand = FALSE) +
    theme_minimal() +
    labs(
      title = NULL,
      x = NULL, y = NULL,
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  # --- Map 2: State proportion choropleth ---
  map_choropleth <- ggplot() +
    
    # Base layer: ALL states (ensures full outlines)
    geom_sf(
      data = us_states,
      fill = NA,
      colour = "grey30",
      linewidth = 0.3
    ) +
    # Choropleth layer: only states with data
    geom_sf(
      data = state_filtered,
      aes(fill = prop),
      colour = "grey30",
      linewidth = 0.3
    ) +
    
    scale_fill_gradient(
      low = "white",
      high = col,
      limits = c(0, 1),
      na.value = "grey90",
      name = "Proportion"
    ) +
    
    coord_sf(xlim = c(-125, -66), ylim = c(25, 50), expand = FALSE) +
    
    theme_minimal() +
    labs(
      title = NULL
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  list(points = map_points, choropleth = map_choropleth)
}

sponsor_types <- unique(merged_data$sponsor_type)

all_maps <- map(sponsor_types, plot_sponsor_maps)
names(all_maps) <- sponsor_types

all_maps[["Private"]]$points
all_maps[["Private"]]$choropleth

all_maps[["Private"]]$choropleth



combined_plot <- wrap_plots(
  lapply(names(all_maps), function(type) {
    
    (
      all_maps[[type]]$points +
        all_maps[[type]]$choropleth +
        plot_layout(ncol = 2)
    ) +
      plot_annotation(title = type)  # 👈 sponsor label per row
    
  }),
  ncol = 1
) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

#ggsave(
  #filename = "sponsor_maps.tiff",
  #plot = combined_plot,
  #width = 13,
  #height = 18,
  #units = "in",
 # dpi = 600,
#  compression = "lzw"
#)

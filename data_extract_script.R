analysis_data <- dedup_ribits %>%
  filter(is_public %in% FALSE) %>%
  filter(is_nonprofit %in% FALSE) %>%
  select(bank_id, sponsor_name) %>% 
  filter(!grepl("NA", sponsor_name))

sponsor_counts <- analysis_data %>%
  count(sponsor_name, sort = TRUE)


top_31_total_banks <- sponsor_counts %>%
  slice_head(n = 31) %>%
  summarise(total_banks = sum(n)) %>%
  pull(total_banks)
top_31_total_banks

top_n <- 20
sponsor_counts %>%
  slice_head(n = top_n) %>%
  ggplot(aes(
    x = reorder(sponsor_name, n),
    y = n
  )) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = paste("Top", top_n, "Sponsors by Number of Banks"),
    x = "Sponsor",
    y = "Count"
  ) +
  theme_minimal()


#makes a top 70 list with all sponsors with <10 banks grouped as others
sponsor_table <- sponsor_counts %>%
  arrange(desc(n)) %>%
  mutate(
    sponsor_group = case_when(
      row_number() <= 70 ~ sponsor_name,   # top 70 sponsors
      n < 10 ~ "Other",                     # small sponsors
      TRUE ~ sponsor_name                  # keep others as-is
    )
  ) %>%
  group_by(sponsor_group) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n))

#create csv for ordered sponsor table
#write_csv(sponsor_table, "sponsor_table.csv")

#creates a csv containing just bank and centroid info
#write_csv(centroids_geom, "bank_locations.csv" )



#adds bank ids back to sponsor table 
top70_id_sponsor_table <- sponsor_table %>%
  rename("sponsor_name" = "sponsor_group") %>%
  inner_join(analysis_data, by = "sponsor_name")

#remove duplicate bank ids since they repeat a bit - will need to do this for others
dedup_sponsor <- distinct(top70_id_sponsor_table, bank_id, .keep_all = T)

#write_csv(dedup_sponsor, "sponsors_ids.csv")
dedup_ribits <- distinct(ribits_data_simplified, bank_id, .keep_all = T)

sponsor_ids_centroid <- sponsors_ids %>%
  left_join(centroids_geom, by = "bank_id") %>%
  select(sponsor_name, bank_id, classification, centroid)

top70private <- sponsor_ids_centroid %>% 
  # Keep only banks with a centroid geometry
  filter(!sapply(centroid, function(x) is.null(x) || st_is_empty(x))) %>%
  # Keep only banks owned private
  filter(grepl("Privately held company", classification)) %>%
  # Convert to a proper sf object
  st_as_sf(sf_column_name = "centroid", crs = 4326) # Assume WGS84
cat("Prepared", nrow(top70private), "centroids for plotting.\n")

top70pe <- sponsor_ids_centroid %>% 
  # Keep only banks with a centroid geometry
  filter(!sapply(centroid, function(x) is.null(x) || st_is_empty(x))) %>%
  # Keep only banks owned private equity
  filter(grepl("Private equity–owned", classification)) %>%
  # Convert to a proper sf object
  st_as_sf(sf_column_name = "centroid", crs = 4326) # Assume WGS84
cat("Prepared", nrow(top70pe), "centroids for plotting.\n")

top70pub <- sponsor_ids_centroid %>% 
  # Keep only banks with a centroid geometry
  filter(!sapply(centroid, function(x) is.null(x) || st_is_empty(x))) %>%
  # Keep only banks owned public
  filter(grepl("Publicly listed company", classification)) %>%
  # Convert to a proper sf object
  st_as_sf(sf_column_name = "centroid", crs = 4326) # Assume WGS84
cat("Prepared", nrow(top70pe), "centroids for plotting.\n")

##st_write(banks_locations, dsn="bank_locations.gpkg", layer='bank_geoms')

loc_gkpg <- here("bank_locations.gpkg")
centroids <- st_read(loc_gkpg, layer = "bank_geoms", quiet = TRUE)
dedup_centroids <- distinct(centroids, bank_id, .keep_all = T)

test <- dedup_centroids %>% 
  # Keep only banks with a centroid geometry
  filter(!sapply(geom, function(x) is.null(x) || st_is_empty(x))) %>%
  st_as_sf(sf_column_name = "centroid", crs = 4326) # Assume WGS84
cat("Prepared", nrow(test), "centroids for plotting.\n")

# 2. Get base map data using rnaturalearth to get country and state boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America", returnclass = "sf")
#this only has to be done once 


ggplot() +
  # Add a base layer for the world, filled with a light grey
  geom_sf(data = world, fill = "gray90", color = "white") +
  # Add US state boundaries
  geom_sf(data = states, fill = "gray80", color = "white") +
  # Add the bank centroids on top
  geom_sf(
    data = test,  #change depending on use 
    color = "red",
    size = 1.5,
    alpha = 0.4,
    shape = 16
  ) +
  # Set the coordinate system and limits.
  # This zooms the map to show the continental US, Alaska, Hawaii, and the Caribbean.
  coord_sf(
    xlim = c(-180, -65),
    ylim = c(18, 72),
    expand = FALSE,
    crs = 4326 # Use WGS84 projection
  ) +
  # Use a minimal theme suitable for maps
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )


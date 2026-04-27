#packages
library(dplyr)                 # Data manipulation
library(ggplot2)               # Data visualization
library(here) 
library(geosphere)
library(tidyr)
library(stringr)

# load in ribits ledger
ledger <- readRDS("ribits_data/harmonized_ribits_ledgers.rds")

#fill out missing bank ids using bank name
#remove banks with no name, since they have very little information
sum(is.na(ledger$name) & is.na(ledger$bank_id))
#attempted to do this, no matches between bank names in ledger and all_sponsors for those with na bank ids

###ADD RAWRIBITS DATA ABOUT HUC CODE
bank_huc <- read.csv("raw_data/ribits_data_raw.csv", stringsAsFactors = FALSE) %>%
  #NEED TO TAKE THE FIRST HUC LOCATION ONLY, SINCE THERE ARE MULTIPLE
  distinct(bank_id, .keep_all = TRUE) %>%
  select(bank_id, huc_list_from_bank_location) %>%
  mutate(
    huc_list_from_bank_location = str_trim(
      str_extract(huc_list_from_bank_location, "^[^,]+")
    )) %>%
  filter(!is.na(huc_list_from_bank_location))
  
sum(is.na(bank_huc$huc_list_from_bank_location))

#load in csv
all_sponsors <- read.csv("full_sorted_data/all_sponsors.csv") %>%
#FILTER FOR ONLY APPROVED OR SOLD OUT BANKS 
  filter(bank_status %in% c("Approved", "Sold-Out")) %>%
  left_join(
    bank_huc,
    by = "bank_id"
  )

#ensure hucs are even length
all_sponsors <- all_sponsors %>%
  mutate(
    huc_list_from_bank_location = str_trim(huc_list_from_bank_location),
    huc_list_from_bank_location = ifelse(
      !is.na(huc_list_from_bank_location) &
        nchar(huc_list_from_bank_location) %% 2 == 1,
      paste0("0", huc_list_from_bank_location),
      huc_list_from_bank_location
    )
  )

table(nchar(all_sponsors$huc_list_from_bank_location))

#filter for data with no coords, as well as banks with no id, and HUC info - later, filter for co-ords within USA as well 
ledger <- ledger %>%
  filter(
    !is.na(impact_location_latitude), 
    !is.na(impact_location_longitude),
    !is.na(bank_id),
    !is.na(impact_huc)
  )
sum(is.na(ledger$bank_id))
sum(is.na(ledger$impact_huc))


#calc distance

ledger2 <- ledger %>%
  mutate(
    impact_location_longitude = as.numeric(impact_location_longitude),
    impact_location_latitude  = as.numeric(impact_location_latitude)
  )

sponsors2 <- all_sponsors %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude)
  )

#fixing the error coords
summary(ledger2$impact_location_longitude)
summary(ledger2$impact_location_latitude)
summary(sponsors2$longitude)
summary(sponsors2$latitude)

#decimal shift
fix_coord <- function(x, type = c("lon", "lat")) {
  type <- match.arg(type)
  limit <- if (type == "lon") 180 else 90
  
  x <- as.numeric(x)
  
  for (i in seq_along(x)) {
    if (!is.na(x[i]) && abs(x[i]) > limit) {
      # keep dividing by 10 until the value looks valid
      while (abs(x[i]) > limit) {
        x[i] <- x[i] / 10
      }
    }
  }
  
  x
}
#apply function
ledger2 <- ledger %>%
  mutate(
    impact_location_longitude = fix_coord(impact_location_longitude, "lon"),
    impact_location_latitude  = fix_coord(impact_location_latitude, "lat")
  ) %>%
  mutate(impact_location_longitude = -abs(impact_location_longitude)) #MANY SEEM TO BE MISSING NEGATIVE SIGn

sponsors2 <- all_sponsors %>%
  mutate(
    longitude = fix_coord(longitude, "lon"),
    latitude  = fix_coord(latitude, "lat")
  )

#check it worked
summary(ledger2$impact_location_longitude)
summary(ledger2$impact_location_latitude)
summary(sponsors2$longitude)
summary(sponsors2$latitude)


###CHECK IF THESE CO_ORDS FALL WITHIN THE US,PR,ALASKA OR HAWAII
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

# U.S. states + DC + Puerto Rico
us_region <- tigris::states(cb = TRUE, class = "sf") %>%
  filter(STUSPS %in% c(state.abb, "DC", "PR")) %>%
  st_transform(4326) %>%
  st_union()

# Turn your ledger into spatial points
ledger_sf <- ledger2 %>%
  mutate(row_id = row_number()) %>%
  st_as_sf(
    coords = c("impact_location_longitude", "impact_location_latitude"),
    crs = 4326,
    remove = FALSE,
    na.fail = FALSE
  )

# Flag points that fall inside the U.S. region
ledger_sf <- ledger_sf %>%
  mutate(within_us = lengths(st_within(geometry, us_region)) > 0)

# Rows outside the region, plus rows with missing coords
outside_rows <- ledger_sf %>%
  filter(!within_us | is.na(impact_location_longitude) | is.na(impact_location_latitude)) %>%
  st_drop_geometry()

ledger2$within_us <- ledger_sf$within_us

#FILTER ONLY BANKS THAT ARE WITHIN THE USA
ledger2 <- ledger2 %>%
  filter(within_us)

#compute again
avg_dist_by_bank <- ledger2 %>%
  left_join(
    sponsors2 %>% select(bank_id, sponsor_lat = latitude, sponsor_lon = longitude),
    by = "bank_id"
  ) %>%
  filter(
    !is.na(sponsor_lon), !is.na(sponsor_lat),
    !is.na(impact_location_longitude), !is.na(impact_location_latitude)
  ) %>%
  mutate(
    distance_m = distHaversine(
      cbind(sponsor_lon, sponsor_lat),
      cbind(impact_location_longitude, impact_location_latitude)
    )
  ) %>%
  group_by(bank_id) %>%
  summarise(
    avg_distance_m = mean(distance_m, na.rm = TRUE),
    .groups = "drop"
  )


#adding sponsor type information
# keep only sponsor rows that appear in avg_dist_by_bank


#plot a graph
summary_by_type <- avg_dist_by_bank %>%
  left_join(all_sponsors, by = "bank_id") %>%
  group_by(sponsor_type) %>%
  summarise(
    n = n(),
    mean_km = mean(avg_distance_m, na.rm = TRUE) / 1000,
    sd_km = sd(avg_distance_m, na.rm = TRUE) / 1000,
    se_km = sd_km / sqrt(n),
    ci_lower = mean_km - 1.96 * se_km,
    ci_upper = mean_km + 1.96 * se_km,
    .groups = "drop"
  )

summary_by_type <- summary_by_type %>%
  arrange(mean_km) %>%
  mutate(sponsor_type = factor(sponsor_type, levels = sponsor_type))

ggplot(summary_by_type, aes(x = sponsor_type, y = mean_km)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  theme_minimal()

ggplot(summary_by_type, aes(x = sponsor_type, y = mean_km)) +
  geom_col() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "Average Distance by Sponsor Type",
    x = "Sponsor Type",
    y = "Average Distance (km)"
  ) +
  theme_minimal()

### STAGE 3 HUC ECOLOGICAL ANALYSIS


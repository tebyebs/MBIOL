#packages
library(dplyr)                 # Data manipulation
library(ggplot2)               # Data visualization
library(here) 
library(geosphere)
library(tidyr)


# load in ribits ledger
ledger <- readRDS("ribits_data/harmonized_ribits_ledgers.rds")

#load in csv
all_sponsors <- read.csv("full_sorted_data/all_sponsors.csv")

#filter for data with no coords
ledger <- ledger %>%
  filter(
    !is.na(impact_location_latitude), 
    !is.na(impact_location_longitude),
  )

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
  )

sponsors2 <- all_sponsors %>%
  mutate(
    longitude = fix_coord(longitude, "lon"),
    latitude  = fix_coord(latitude, "lat")
  )

#check it worked
summary(ledger2$impact_location_longitude)
summary(ledger2$impact_location_latitude)

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
bank_sponsors <- all_sponsors %>%
  semi_join(avg_dist_by_bank, by = "bank_id")

# join sponsor columns onto the distance table
bank_dist <- avg_dist_by_bank %>%
  left_join(bank_sponsors, by = "bank_id")

avg_dist_by_sponsor_type <- avg_dist_by_bank %>%
  left_join(all_sponsors, by = "bank_id") %>%
  group_by(sponsor_type) %>%
  summarise(
    n_banks = n(),
    avg_distance_m = mean(avg_distance_m, na.rm = TRUE),
    avg_distance_km = avg_distance_m / 1000,
    .groups = "drop"
  )

#looking at a state by state basis

avg_dist_by_type_state <- avg_dist_by_bank %>%
  left_join(all_sponsors, by = "bank_id") %>%
  group_by(sponsor_type, state_list) %>%
  summarise(
    n_banks = n(),
    avg_distance_m = mean(avg_distance_m, na.rm = TRUE),
    avg_distance_km = avg_distance_m / 1000,
    .groups = "drop"
  ) %>%
  arrange(sponsor_type, state_list)

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


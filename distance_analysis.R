#packages
library(dplyr)                 # Data manipulation
library(ggplot2)               # Data visualization
library(here) 
library(geosphere)
library(tidyr)
library(stringr)
library(purrr)

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
  
#load in csv
all_sponsors <- read.csv("full_sorted_data/all_sponsors.csv") %>%
#FILTER FOR ONLY APPROVED OR SOLD OUT BANKS 
  filter(bank_status %in% c("Approved", "Sold-Out")) %>%
  inner_join(
    bank_huc,
    by = "bank_id"
  )

sum(is.na(all_sponsors$huc_list_from_bank_location))

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



 




### STAGE 3 HUC ECOLOGICAL ANALYSIS
ledger2_clean <- ledger2 %>%
  mutate(
    impact_huc = str_trim(impact_huc),
    impact_huc = ifelse(
      !is.na(impact_huc) & nchar(impact_huc) %% 2 == 1,
      paste0("0", impact_huc),
      impact_huc
    )
  ) %>%
  filter(!is.na(impact_huc), impact_huc != "")

#check the banks 
table(nchar(ledger2$impact_huc))
table(nchar(ledger2_clean$impact_huc))

bank_impacts <- ledger2_clean %>%
  group_by(bank_id) %>%
  summarise(
    impact_hucs = list(impact_huc),
    .groups = "drop"
  )

bank_data <- bank_impacts %>%
  inner_join(all_sponsors, by = "bank_id")

#huc distance function
huc_distance <- function(bank_huc, impact_huc) {
  if (is.na(bank_huc) | is.na(impact_huc)) return(NA)
  
  if (substr(bank_huc, 1, 8) == substr(impact_huc, 1, 8)) return(0)
  else if (substr(bank_huc, 1, 6) == substr(impact_huc, 1, 6)) return(1)
  else if (substr(bank_huc, 1, 4) == substr(impact_huc, 1, 4)) return(2)
  else if (substr(bank_huc, 1, 2) == substr(impact_huc, 1, 2)) return(3)
  else return(4)
}


#THIS DID WORK - creates min/max huc, as well as avg huc, and proportion in same basin
bank_data <- bank_data %>%
  mutate(
    huc_distances = map2(
      huc_list_from_bank_location,
      impact_hucs,
      ~ {
        bank_huc <- .x
        map_dbl(.y, ~ huc_distance(bank_huc, .x))
      }
    ),
    mean_huc_distance = map_dbl(huc_distances, mean, na.rm = TRUE),
    max_huc_distance  = map_dbl(huc_distances, max, na.rm = TRUE),
    min_huc_distance  = map_dbl(huc_distances, min, na.rm = TRUE)
  )


###using a jitterplot since violin plot didnt really make sense here - the probability distribution extended beyond the axis limits 

ggplot(bank_data, aes(x = sponsor_type, y = mean_huc_distance, fill = sponsor_type)) +
  
  # boxplot
  geom_boxplot(
    width = 0.4,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  
  # jitter points
  geom_jitter(
    width = 0.15,
    alpha = 0.5,
    size = 1.5
  ) +
  
  # mean point
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,          # filled circle with border
    size = 4,            # larger
    fill = "white",      # contrast fill
    colour = "black",    # strong outline
    stroke = 1.2         # thicker border
  )  +
  
  # colour scheme
  scale_fill_manual(values = c(
    "PE" = "darkred",
    "Government"   = "lightgreen",
    "Nonprofit"      = "violet",
    "Listed"         = "orange",
    "Private"        = "cyan"
  )) +
  
  # keep within valid bounds
  coord_cartesian(ylim = c(0, 4)) +
  
  labs(
    x = "Sponsor Type",
    y = "Mean Watershed (HUC) Distance Score"
  ) +
  
  theme_classic() +
  
  # cleaner legend handling (optional)
  theme(
    legend.position = "none",
    text = element_text(size = 16)
  )

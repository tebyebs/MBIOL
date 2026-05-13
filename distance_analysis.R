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
    min_huc_distance  = map_dbl(huc_distances, min, na.rm = TRUE),
    prop_same_huc = map_dbl(
      huc_distances,
      ~ mean(.x == 0, na.rm = TRUE)
    )
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


#plotting distance by bank, based on the cleaned dataset
avg_dist_by_bank <- ledger2_clean %>%
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
  ) %>%
  inner_join(all_sponsors, by = "bank_id")

avg_dist_by_bank <- avg_dist_by_bank %>%
  mutate(
    avg_distance_km = avg_distance_m / 1000
  )

#plot jitter of distances
# Define colour scheme (from your spec)
sponsor_cols <- c(
  "PE" = "darkred",
  "Government" = "lightgreen",
  "Nonprofit" = "violet",
  "Listed" = "orange",
  "Private" = "cyan"
)

dist <- ggplot(avg_dist_by_bank, aes(x = sponsor_type, y = avg_distance_km, fill = sponsor_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(color = sponsor_type), width = 0.2, size = 2, alpha = 0.4) +
  #geom_violin(trim = T, alpha = 0.5, colour = NA, #bounds = c(0,200)
  #            ) + 
  scale_fill_manual(values = sponsor_cols) +
  scale_color_manual(values = c("grey20","grey20","grey20","grey20","grey20")) +
  labs(
    x = "Sponsor Type",
    y = "Average Distance (km)",
   # title = "Distance Between Impact and Mitigation Bank by Sponsor Type"
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
  #coord_cartesian(ylim = c(0, 200)) +
  theme_classic() +
  
  # cleaner legend handling (optional)
  theme(
    legend.position = "none",
    text = element_text(size = 16)
  )


avg_dist_by_bank <- avg_dist_by_bank %>%
  mutate(
    log_distance_km = log(avg_distance_km)
  )

##summary:
distsum <- bank_data %>%
  group_by(sponsor_type) %>%
  summarise(
    mean = mean(mean_huc_distance, na.rm = TRUE),
    sd = sd(mean_huc_distance, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se,
    .groups = "drop"
  )

logdist <- ggplot(avg_dist_by_bank, aes(x = sponsor_type, y = log_distance_km, fill = sponsor_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(color = sponsor_type), width = 0.2, size = 2, alpha = 0.4) +
  #geom_violin(trim = T, alpha = 0.5, colour = NA, #bounds = c(0,200)
  #            ) + 
  scale_fill_manual(values = sponsor_cols) +
  scale_color_manual(values = c("grey20","grey20","grey20","grey20","grey20")) +
  labs(
    x = "Sponsor Type",
    y = "Log Average Distance",
    # title = "Distance Between Impact and Mitigation Bank by Sponsor Type"
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
  #coord_cartesian(ylim = c(0, 200)) +
  theme_classic() +
  
  # cleaner legend handling (optional)
  theme(
    legend.position = "none",
    text = element_text(size = 16)
  )
library(patchwork)
dist <- dist + ggtitle("Raw Distance")
logdist <- logdist + ggtitle(("Log Distance"))

dist + logdist +
  plot_annotation(
    tag_levels = "A")

dist_model_log <- aov(log_distance_km ~ sponsor_type, data = avg_dist_by_bank)

library(broom)
library(dplyr)
library(gt)

dist_log_anova <- tidy(dist_model_log) %>%
  mutate(
    term = case_when(
      term == "sponsor_type" ~ "Sponsor Type",
      term == "Residuals" ~ "Residuals"
    ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

dist_log_anova_gt <- dist_log_anova %>%
  gt() %>%
  fmt_number(columns = c(sumsq, meansq, statistic), decimals = 3) %>%
  fmt_scientific(columns = p.value, decimals = 2) %>%
  cols_merge(columns = c(p.value, sig), pattern = "{1} {2}") %>%
  cols_label(
    term = "Source",
    df = "Df",
    sumsq = "Sum Sq",
    meansq = "Mean Sq",
    statistic = "F",
    p.value = "p-value"
  ) %>%
  tab_header(
    title = "Effect of Sponsor Type on Log Distance",
    subtitle = "One-way ANOVA (log-transformed distance in km)"
  ) %>%
  tab_source_note(
    source_note = md("*Note:* Values are based on log-transformed distance (km). Significance: *p* < 0.05 (*), < 0.01 (**), < 0.001 (***).")
  ) %>%
  cols_align(align = "center", -term)

dist_log_tukey <- TukeyHSD(dist_model_log)

dist_log_tukey_df <- as.data.frame(dist_log_tukey$sponsor_type) %>%
  mutate(comparison = rownames(.)) %>%
  rename(
    Difference = diff,
    Lower_CI = lwr,
    Upper_CI = upr,
    p.value = `p adj`
  ) %>%
  mutate(
    distance_ratio = exp(Difference),
    ratio_lower = exp(Lower_CI),
    ratio_upper = exp(Upper_CI),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

dist_log_tukey_gt <- dist_log_tukey_df %>%
  gt() %>%
  
  # Format numeric columns
  fmt_number(columns = c(Difference, Lower_CI, Upper_CI), decimals = 3) %>%
  fmt_number(columns = c(distance_ratio, ratio_lower, ratio_upper), decimals = 2) %>%
  fmt_scientific(columns = p.value, decimals = 2) %>%
  
  # Merge p-value + stars
  cols_merge(
    columns = c(p.value, sig),
    pattern = "{1} {2}"
  ) %>%
  
  # Labels
  cols_label(
    comparison = "Comparison",
    Difference = "Mean Diff (log km)",
    Lower_CI = "Lower CI",
    Upper_CI = "Upper CI",
    distance_ratio = "Distance Ratio",
    ratio_lower = "Ratio CI (Lower)",
    ratio_upper = "Ratio CI (Upper)",
    p.value = "p-value"
  ) %>%
  
  # Title
  tab_header(
    title = "Pairwise Comparisons of Sponsor Type",
    subtitle = "Tukey HSD with Back-Transformed Distance Ratios"
  ) %>%
  
  # Footnote
  tab_source_note(
    source_note = md("*Note:* Distance ratios represent multiplicative differences between groups (e.g., 2 = twice as far, 0.5 = half as far). Confidence intervals shown for both log-scale and ratio-scale estimates.")
  ) %>%
  
  cols_align(
    align = "center",
    -comparison
  )

#gtsave(dist_log_anova_gt, "log_distance_anova.docx")
#gtsave(dist_log_tukey_gt, "log_distance_tukey_with_ratios.docx")

### Proportion same HUC plot + stats test
huc_summary <- bank_data %>%
  group_by(sponsor_type) %>%
  summarise(
    mean_prop = mean(prop_same_huc, na.rm = TRUE),
    n = n(),
    se = sd(prop_same_huc, na.rm = TRUE) / sqrt(n),
    ci_lower = mean_prop - 1.96 * se,
    ci_upper = mean_prop + 1.96 * se,
    .groups = "drop"
  )

huc_plot_data <- huc_summary %>%
  mutate(remaining = 1 - mean_prop) %>%
  pivot_longer(
    cols = c(mean_prop, remaining),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    fill_group = ifelse(type == "mean_prop", sponsor_type, "remaining")
  )

huc_plot_data <- huc_plot_data %>%
  mutate(
    type = factor(type, levels = c("mean_prop", "remaining"))
  )

sponsor_cols <- c(
  "PE" = "darkred",
  "Government" = "lightgreen",
  "Nonprofit" = "violet",
  "Listed" = "orange",
  "Private" = "cyan",
  "remaining" = "white"
)
##NEED TO RUN BOTH GGPLOTS
ggplot(huc_plot_data, aes(x = sponsor_type, y = value, fill = type)) +
  
  geom_bar(stat = "identity", width = 0.7, colour = "black") +
  
  geom_errorbar(
    data = huc_summary,
    aes(
      x = sponsor_type,
      ymin = ci_lower,
      ymax = ci_upper
    ),
    width = 0.2,
    colour = "black",
    inherit.aes = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "mean_prop" = "grey",   # placeholder (we override below)
      "remaining" = "white"
    )
  ) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggplot() +
  
  # White full bars (background)
  geom_bar(
    data = huc_summary,
    aes(x = sponsor_type, y = 1),
    stat = "identity",
    width = 0.7,
    fill = "white",
    colour = "black"
  ) +
  
  # Coloured portion (actual proportion)
  geom_bar(
    data = huc_summary,
    aes(x = sponsor_type, y = mean_prop, fill = sponsor_type),
    stat = "identity",
    width = 0.7,
    colour = "black"
  ) +
  
  # Error bars (NOW FIXED)
  geom_errorbar(
    data = huc_summary,
    aes(
      x = sponsor_type,
      ymin = ci_lower,
      ymax = ci_upper
    ),
    width = 0.2,
    colour = "black",
    inherit.aes = FALSE   # ✅ THIS FIXES YOUR ERROR
  ) +
  
  scale_fill_manual(values = sponsor_cols) +
  
  labs(
    x = "Sponsor Type",
    y = "Proportion of Banks in Same HUC8",
    #title = "Localisation of Mitigation by Sponsor Type"
  ) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",text = element_text(size = 16))

##glm attempt
bank_data <- bank_data %>%
  mutate(
    n_impacts = map_int(huc_distances, ~ sum(!is.na(.x))),
    n_same_huc = map_int(huc_distances, ~ sum(.x == 0, na.rm = TRUE))
  )

glm_quasi <- glm(
  cbind(n_same_huc, n_impacts - n_same_huc) ~ sponsor_type,
  family = quasibinomial,
  data = bank_data
)

summary(glm_quasi)

exp(coef(glm_quasi))
anova(glm_quasi, test = "F")

library(emmeans)

emmeans(glm_quasi, pairwise ~ sponsor_type, type = "response")


contrast_df <- as.data.frame(
  emmeans(glm_quasi, pairwise ~ sponsor_type, type = "response")$contrasts
)

contrast_df <- contrast_df %>%
  rename(
    Comparison = contrast,
    Odds_Ratio = odds.ratio,
    SE = SE,
    z = z.ratio,
    p.value = p.value
  ) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

contrast_gt <- contrast_df %>%
  gt() %>%
  
  fmt_number(columns = c(Odds_Ratio, SE, z), decimals = 3) %>%
  fmt_scientific(columns = p.value, decimals = 2) %>%
  
  cols_merge(
    columns = c(p.value, sig),
    pattern = "{1} {2}"
  ) %>%
  
  cols_label(
    Comparison = "Comparison",
    Odds_Ratio = "Odds Ratio",
    SE = "SE",
    z = "z",
    p.value = "p-value"
  ) %>%
  
  tab_header(
    title = "Pairwise Comparisons of Sponsor Type",
    subtitle = "Quasibinomial GLM (odds ratios)"
  ) %>%
  
  tab_source_note(
    source_note = md("*Note:* Odds ratios >1 indicate higher likelihood of mitigation within the same HUC for the first group in the comparison. Significance: *p* < 0.05 (*), < 0.01 (**), < 0.001 (***).")
  ) %>%
  
  cols_align(align = "center", -Comparison)

#gtsave(contrast_gt, "glm_pairwise_odds_ratios.docx")

#anova
anova_df <- as.data.frame(anova(glm_quasi, test = "F")) %>%
  mutate(
    term = rownames(.)
  ) %>%
  rename(
    Df = Df,
    Deviance = Deviance,
    Resid_Df = `Resid. Df`,
    Resid_Dev = `Resid. Dev`,
    F = `F`,
    p.value = `Pr(>F)`
  ) %>%
  mutate(
    term = case_when(
      term == "NULL" ~ "Intercept",
      term == "sponsor_type" ~ "Sponsor Type",
      TRUE ~ term
    ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  filter(term != "Intercept")



anova_gt <- anova_df %>%
  gt() %>%
  
  fmt_number(columns = c(Deviance, Resid_Dev, F), decimals = 2) %>%
  fmt_scientific(columns = p.value, decimals = 2) %>%
  
  cols_merge(
    columns = c(p.value, sig),
    pattern = "{1} {2}"
  ) %>%
  
  cols_label(
    term = "Source",
    Df = "Df",
    Deviance = "Deviance",
    Resid_Df = "Residual Df",
    Resid_Dev = "Residual Deviance",
    F = "F",
    p.value = "p-value"
  ) %>%
  
  tab_header(
    title = "Effect of Sponsor Type on Local Mitigation",
    subtitle = "Analysis of Deviance (Quasibinomial GLM)"
  ) %>%
  
  tab_source_note(
    source_note = md("*Note:* Results from quasibinomial GLM with logit link. F-tests are based on analysis of deviance. Significance: *p* < 0.05 (*), < 0.01 (**), < 0.001 (***).")
  ) %>%
  
  cols_align(align = "center", -term)

#gtsave(anova_gt, "glm_anova_table.docx")


#histogram
ggplot(bank_data, aes(x = mean_huc_distance, fill = sponsor_type)) +
  geom_histogram(
    binwidth = 0.2,
    boundary = 0,
    color = "black"
  ) +
  facet_wrap(~ sponsor_type, ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5)) +
  scale_fill_manual(values = c(
    "PE" = "darkred",
    "Government" = "lightgreen",
    "Nonprofit" = "violet",
    "Listed" = "orange",
    "Private" = "cyan"
  )) +
  labs(
    title = "Distribution of Mean HUC Score by Sponsor Type",
    x = "Mean HUC Score",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )
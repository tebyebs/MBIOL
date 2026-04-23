#packages
library(dplyr)                 # Data manipulation
library(janitor)               # Data cleaning
library(purrr)                 # Functional programming
library(ggplot2)               # Data visualization
library(here)                  # File path handling
library(tidyr) 
library(scales)

#load in the data
all_sponsors <- read.csv("full_sorted_data/all_sponsors")

###Part 2 - Analysis over time and pending vs approved - based on gains and losses linear model

#filter for banks with no year established,maybe use bank status date instead
#ALTERNATIVELY, can ignore year established and just do approved vs pending 
combined_counts <- all_sponsors %>%
  mutate(
    status_group = case_when(
      bank_status %in% c("Approved", "Sold-Out") ~ "Approved/Sold-Out",
      bank_status == "Pending" ~ "Pending",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(status_group)) %>%
  count(status_group, sponsor_type) %>%
  group_by(status_group) %>%
  mutate(
    pct = n / sum(n),
    pct_label = scales::percent(pct)
  ) %>%
  ungroup() %>%
  arrange(status_group, desc(n))

combined_counts <- all_sponsors %>%
  mutate(
    status_group = case_when(
      bank_status %in% c("Approved", "Sold-Out") ~ "Approved/Sold-Out",
      bank_status == "Pending" ~ "Pending",
      bank_status == "Withdrawn" ~ "Withdrawn",
      bank_status == "Terminated" ~ "Terminated",
      bank_status == "Suspended" ~ "Suspended",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(status_group)) %>%
  count(status_group, sponsor_type) %>%
  group_by(status_group) %>%
  mutate(
    pct = n / sum(n),
    pct_label = percent(pct)
  ) %>%
  ungroup() %>%
  arrange(status_group, desc(n))

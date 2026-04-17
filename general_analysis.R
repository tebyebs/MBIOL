#packages
library(dplyr)                 # Data manipulation
library(janitor)               # Data cleaning
library(purrr)                 # Functional programming
library(ggplot2)               # Data visualization
library(here)                  # File path handling
library(tidyr) 

#load in the data
pe_sponsors <- read.csv("full_sorted_data/pe_sponsors.csv")
listed_sponsors <- read.csv("full_sorted_data/listed_sponsors.csv")
govt_sponsors <- read.csv("full_sorted_data/govt_sponsors.csv")
private_sponsors <- read.csv("full_sorted_data/private_sponsors.csv")
nonprofit_sponsors <- read.csv("full_sorted_data/nonprofit_sponsors.csv")


#preparing private,pe, public database

private_sponsors <- private_sponsors %>%
  mutate(zip_sponsor = as.character(zip_sponsor)) %>%
  mutate(zip_poc = as.character(zip_poc))


pe_sponsors <- pe_sponsors %>%
  mutate(zip_sponsor = as.character(zip_sponsor)) %>%
  mutate(zip_poc = as.character(zip_poc))


govt_sponsors <- govt_sponsors %>%
  mutate(zip_sponsor = as.character(zip_sponsor)) %>%
  mutate(zip_poc = as.character(zip_poc))

nonprofit_sponsors <- nonprofit_sponsors %>%
  mutate(zip_sponsor = as.character(zip_sponsor)) %>%
  mutate(zip_poc = as.character(zip_poc))

listed_sponsors <- listed_sponsors %>%
  mutate(zip_sponsor = as.character(zip_sponsor)) %>%
  mutate(zip_poc = as.character(zip_poc))


#combines sponsors
all_sponsors <- bind_rows(
  private_sponsors,
  pe_sponsors,
  govt_sponsors,
  listed_sponsors,
  nonprofit_sponsors
)
table(all_sponsors$sponsor_type)
#Assume single are all govt
all_sponsors <- all_sponsors %>%
  mutate(sponsor_type = ifelse(sponsor_type == "single", "govt", sponsor_type))
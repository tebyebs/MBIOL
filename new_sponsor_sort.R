#Packages
library(dplyr)                 # Data manipulation
library(janitor)               # Data cleaning
library(purrr)                 # Functional programming
library(ggplot2)               # Data visualization
library(here)                  # File path handling
library(tidyr)     

#Load the raw data
ribits_data <- read.csv("raw_data/ribits_data_simplified.csv") 

#filtering the data
ribits_data <- distinct(ribits_data, bank_id, .keep_all = T) #remove duplicates

# read the raw data to attach columns
ribits_data_raw <- read.csv("raw_data/ribits_data_raw.csv", stringsAsFactors = FALSE)
ribits_data_raw_unique <- ribits_data_raw %>%
  select(bank_id, year_established, kind_of_bank) %>% 
  distinct(bank_id, .keep_all = TRUE) #remove duplicates and filter columns of note

# join kind of bank and year established columns to allow filtering
ribits_data <- ribits_data %>%
  left_join(
    ribits_data_raw_unique %>%
      select(bank_id, year_established, kind_of_bank),
    by = "bank_id"
  )

#filter only approved banks 
ribits_data <- ribits_data %>%
  filter(bank_status %in% c("Approved", "Sold-Out"), #only approved banks
         year_established > 1995, #established after the 1995 USACE guidelines were established
         #kind_of_bank != "ILF"), #this would remove ILF banks BUT off rn as were categorising all banks
         !is.na(sponsor_name)) #remove NA values in sponsor name 




 



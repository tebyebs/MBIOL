#Packages
library(dplyr)                 # Data manipulation
library(janitor)               # Data cleaning
library(purrr)                 # Functional programming
library(ggplot2)               # Data visualization
library(here)                  # File path handling
library(tidyr) 
library(gt)

#Load the raw data
ribits_data <- read.csv("raw_data/ribits_data_simplified.csv") 

#filtering the data & remove duplicates
ribits_data <- ribits_data %>%
  group_by(bank_id) %>%
  group_modify(~ {
    g <- .x
    
    # Count missing values in each row
    g <- g %>%
      mutate(.na_count = rowSums(is.na(across(everything())))) %>%
      arrange(.na_count)
    
    # Start with the row that has the fewest NAs
    out <- g[1, , drop = FALSE]
    
    # For each column, fill missing base values from other rows in the group
    for (nm in names(out)) {
      if (nm == ".na_count") next
      
      vals <- g[[nm]]
      first_non_na <- vals[which(!is.na(vals))[1]]
      
      if (!is.na(first_non_na)) {
        out[[nm]] <- first_non_na
      }
    }
    
    # Drop helper column
    out$.na_count <- NULL
    out
  }) %>%
  ungroup()


# read the raw data to attach columns
ribits_data_raw <- read.csv("raw_data/ribits_data_raw.csv", stringsAsFactors = FALSE)
ribits_data_raw <- ribits_data_raw %>%
  select(bank_id, year_established, kind_of_bank) %>% 
  distinct(bank_id, .keep_all = TRUE) #remove duplicates and filter columns of note

# join kind of bank and year established columns to allow filtering
ribits_data <- ribits_data %>%
  left_join(
    ribits_data_raw %>%
      select(bank_id, year_established, kind_of_bank),
    by = "bank_id"
  )

###OPTIONAL
###FILTERING ALL BANKS
sum(is.na(ribits_data$sponsor_name) & is.na(ribits_data$email_poc)) #885 instances of both no sponsor or poc 
sum(is.na(ribits_data$year_established)) #ISSUE - 1677 YEAR ESTABLISHED NAs


ribits_data_total <- ribits_data %>%
  filter(
    !(is.na(sponsor_name) & is.na(email_poc))
  ) %>%
  mutate(  #additional columns for analysis
    pe_owner = "no",
    listing = "no",
    private = if_else(bank_type %in% c("Private Commercial", "Combination Public/Private"), "yes", "no"),
    govt = if_else(bank_type == "Public Commercial", "yes", "no"),
    nonprofit = if_else(bank_type == "Private Nonprofit", "yes", "no"),
    sponsor_type = case_when(
      private == "yes" ~ "private",
      govt == "yes" ~ "govt",
      nonprofit == "yes" ~ "nonprofit",
      TRUE ~ "single"
    )
  )


#export file for analysis
#write.csv(ribits_data_total, file = "all_sponsors.csv")



#filter only approved banks 
ribits_data <- ribits_data %>%
  filter(bank_status %in% c("Approved", "Sold-Out"), #only approved banks and sold out
         year_established > 1995, #established after the 1995 USACE guidelines were established
         #kind_of_bank != "ILF"), #this would remove ILF banks BUT off as were categorizing all banks
         !is.na(sponsor_name)) #remove NA values in sponsor name 

### DATA READY FOR SORTING 
### STEP 2 - ORGANISING SPONSORS BY NUMBERS OF BANKS

# table(ribits_data$bank_type) # Checks the kinds of banks - could be useful to create a table of the diff categories at this point, perhaps with more of the banks that have yet to be filtered

ribits_data_private <- ribits_data %>%
  filter(bank_type %in% c("Private Commercial", "Combination Public/Private"))

#single_client <- ribits_data %>%
 # filter(bank_type %in% "Single-Client") #just to check if it should be included - mostly public, some private 

private_counts <- ribits_data_private %>%
  count(sponsor_name, sort = TRUE)

### DATA SORTED MANUALLY IN EXCEL
### STEP 3 - CREATE PRIVATE BANKS FILE AND BEGIN FUNCTIONAL DENSITY ANALYSIS
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

#CHECK FOR DUPLICATES & NA 
sum(is.na(all_sponsors$sponsor_type))
all_sponsors %>%
  count(bank_id) %>%
  filter(n > 1)

all_sponsors <- all_sponsors %>%
  distinct(bank_id, .keep_all = TRUE)

priv_count <- all_sponsors %>% #checking which credit classifications to compare for the functional density analysis
  count(private, sort = T)

all_sponsors <- all_sponsors %>%
  select(-X) %>%  # remove the first column
  mutate(
    sponsor_type = recode(sponsor_type,
                          "govt" = "Government",
                          "private" = "Private",
                          "pe" = "PE",
                          "nonprofit" = "Nonprofit",
                          "listed" = "Listed"
    )
  )

#write the cleaned and sorted version to disk
#write.csv(all_sponsors, file = "full_sorted_data/all_sponsors.csv")



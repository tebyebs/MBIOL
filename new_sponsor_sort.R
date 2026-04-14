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

#filtering the data
ribits_data <- distinct(ribits_data, bank_id, .keep_all = T) #remove duplicates

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
table(ribits_data$bank_status)

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
pe_sponsors <- read.csv("raw_data/pe_sponsors.csv")
public_sponsors <- read.csv("raw_data/public_sponsors.csv")

#preparing private,pe, public database
private_sponsors <- ribits_data_private %>%
  mutate(
    pe_owner = "no",
    listing  = "no",
    private = "yes"
  )

pe_sponsors <- pe_sponsors %>%
  mutate(
    listing  = "no",
    private = "no"
  )

public_sponsors <- public_sponsors %>%
  mutate(
    pe_owner = "no",
    private = "no"
  )

private_sponsors <- private_sponsors %>%
  mutate(zip_sponsor = as.character(zip_sponsor)) %>%
  mutate(zip_poc = as.character(zip_poc))


pe_sponsors <- pe_sponsors %>%
  mutate(zip_sponsor = as.character(zip_sponsor)) %>%
  mutate(zip_poc = as.character(zip_poc))


public_sponsors <- public_sponsors %>%
  mutate(zip_sponsor = as.character(zip_sponsor)) %>%
  mutate(zip_poc = as.character(zip_poc))


#removes bank ids from private that were in pe or pub

private_sponsors <- private_sponsors %>%
  filter(
    !bank_id %in% c(pe_sponsors$bank_id, public_sponsors$bank_id)
  )


#combines sponsors
all_sponsors <- bind_rows(
  private_sponsors,
  pe_sponsors,
  public_sponsors
)

#create a table showing the differing sponsor names associated with each PE owner
pe_summary <- pe_sponsors %>%
  count(pe_owner, sponsor_name) %>%
  arrange(pe_owner, desc(n)) %>%
  group_by(pe_owner) %>%
  summarise(
    sponsor_names = paste0(sponsor_name, " (", n, ")", collapse = ", "),
    .groups = "drop"
  )

pe_summary %>%
  gt() %>%
  # Rename columns
  cols_label(
    pe_owner = "Private Equity Owner",
    sponsor_names = "Affiliated Sponsor Name Entries"
  ) %>%
  # Replace abbreviations in pe_owner column
  text_transform(
    locations = cells_body(columns = pe_owner),
    fn = function(x) {
      dplyr::recode(
        x,
        "ARC" = "Arc Ventures",
        "DCG" = "Domain Capital Group",
        "EIP" = "Ecosystem Investment Partners",
        "RES" = "Resource Environmental Solutions"
      )
    }
  ) %>%
  tab_header(
    title = "Sponsor Names by Private Equity Owner"
  ) #%>%
  #gtsave("pe_summary_table.html") save if needed

#how many unique private entities? roughly since they need more merging
private_counts <- private_sponsors %>%
  count(sponsor_name, sort = TRUE)


write.csv(private_sponsors, file = "private_sponsors.csv")

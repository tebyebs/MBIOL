###FUNCTIONAL DENSITY ANALYSIS
# load in ribits ledger

ledger <- readRDS("ribits_data/harmonized_ribits_ledgers.rds")

ledger_counts <- ledger %>% #checking which credit classifications to compare for the functional density analysis
  count(credit_classification_or_subdivision)

#filter to remove NAs in credits or acres, as well as banks with 0 acres

ledger <- ledger %>%
  filter(
    credit_classification_or_subdivision %in% c("Wetlands", "Stream"),
    !is.na(credits), 
    !is.na(acres),
    acres !=0
  )

#calculate credit acres

ledger <- ledger %>%
  mutate(credit_acres = credits / acres)

#create avg of credit acres across banks
functional_density <- ledger %>%
  group_by(bank_id, credit_classification_or_subdivision) %>%
  summarise(
    avg_credit_acres = mean(credit_acres, na.rm = TRUE),
    .groups = "drop"
  )


pe_functional_density <- functional_density %>%
  semi_join(pe_sponsors, by = "bank_id")

pe_density_summary <- pe_functional_density %>%
  filter(credit_classification_or_subdivision %in% c("Wetlands", "Stream")) %>%
  group_by(credit_classification_or_subdivision) %>%
  summarise(
    avg_credit_acres = mean(avg_credit_acres, na.rm = TRUE),
    .groups = "drop"
  )

#compare to public and non-profit




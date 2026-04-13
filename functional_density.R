###FUNCTIONAL DENSITY ANALYSIS
# load in ribits ledger

ledger <- readRDS("ribits_data/harmonized_ribits_ledgers.rds")

ledger_counts <- ledger %>% #checking which credit classifications to compare for the functional density analysis
  count(credit_classification_or_subdivision, sort = T)

#filter to remove NAs in credits or acres, as well as banks with 0 acres

ledger <- ledger %>%
  filter(
    credit_classification_or_subdivision %in% c("Bottomland Hardwood", "Bottomland Hardwoods"),  #can replace to include any kind of credit
    !is.na(credits), 
    !is.na(acres),
    acres !=0,
    credits !=0
  )

#calculate credit acres

ledger <- ledger %>%
  mutate(credit_acres = abs(credits / acres))

ledger_simple <- ledger %>%
  select(bank_id, credits, acres, credit_acres, bank_transaction_id) #easier to compare

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
  filter(credit_classification_or_subdivision %in% c("Bottomland Hardwood", "Bottomland Hardwoods")) %>%
  group_by(credit_classification_or_subdivision) %>%
  summarise(
    avg_credit_acres = mean(avg_credit_acres, na.rm = TRUE),
    .groups = "drop"
  )

#compare to public and non-profit




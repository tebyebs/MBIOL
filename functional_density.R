###FUNCTIONAL DENSITY ANALYSIS
library(dplyr)                 # Data manipulation
library(ggplot2)               # Data visualization
library(here) 
library(lme4)                  # Statistcal Testing

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

#test for normality of data
qqnorm(ledger$credit_acres)
qqline(ledger$credit_acres)

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
  summarise(
    avg_credit_acres = mean(avg_credit_acres, na.rm = TRUE),
  )

#compare to public and non-profit


#PART 2 STATISTICAL ANALYSIS
#add a log transformation to deal with heavy tails of the initial credit acres model
ledger <- ledger %>%
  mutate(log_credit_acres = log(credit_acres))

#generate a linear mixed effect model for log data 
model <- lmer(log_credit_acres ~ 1 + (1 | bank_id), data = ledger)

qqnorm(residuals(model))
qqline(residuals(model)) 

#still showing heavy tails - gonna try predicting model with ownership
#this will require me to make the full excel doc with ownership structure
#pivoting to organise sponsors now, will come back to this 



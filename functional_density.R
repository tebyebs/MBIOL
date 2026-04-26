###FUNCTIONAL DENSITY ANALYSIS
library(dplyr)                 # Data manipulation
library(ggplot2)               # Data visualization
library(here) 
library(lme4)                  # Statistcal Testing
library(emmeans)

# load in ribits ledger
ledger <- readRDS("ribits_data/harmonized_ribits_ledgers.rds")

#filter to remove NAs in credits or acres, as well as banks with 0 acres

ledger <- ledger %>%
  filter(
    !is.na(credits), 
    !is.na(acres),
    acres !=0,
    credits !=0
  )

ledger_counts <- ledger %>% #checking which credit classifications to compare for the functional density analysis
  count(credit_classification_or_subdivision, sort = T)

#calculate credit acres

ledger <- ledger %>%
  mutate(credit_acres = abs(credits / acres))

ledger_simple <- ledger %>%
  select(bank_id, credits, acres, credit_acres, bank_transaction_id, impact_huc) #easier to compare

#PART1A: PALUSTRINE FORESTED (PFO)
ledger_pfo <- ledger %>%
  filter(
    credit_classification_or_subdivision %in% c( "Palustrine Forested", "PFO", "PFO - Palustrine Forested"))
#PART1B: PALUSTRINE EMERGENT (PEM)
ledger_pem <- ledger %>%
  filter(
    credit_classification_or_subdivision %in% c( "Palustrine Emergent", "PEM", "PEM - Palustrine Emergent Marsh",
                                                 "Palustrine Emergent Marsh (PEM)", "Palustrine Emergent Wetland (PEMA/C)"))


###PART 2 STATISTICAL ANALYSIS
#read sponsor categories
all_sponsors <- read.csv("full_sorted_data/all_sponsors.csv") %>%
#FILTER FOR ONLY APPROVED OR SOLD OUT BANKS 
  filter(bank_status %in% c("Approved", "Sold-Out"))


# collapse data to be at bank level
functional_density_pfo <- ledger_pfo %>%
  group_by(bank_id) %>%
  summarise(
    avg_credit_acres = mean(credit_acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(all_sponsors, by = "bank_id") %>%
  mutate(
    log_avg_credit_acres = log(avg_credit_acres))

functional_density_pem <- ledger_pem %>%
  group_by(bank_id) %>%
  summarise(
    avg_credit_acres = mean(credit_acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(all_sponsors, by = "bank_id") %>%
  mutate(
    log_avg_credit_acres = log(avg_credit_acres)) 


###testing for anova suitabilit
fd_test <- functional_density_pfo %>%
  filter(!is.na(avg_credit_acres), !is.na(sponsor_type))


qqnorm(fd_test$log_avg_credit_acres)
qqline(fd_test$log_avg_credit_acres)

#simple avg analysis
summary_table_pfo <- functional_density_pfo %>%
  group_by(sponsor_type) %>%
  summarise(
    mean = mean(avg_credit_acres, na.rm = TRUE),
    sd = sd(avg_credit_acres, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se,
    .groups = "drop"
  )

#log avg analysis
log_summary_table_pfo <- functional_density_pfo %>%
  group_by(sponsor_type) %>%
  summarise(
    mean = mean(log_avg_credit_acres, na.rm = TRUE),
    sd = sd(log_avg_credit_acres, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se,
    .groups = "drop"
  )

#simple means graph, no log transform
p <- ggplot(log_summary_table_pfo, aes(x = sponsor_type, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Sponsor Type",
    y = "Average Credit Acres",
    title = "Average Credit Acres by Sponsor Type",
    subtitle = "Points show means; bars show 95% confidence intervals"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

p




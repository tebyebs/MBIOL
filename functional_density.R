###FUNCTIONAL DENSITY ANALYSIS
library(dplyr)                 # Data manipulation
library(ggplot2)               # Data visualization
library(here) 
library(lme4)                  # Statistcal Testing
library(emmeans)

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



###PART 2 STATISTICAL ANALYSIS


# collapse data to be at bank level
functional_density <- ledger %>%
  group_by(bank_id) %>%
  summarise(
    avg_credit_acres = mean(credit_acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(all_sponsors, by = "bank_id")


# Bank-level analysis dataset
bank_level <- functional_density %>%
  mutate(
    sponsor_type = factor(sponsor_type),
    log_avg_credit_acres = log(avg_credit_acres)
  ) %>%
  filter(!is.na(sponsor_type))

# Aggregated model
model <- lm(log_avg_credit_acres ~ sponsor_type, data = bank_level)

# Estimated marginal means
emm_df <- as.data.frame(emmeans(model, ~ sponsor_type))

# Back-transform from log scale to original scale
emm_df <- emm_df %>%
  mutate(
    estimate = exp(emmean),
    lower = exp(lower.CL),
    upper = exp(upper.CL)
  )

# Publication-ready plot
p <- ggplot(emm_df, aes(x = sponsor_type, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Sponsor type",
    y = "Estimated average credit acres",
    title = "Average credit acres by sponsor type",
    subtitle = "Points are model-estimated means; bars are 95% confidence intervals"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

p

#ggsave("average_credit_acres_by_sponsor_type.png", plot = p, width = 8, height = 5, dpi = 300)

#simple avg analysis

summary_table <- bank_level %>%
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

#simple means graph, no log transform
p <- ggplot(summary_table, aes(x = sponsor_type, y = mean)) +
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


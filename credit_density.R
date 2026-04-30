###FUNCTIONAL DENSITY ANALYSIS
library(dplyr)                 # Data manipulation
library(ggplot2)               # Data visualization
library(here) 
library(patchwork)
library(gt)
library(broom)



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


# collapse data to be at bank level for PEM and PFO
functional_density_pfo <- ledger_pfo %>%
  group_by(bank_id) %>%
  summarise(
    avg_credit_acres = mean(credit_acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(all_sponsors, by = "bank_id") %>%
  mutate(
    log_avg_credit_acres = log(avg_credit_acres),
    sqrt_acres = sqrt(avg_credit_acres))

functional_density_pem <- ledger_pem %>%
  group_by(bank_id) %>%
  summarise(
    avg_credit_acres = mean(credit_acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(all_sponsors, by = "bank_id") %>%
  mutate(
    log_avg_credit_acres = log(avg_credit_acres),
    sqrt_acres = sqrt(avg_credit_acres)) 




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

summary_table_pem <- functional_density_pem %>%
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

p1s <- ggplot(summary_table_pfo, aes(x = sponsor_type, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  theme_minimal(base_size = 12) +
  labs(
    x = "PFO Sponsor Type",
    y = "Average Credit Acres",
    title = "Average Credit Acres by Sponsor Type for PFO",
    subtitle = "Points show means; bars show 95% confidence intervals"
  ) +
  theme(
    panel.grid.minor = element_blank(),
  )


p2s <- ggplot(summary_table_pem, aes(x = sponsor_type, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  theme_minimal(base_size = 12) +
  labs(
    x = "PEM Sponsor Type",
    y = "Average Credit Acres",
    title = "Average Credit Acres by Sponsor Type for PEM",
    subtitle = "Points show means; bars show 95% confidence intervals"
  ) +
  theme(
    panel.grid.minor = element_blank(),
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

log_summary_table_pem <- functional_density_pem %>%
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

#log transform graph
p1 <- ggplot(log_summary_table_pfo, aes(x = sponsor_type, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  theme_minimal(base_size = 12) +
  labs(
    x = "PFO Sponsor Type",
    y = "Average Log Credit Acres",
    title = "Average Log Credit Acres by Sponsor Type for PFO",
    subtitle = "Points show means; bars show 95% confidence intervals"
  ) +
  theme(
    panel.grid.minor = element_blank(),
  )


p2 <- ggplot(log_summary_table_pem, aes(x = sponsor_type, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  theme_minimal(base_size = 12) +
  labs(
    x = "PEM Sponsor Type",
    y = "Average Log Credit Acres",
    title = "Average Log Credit Acres by Sponsor Type for PEM",
    subtitle = "Points show means; bars show 95% confidence intervals"
  ) +
  theme(
    panel.grid.minor = element_blank(),
  )

### Plotting
#simple graphs
p1s <- p1s + ylim(0, 1)
p2s <- p2s + ylim(0, 1)

p2s <- p2s + theme(axis.title.y = element_blank(),
                   axis.text.y  = element_blank(),
                   axis.ticks.y = element_blank())

p1s + p2s + plot_layout(widths = c(1,1))

#transformed
p1 <- p1 + ylim(-4, 1)
p2 <- p2 + ylim(-4, 1)

p2 <- p2 + theme(axis.title.y = element_blank(),
                   axis.text.y  = element_blank(),
                   axis.ticks.y = element_blank())

p1 + p2 + plot_layout(widths = c(1,1))


###ANOVA TEST
# PFO model
pfo_model <- aov(log_avg_credit_acres ~ sponsor_type, data = functional_density_pfo)

# PEM model
pem_model <- aov(log_avg_credit_acres ~ sponsor_type, data = functional_density_pem)


###Tukey Post Hoc
pfo_tukey <- TukeyHSD(pfo_model)$sponsor_type
pem_tukey <- TukeyHSD(pem_model)$sponsor_type

#Plot result

# --- Prepare data ---
pfo_anova <- tidy(pfo_model) %>%
  mutate(
    term = case_when(
      term == "sponsor_type" ~ "Sponsor Type",
      term == "Residuals" ~ "Residuals",
      TRUE ~ term
    ),
    Dataset = "PFO"
  )

pem_anova <- tidy(pem_model) %>%
  mutate(
    term = case_when(
      term == "sponsor_type" ~ "Sponsor Type",
      term == "Residuals" ~ "Residuals",
      TRUE ~ term
    ),
    Dataset = "PEM"
  )

combined_anova <- bind_rows(pfo_anova, pem_anova)

# --- Add significance stars ---
combined_anova <- combined_anova %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# --- Build journal-style table ---
anova_table <- combined_anova %>%
  gt(groupname_col = "Dataset") %>%
  
  # Format numbers
  fmt_number(columns = c(sumsq, meansq, statistic), decimals = 3) %>%
  fmt_scientific(columns = p.value, decimals = 2) %>%
  
  # Merge p-value + stars
  cols_merge(
    columns = c(p.value, sig),
    pattern = "{1} {2}"
  ) %>%
  
  # Clean labels
  cols_label(
    term = "Source",
    df = "Df",
    sumsq = "Sum Sq",
    meansq = "Mean Sq",
    statistic = "F",
    p.value = "p-value"
  ) %>%
  
  # Table title + subtitle
  tab_header(
    title = "Effect of Sponsor Type on Functional Density",
    subtitle = "One-way ANOVA results for PFO and PEM datasets"
  ) %>%
  
  # Footnote
  tab_source_note(
    source_note = md("*Note:* p-values in scientific notation. Significance levels: *p* < 0.05 (*), < 0.01 (**), < 0.001 (***).")
  ) %>%
  
  # Style (journal-like)
  tab_options(
    table.font.size = 12,
    data_row.padding = px(4),
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12
  ) %>%
  
  cols_align(
    align = "center",
    -term
  )

##gtsave(anova_table, "anova_table_journal.docx")

###tukey formatting

# PFO Tukey
pfo_tukey <- as.data.frame(TukeyHSD(pfo_model)$sponsor_type) %>%
  mutate(
    comparison = rownames(.),
    Dataset = "PFO"
  )

# PEM Tukey
pem_tukey <- as.data.frame(TukeyHSD(pem_model)$sponsor_type) %>%
  mutate(
    comparison = rownames(.),
    Dataset = "PEM"
  )

# Combine
tukey_combined <- bind_rows(pfo_tukey, pem_tukey)

tukey_combined <- tukey_combined %>%
  rename(
    Difference = diff,
    Lower_CI = lwr,
    Upper_CI = upr,
    p.value = `p adj`
  ) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )


tukey_table <- tukey_combined %>%
  gt(groupname_col = "Dataset") %>%
  
  # Format numbers
  fmt_number(columns = c(Difference, Lower_CI, Upper_CI), decimals = 3) %>%
  fmt_scientific(columns = p.value, decimals = 2) %>%
  
  # Merge p-value + stars
  cols_merge(
    columns = c(p.value, sig),
    pattern = "{1} {2}"
  ) %>%
  
  # Labels
  cols_label(
    comparison = "Comparison",
    Difference = "Mean Diff",
    Lower_CI = "Lower CI",
    Upper_CI = "Upper CI",
    p.value = "p-value"
  ) %>%
  
  # Title + subtitle (match ANOVA style)
  tab_header(
    title = "Pairwise Comparisons of Sponsor Type",
    subtitle = "Tukey HSD post hoc test (PFO and PEM datasets)"
  ) %>%
  
  # Footnote
  tab_source_note(
    source_note = md("*Note:* Values are differences in log-transformed means. Confidence intervals are 95%. Significance: *p* < 0.05 (*), < 0.01 (**), < 0.001 (***).")
  ) %>%
  
  # Style (match ANOVA)
  tab_options(
    table.font.size = 12,
    data_row.padding = px(4),
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12
  ) %>%
  
  cols_align(
    align = "center",
    -comparison
  )

#gtsave(tukey_table, "tukey_table_journal.docx")

###Violin Plot
sponsor_cols <- c(
  "PE" = "#8B0000",
  "Government"     = "#90EE90",
  "Nonprofit"      = "#800080",
  "Listed"         = "#FFA500",
  "Private"        = "#00FFFF"
)

plot_violin_box <- function(data, y_var, y_label) {
  ggplot(data, aes(x = sponsor_type, y = .data[[y_var]], fill = sponsor_type)) +
    
    #Violin
    geom_violin(trim = F, alpha = 0.5, colour = NA, #bounds = c(0,1.5) 
                ) +
    
    # Boxplot
    geom_boxplot(
      width = 0.4,
      outliers = F,
      alpha = 0.7
    ) +
    
    #geom_jitter(aes(colour = sponsor_type), width = 0.15, alpha = 0.6, size = 1.5) +
    
    # Mean point (make visible)
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,          # filled circle with border
      size = 4,            # larger
      fill = "white",      # contrast fill
      colour = "black",    # strong outline
      stroke = 1.2         # thicker border
    )  +
    
    
    scale_fill_manual(values = sponsor_cols) +
    scale_colour_manual(values = c("black","black","black","black","black")) +
    
    labs(
      x = "Sponsor Type",
      y = y_label
    ) +
    
    
    theme_minimal() +
    theme(
      legend.position = "none",
      
      text = element_text(size = 16, family = "sans")
    )
} 

pfo_credit_plot <- plot_violin_box(
  functional_density_pfo,
  "avg_credit_acres",
  "Credit Acres"
)

pfo_log_plot <- plot_violin_box(
  functional_density_pfo,
  "log_avg_credit_acres",
  "Log Credit Acres"
)

pem_credit_plot <- plot_violin_box(
  functional_density_pem,
  "avg_credit_acres",
  "Credit Acres"
)

pem_log_plot <- plot_violin_box(
  functional_density_pem,
  "log_avg_credit_acres",
  "Log Credit Acres"
)

pfo_credit_plot <- pfo_credit_plot + ggtitle("PFO (Raw)")
pfo_log_plot    <- pfo_log_plot + ggtitle("PFO (Log)")
pem_credit_plot <- pem_credit_plot + ggtitle("PEM (Raw)")
pem_log_plot    <- pem_log_plot + ggtitle("PEM (Log)")

(pfo_credit_plot | pem_credit_plot ) +

  plot_annotation(
    tag_levels = "A"
  )

(pfo_log_plot | pem_log_plot ) +
  
  plot_annotation(
    tag_levels = "A "
  )
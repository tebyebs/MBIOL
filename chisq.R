library(dplyr)
library(here)

all_sponsors <- read.csv("full_sorted_data/all_sponsors.csv") %>%
  filter(bank_status != "Suspended")
         #,
         #sponsor_type != "Listed")

sponsor_status_table <- all_sponsors %>%
  count(sponsor_type, bank_status) %>%
  group_by(bank_status) %>%
  mutate(
    total = sum(n),
    percent = (n / total) * 100
  ) %>%
  ungroup()

library(tidyr)
library(gt)

gt_table <- sponsor_status_table %>%
  mutate(
    value = sprintf("%d (%.1f%%)", n, percent)
  ) %>%
  select(sponsor_type, bank_status, value) %>%
  pivot_wider(
    names_from = bank_status,
    values_from = value
  ) %>%
  gt() %>%
  tab_header(
    title = "Distribution of Sponsor Types Within Bank Status Categories"
  ) %>%
  cols_label(
    sponsor_type = "Sponsor Type"
  ) %>%
  cols_align(
    align = "center",
    -sponsor_type
  ) %>%
  tab_source_note(
    source_note = "Values shown as count (percentage within bank status; columns sum to 100%)."
  )



#gtsave(gt_table, "bankstatus.docx")

chisq_data <- table(all_sponsors$sponsor_type, all_sponsors$bank_status)
chisq_result <- chisq.test(chisq_data)
chisq_result

chisq_result$expected

chisq_result$stdres


library(reshape2)

library(ggplot2)

# convert to dataframe
residuals_df <- as.data.frame(as.table(chisq_result$stdres))
colnames(residuals_df) <- c("Sponsor_Type", "Bank_Status", "Residual")

ggplot(residuals_df, aes(x = Bank_Status, y = Sponsor_Type, fill = Residual)) +
  geom_tile() +
  
  # show values
  geom_text(aes(label = round(Residual, 2)), size = 4) +
  
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  
  labs(
    x = "Bank Status",
    y = "Sponsor Type",
    fill = "Std Residual"
  ) +
  
  theme_minimal()  +
  theme(
    
    text = element_text(size = 16, family = "sans")
  )


library(dplyr)
library(ggplot2)

residuals_df <- as.data.frame(as.table(chisq_result$stdres))
colnames(residuals_df) <- c("Sponsor_Type", "Bank_Status", "Residual")

residuals_df <- residuals_df %>%
  mutate(
    stars = case_when(
      abs(Residual) > 4 ~ "***",
      abs(Residual) > 3 ~ "**",
      abs(Residual) > 2 ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(Residual, 2), stars)
  )

ggplot(residuals_df, aes(x = Bank_Status, y = Sponsor_Type, fill = Residual)) +
  geom_tile() +
  
  geom_text(aes(label = label), size = 4) +
  
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  
  labs(
    x = "Bank Status",
    y = "Sponsor Type",
    fill = "Std Residual"
  ) +
  
  theme_minimal()  +
  theme(
    
    text = element_text(size = 16, family = "sans")
  )
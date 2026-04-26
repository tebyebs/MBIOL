#packages
library(dplyr)                 # Data manipulation
library(janitor)               # Data cleaning
library(purrr)                 # Functional programming
library(ggplot2)               # Data visualization
library(here)                  # File path handling
library(tidyr) 
library(scales)
library(networkD3)

#load in the data
all_sponsors <- read.csv("full_sorted_data/all_sponsors.csv")

###Part 2 - Analysis over time and pending vs approved - based on gains and losses linear model
library(gt)

bank_type_table <- all_sponsors %>%
  count(bank_type, name = "Count") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  rename(`Bank Type` = bank_type) %>%
  arrange(desc(Count))

# Create publication-ready table
bank_type_table %>%
  gt() %>%
  fmt_percent(
    columns = Percentage,
    decimals = 1
  ) %>%
  cols_align(
    align = "center",
    columns = c(Count, Percentage)
  ) %>%
  cols_label(
    `Bank Type` = "Bank Type",
    Count = "Count",
    Percentage = "Percentage (%)"
  ) %>%
  tab_header(
    title = "Distribution of Bank Types",
    subtitle = "Summary of categories in the RIBITS dataset"
  )
#filter for banks with no year established,maybe use bank status date instead
#ALTERNATIVELY, can ignore year established and just do approved vs pending 

###Part 3 - Sankey Diagram 

flows <- all_sponsors %>%
  count(bank_type, sponsor_type) %>%
  rename(source = bank_type, target = sponsor_type, value = n)


# Create node list
nodes <- data.frame(
  name = unique(c(flows$source, flows$target))
)
# Calculate totals per bank_type
source_totals <- flows %>%
  group_by(source) %>%
  summarise(total = sum(value))

# Calculate totals per sponsor_type
target_totals <- flows %>%
  group_by(target) %>%
  summarise(total = sum(value))

# Combine
node_totals <- bind_rows(
  source_totals %>% rename(name = source),
  target_totals %>% rename(name = target)
)

nodes <- nodes %>%
  left_join(node_totals, by = "name") %>%
  mutate(label = paste0(name, " (", total, ")"))

# Convert to indices
flows$source_id <- match(flows$source, nodes$name) - 1
flows$target_id <- match(flows$target, nodes$name) - 1

p <- sankeyNetwork(
  Links = flows,
  Nodes = nodes,
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "label",   # ← MUST be "label", not "name"
  fontSize = 18,
  nodeWidth = 40
)
p
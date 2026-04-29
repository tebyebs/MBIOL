#packages
library(dplyr)                 # Data manipulation
library(janitor)               # Data cleaning
library(purrr)                 # Functional programming
library(ggplot2)               # Data visualization
library(here)                  # File path handling
library(tidyr) 
library(scales)
library(networkD3)
library(htmlwidgets)
library(gt)

#load in the data
all_sponsors <- read.csv("full_sorted_data/all_sponsors.csv")

###Part 2 - Analysis over time and pending vs approved - based on gains and losses linear model

bank_type_table <- all_sponsors %>%
  count(bank_type, name = "Count") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  rename(`Bank Type` = bank_type) %>%
  arrange(desc(Count))

# Create publication-ready table
bank_table <- bank_type_table %>%
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

#gtsave(bank_table, "bank_type_table.docx")

#filter for banks with no year established,maybe use bank status date instead
#ALTERNATIVELY, can ignore year established and just do approved vs pending 

###Part 3 - Sankey Diagram 

#---------------------------
# 1. Prepare flows
#---------------------------
flows <- all_sponsors %>%
  count(bank_type, sponsor_type) %>%
  rename(source = bank_type, target = sponsor_type, value = n)

#---------------------------
# 2. Node list
#---------------------------
nodes <- data.frame(
  name = unique(c(flows$source, flows$target)),
  stringsAsFactors = FALSE
)

#---------------------------
# 3. Calculate totals
#---------------------------

# Totals for bank_type (sources)
bank_totals <- flows %>%
  group_by(source) %>%
  summarise(total = sum(value), .groups = "drop") %>%
  rename(name = source)

# Totals for sponsor_type (targets)
sponsor_totals <- flows %>%
  group_by(target) %>%
  summarise(total = sum(value), .groups = "drop") %>%
  rename(name = target)

# Combine totals
node_totals <- bind_rows(bank_totals, sponsor_totals)

# Merge into nodes
nodes <- nodes %>%
  left_join(node_totals, by = "name") %>%
  mutate(
    total = ifelse(is.na(total), 0, total),
    label = paste0(name, " (", total, ")")
  )

#---------------------------
# 4. Colours
#---------------------------

# Bank type colours
bank_colours <- c(
  "Public Commercial" = "lightgreen",  
  "Private Nonprofit" = "violet",   
  "Single-Client" = "yellow",      
  "Combination Public/Private" = "#00A86A", 
  "Private Commercial" = "purple"
)

# Sponsor type colours (from your brief)
sponsor_colours <- c(
  "PE" = "#8B0000",
  "Government"     = "#90EE90",
  "Nonprofit"      = "violet",
  "Listed"         = "#FFA500",
  "Private"        = "#00FFFF"
)

colour_map <- c(bank_colours, sponsor_colours)

#---------------------------
# 5. Indexing
#---------------------------
flows$source_id <- match(flows$source, nodes$name) - 1
flows$target_id <- match(flows$target, nodes$name) - 1

# Links coloured by bank_type
flows$link_group <- flows$source

#---------------------------
# 6. Colour scale
#---------------------------
colourJS <- paste0(
  "d3.scaleOrdinal().domain([",
  paste0("'", names(colour_map), "'", collapse = ","),
  "]).range([",
  paste0("'", colour_map, "'", collapse = ","),
  "])"
)

#---------------------------
# 7. Build Sankey
#---------------------------
p <- sankeyNetwork(
  Links = flows,
  Nodes = nodes,
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "label",        # ✅ shows counts
  LinkGroup = "target",
  colourScale = JS(colourJS),
  fontSize = 18,
  nodeWidth = 30
)

#---------------------------
# 8. Force node colours + Arial
#---------------------------
p <- onRender(p, '
function(el) {

  var bankColours = {
    "Public Commercial": "lightgreen",
    "Private Nonprofit": "violet",
    "Single-Client": "darkgreen",
    "Combination Public/Private": "#00A86A",
    "Private Commercial": "cyan"
  };

  var sponsorColours = {
    "PE": "#8B0000",
    "Government": "#90EE90",
    "Nonprofit": "violet",
    "Listed": "#FFA500",
    "Private": "#00FFFF"
  };

  // Colour nodes correctly
  d3.select(el).selectAll(".node rect")
    .style("fill", function(d) {
      var name = d.name.replace(/ \\(.*\\)/, "");
      if (bankColours[name]) return bankColours[name];
      if (sponsorColours[name]) return sponsorColours[name];
      return "#cccccc";
    });

  // Arial font
  d3.select(el).selectAll("text")
    .style("font-family", "Arial, sans-serif");
}
')

p

###table of naming confusion
cols <- c("pe_owner", "listing", "private", "govt", "nonprofit")

# Function to process each column
get_top_inconsistency <- function(df, col_name) {
  
  col_sym <- rlang::sym(col_name)
  
  # Step 1: Find most common non-yes/no entry
  top_entry <- df %>%
    filter(!is.na(!!col_sym)) %>%
    filter(!tolower(!!col_sym) %in% c("yes", "no")) %>%
    count(!!col_sym, sort = TRUE) %>%
    slice(1) %>%
    pull(!!col_sym)
  
  # Step 2: Filter rows for that entry
  filtered <- df %>%
    filter(!!col_sym == top_entry)
  
  # Step 3: Count banks
  n_banks <- nrow(filtered)
  
  # Step 4: Get top 10 sponsor_name variants
  top_names <- filtered %>%
    count(sponsor_name, sort = TRUE) %>%
    slice_head(n = 10) %>%
    mutate(label = paste0(sponsor_name, " (", n, ")")) %>%
    summarise(names = paste(label, collapse = "; ")) %>%
    pull(names)
  
  # Step 5: Extract sponsor_type (most common)
  sponsor_type <- filtered %>%
    count(sponsor_type, sort = TRUE) %>%
    slice(1) %>%
    pull(sponsor_type)
  
  tibble(
    sponsor_type = sponsor_type,
    name = top_entry,
    banks = n_banks,
    top_10_sponsor_names = top_names
  )
}

# Apply across all columns
results <- map_dfr(cols, ~get_top_inconsistency(all_sponsors, .x))

gt_table <- results %>%
  rename(
    `Sponsor Type` = sponsor_type,
    `Entity Name` = name,
    `Number of Banks` = banks,
    `Top 10 Sponsor Name Variants` = top_10_sponsor_names
  ) %>%
  gt() %>%
  tab_header(
    title = "Top Sponsor Naming Inconsistencies"
  ) %>%
  cols_align(
    align = "left",
    columns = c(`Sponsor Type`, `Entity Name`, `Top 10 Sponsor Name Variants`)
  ) %>%
  cols_align(
    align = "center",
    columns = `Number of Banks`
  ) %>%
  cols_width(
    `Sponsor Type` ~ px(120),
    `Entity Name` ~ px(200),
    `Number of Banks` ~ px(120),
    `Top 10 Sponsor Name Variants` ~ px(500)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

#gtsave(gt_table, "naming_inconsistencies.docx")

###results table
library(dplyr)
library(stringr)
library(purrr)
library(gt)
library(scales)

cols <- c("pe_owner", "listing", "private", "govt", "nonprofit")

# Total banks across entire dataset
total_all_banks <- nrow(all_sponsors)

# ---------------------------
# Helper: clean + merge entities
# ---------------------------
clean_entities <- function(df, col_name) {
  
  col_sym <- rlang::sym(col_name)
  
  df %>%
    filter(!is.na(!!col_sym)) %>%
    filter(!tolower(!!col_sym) %in% c("yes", "no")) %>%
    mutate(entity = !!col_sym) %>%
    mutate(
      entity = case_when(
        
        # Nonprofit merge
        col_name == "nonprofit" &
          str_detect(tolower(entity), "nature conservancy") ~ 
          "The Nature Conservancy",
        
        # Private merge
        col_name == "private" &
          str_detect(tolower(entity), "naturion|water and land solutions") ~ 
          "Naturion / Water & Land Solutions",
        
        TRUE ~ entity
      )
    )
}

# ---------------------------
# Main function per column
# ---------------------------
get_top3_entities <- function(df, col_name) {
  
  cleaned <- clean_entities(df, col_name)
  
  # Total banks per sponsor_type
  totals <- df %>%
    count(sponsor_type, name = "total_banks")
  
  # Count per entity
  counts <- cleaned %>%
    count(entity, sponsor_type, name = "banks")
  
  # Aggregate AFTER merging
  top3 <- counts %>%
    group_by(entity) %>%
    summarise(
      banks = sum(banks),
      sponsor_type = first(sponsor_type),
      .groups = "drop"
    ) %>%
    arrange(desc(banks)) %>%
    slice_head(n = 3)
  
  # Add proportions
  top3 %>%
    left_join(totals, by = "sponsor_type") %>%
    mutate(
      prop_within_type = banks / total_banks,
      prop_overall = banks / total_all_banks
    ) %>%
    select(sponsor_type, entity, banks, prop_within_type, prop_overall)
}

# ---------------------------
# Apply across columns
# ---------------------------
results <- map_dfr(cols, ~get_top3_entities(all_sponsors, .x))

# ---------------------------
# Format table
# ---------------------------
gt_table <- results %>%
  mutate(
    prop_within_type = percent(prop_within_type, accuracy = 0.1),
    prop_overall = percent(prop_overall, accuracy = 0.1)
  ) %>%
  rename(
    `Sponsor Type` = sponsor_type,
    `Entity Name` = entity,
    `Number of Banks` = banks,
    `Proportion within Sponsor Type` = prop_within_type,
    `Proportion of All Banks` = prop_overall
  ) %>%
  gt() %>%
  tab_header(
    title = "Major Players in Mitigation Banking"
  ) %>%
  cols_align(
    align = "left",
    columns = c(`Sponsor Type`, `Entity Name`)
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Number of Banks`, 
                `Proportion within Sponsor Type`, 
                `Proportion of All Banks`)
  ) %>%
  cols_width(
    `Sponsor Type` ~ px(140),
    `Entity Name` ~ px(300),
    `Number of Banks` ~ px(120),
    `Proportion within Sponsor Type` ~ px(200),
    `Proportion of All Banks` ~ px(200)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

gt_table
#gtsave(gt_table, "major_players.docx")


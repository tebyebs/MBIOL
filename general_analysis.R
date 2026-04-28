#packages
library(dplyr)                 # Data manipulation
library(janitor)               # Data cleaning
library(purrr)                 # Functional programming
library(ggplot2)   ci            # Data visualization
library(here)                  # File path handling
library(tidyr) 
library(scales)
library(networkD3)
library(htmlwidgets)


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
    "Nonprofit": "#800080",
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


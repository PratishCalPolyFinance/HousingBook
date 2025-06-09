# ------------------------------------------------------------------------------
# Title:     NYC Voucher Units Time Series Plot
# Author:    Pratish Patel
# Date:      2025-05-15
# Purpose:   Standardize city names, filter for NYC, and visualize total voucher 
#            units over time by program type using an accessible color palette.
# ------------------------------------------------------------------------------

# Clear the environment
rm(list = ls())

# Load required libraries
library(tidyverse)    # Data manipulation and visualization
library(openxlsx)     # Reading Excel files
library(stringdist)   # String matching (not used here but loaded)
library(here)         # File path handling
library(scales)       # Axis formatting
library(sysfonts)     # Font handling
library(showtext)     # Google font rendering

# Load dataset
load(file = here("data", "TimeSeriesTop5City.RData"))

# Create a mapping vector: raw name -> standardized city name
city_name_map <- c(
  # Los Angeles
  "CA LOS ANGELES" = "Los Angeles",
  "CA LOS ANGELES CITY (PART)" = "Los Angeles",
  "CA Los Angeles city, California" = "Los Angeles",
  
  # Chicago
  "IL CHICAGO" = "Chicago",
  "IL CHICAGO CITY (PART)" = "Chicago",
  "IL Chicago city, Illinois" = "Chicago",
  
  # Boston
  "MA BOSTON" = "Boston",
  "MA BOSTON CITY" = "Boston",
  "MA Boston city, Massachusetts" = "Boston",
  
  # New York
  "NY NEW YORK" = "New York",
  "NY NEW YORK CITY (PART)" = "New York",
  "NY New York city, New York" = "New York",
  
  # Philadelphia
  "PA PHILADELPHIA" = "Philadelphia",
  "PA PHILADELPHIA CITY" = "Philadelphia",
  "PA Philadelphia city, Pennsylvania" = "Philadelphia"
)

# Standardize city names
combined_df <- combined_df %>%
  mutate(city = recode(name, !!!city_name_map))

# Load and apply Roboto Condensed font
font_add_google("Roboto Condensed", "roboto")
showtext_auto()
theme_set(theme_light(base_family = "roboto", base_size = 20))


# Define Okabe-Ito color palette (colorblind-friendly)
okabe_ito_colors <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
  "#0072B2", "#D55E00", "#CC79A7", "black"
)

# Plot total units over time by program type for NYC
ggplot(combined_df, aes(x = year, y = total_units, color = program_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Ten Years, Five Cities, Not Much Change",
    subtitle = "Voucher-supported housing units have remained \n nearly static from 2014 to 2024",
    color = "Program Type"
  ) +
  scale_color_manual(values = okabe_ito_colors[1:2]) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = label_comma()
  ) +
  scale_x_continuous(breaks = unique(combined_df$year)) +
  theme_light(base_family = "roboto", base_size = 20) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 24, color = "gray40", hjust = 0.5),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey90", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()
  ) + 
  facet_wrap(~city)

# Plot Expenditure over time by program type for NYC
ggplot(combined_df, aes(x = year, y = spending_per_month, color = program_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Rising Costs, Same Footprint",
    subtitle = "While the number of voucher-supported units has barely moved, \n the cost to support them has grown steadily across major U.S. cities",
    color = "Program Type"
  ) +
  scale_color_manual(values = okabe_ito_colors[1:2]) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = label_dollar()
  ) +
  scale_x_continuous(breaks = unique(combined_df$year)) +
  theme_light(base_family = "roboto", base_size = 20) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 24, color = "gray40", hjust = 0.5),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey90", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()
  ) + 
  facet_wrap(~city)

# Plot Expenditure over time by program type for NYC
ggplot(combined_df, aes(x = year, y = pct_occupied/100, color = program_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Opting Out of Choice",
    subtitle = "Across U.S. cities, landlords are increasingly turning away from Housing Choice Vouchers—\nleading to declining occupancy despite rising need",
    color = "Program Type"
  ) +
  scale_color_manual(values = okabe_ito_colors[1:2]) +
  scale_y_continuous(
    limits = c(0.7, 1.01),
    expand = expansion(mult = c(0, 0.05)),
    labels = label_percent()
  ) +
  scale_x_continuous(breaks = unique(combined_df$year)) +
  theme_light(base_family = "roboto", base_size = 20) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 24, color = "gray40", hjust = 0.5),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey90", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()
  ) + 
  facet_wrap(~city)

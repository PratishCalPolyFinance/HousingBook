###############################################################################
#  Washington DC Housing Voucher Density Analysis  –  All Households
#  Author: Pratish Patel
#  Date: 2025-06-11
###############################################################################

# Load required packages
library(tidycensus)
library(sf)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(here)
library(tidyr)
library(forcats)
library(scales)
library(purrr)

# Set up Census API key
census_api_key("64b43acef154d75c006c7d87a0dbda7d465c0d9c", install = FALSE)
options(tigris_use_cache = TRUE)

# Define constants
ACS_YEAR <- 2023
DC_FIPS <- "11001"  # Washington DC FIPS code
ACS_HOUSEHOLD_VAR <- "B25003_001"  # Total occupied housing units (households)

# Create output directory
dir.create("plots", recursive = TRUE, showWarnings = FALSE)

###############################################################################
# Step 1: Get Washington DC geography
###############################################################################

print("Loading Washington DC tract geometry...")

# Get Washington DC boundary (DC is both a state and county)
dc_boundary <- get_acs(
  geography = "county",
  variables = "B01001_001",
  state = "DC", 
  survey = "acs5", 
  year = ACS_YEAR,
  geometry = TRUE, 
  cb = TRUE
) %>%
  st_union()

# Get all DC census tracts
dc_tracts <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  state = "DC", 
  survey = "acs5", 
  year = ACS_YEAR,
  geometry = TRUE, 
  cb = TRUE
) %>%
  select(GEOID, geometry)

print(paste("Found", nrow(dc_tracts), "census tracts in Washington DC"))

###############################################################################
# Step 2: Get household data for all tracts
###############################################################################

print("Loading household data...")

# Get total household counts for each tract
household_data <- get_acs(
  geography = "tract",
  variables = c(households = ACS_HOUSEHOLD_VAR),
  state = "DC", 
  survey = "acs5", 
  year = ACS_YEAR
) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  mutate(households = as.numeric(households))

print(paste("Got household data for", nrow(household_data), "tracts"))

###############################################################################
# Step 3: Load HUD voucher data
###############################################################################

print("Loading HUD voucher data...")

# Download and read the HUD data
# Note: DC data is typically in the file that covers DC/MD/VA region
hud_url <- paste0(
  "https://www.huduser.gov/portal/datasets/pictures/files/",
  "TRACT_AK_MN_2024_2020census.xlsx"
)

hud_raw <- read.xlsx(hud_url)

# Filter for DC tracts and voucher programs
hud_data <- hud_raw %>%
  filter(
    program %in% c(3, 5),  # 3 = HCV, 5 = PBV
    state == "DC",
    substr(code, 1, 5) == DC_FIPS
  ) %>%
  transmute(
    GEOID = code,
    program = factor(program, levels = c(3, 5),
                     labels = c("Housing Choice Vouchers", "Project Based Vouchers")),
    units = case_when(
      total_units %in% c(-4, -1) ~ NA_real_,  # Handle missing data codes
      TRUE ~ as.numeric(total_units)
    )
  ) %>%
  group_by(GEOID, program) %>%
  summarise(units = sum(units, na.rm = TRUE), .groups = "drop") %>%
  distinct(GEOID, program, .keep_all = TRUE)

print(paste("Processed HUD data for", length(unique(hud_data$GEOID)), "tracts"))

###############################################################################
# Step 4: Combine data and calculate voucher density
###############################################################################

print("Calculating voucher density metrics...")

# Create a complete grid of all tracts and both programs
programs <- c("Housing Choice Vouchers", "Project Based Vouchers")
all_combinations <- expand_grid(
  GEOID = dc_tracts$GEOID, 
  program = factor(programs, levels = programs)
)

# Join all the data together
analysis_data <- all_combinations %>%
  left_join(hud_data, by = c("GEOID", "program")) %>%
  left_join(household_data, by = "GEOID") %>%
  mutate(
    # Replace missing voucher counts with 0
    units = replace_na(units, 0),
    # Calculate vouchers per 1000 households
    vouchers_per_1k = if_else(
      households == 0 | is.na(households), 
      0,
      (units / households) * 1000
    ),
    # Create density categories
    density_category = cut(
      vouchers_per_1k,
      breaks = c(-Inf, 0, 10, 25, 50, 100, Inf),
      labels = c("None", "<10", "10–25", "25–50", "50–100", "100+"),
      include.lowest = TRUE
    )
  ) %>%
  # Add geometry back
  right_join(dc_tracts %>% select(GEOID, geometry), by = "GEOID") %>%
  st_as_sf()

# Check the results
print("Summary of voucher density categories:")
print(table(analysis_data$density_category, analysis_data$program))

###############################################################################
# Step 5: Create the map
###############################################################################

print("Creating voucher density map...")

# Define colors for the density categories
density_colors <- c(
  "None" = "#E69F00", 
  "<10" = "#56B4E9", 
  "10–25" = "#009E73",
  "25–50" = "#F0E442", 
  "50–100" = "#CC79A7", 
  "100+" = "#000000"
)

# Create the map
voucher_map <- ggplot(analysis_data) +
  geom_sf(aes(fill = density_category), color = "white", linewidth = 0.1) +
  geom_sf(data = dc_boundary, fill = NA, color = "black", linewidth = 0.8) +
  facet_wrap(~ program, ncol = 2) +
  scale_fill_manual(
    values = density_colors,
    name = "Vouchers / 1,000 Households",
    drop = FALSE
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 11) +
  theme(
    strip.text = element_text(size = 14, face = "bold", margin = margin(b = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.key.width = unit(2.3, "cm"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0, margin = margin(b = 25)),
    plot.caption = element_text(size = 10, hjust = 1, color = "grey40"),
    plot.margin = margin(20, 15, 15, 15)
  ) +
  labs(
    title = "Voucher density across Washington DC (all households)",
    caption = paste(
      "Source: HUD Picture of Subsidized Households 2024;",
      "ACS 5-Year Estimates (", ACS_YEAR, ")", sep = " "
    )
  )

# Save the map
ggsave("plots/dc_voucher_density_map.png",
       voucher_map, width = 14, height = 8, dpi = 300, bg = "white")

print("Map saved to 'plots/dc_voucher_density_map.png'")

# Print some summary statistics
print("\nSummary Statistics:")
summary_stats <- analysis_data %>%
  st_drop_geometry() %>%
  group_by(program) %>%
  summarise(
    total_tracts = n(),
    total_vouchers = sum(units, na.rm = TRUE),
    mean_density = mean(vouchers_per_1k, na.rm = TRUE),
    median_density = median(vouchers_per_1k, na.rm = TRUE),
    tracts_below_10 = sum(vouchers_per_1k < 10, na.rm = TRUE),
    percent_below_10 = round((tracts_below_10 / total_tracts) * 100, 1),
    .groups = "drop"
  )

print(summary_stats)


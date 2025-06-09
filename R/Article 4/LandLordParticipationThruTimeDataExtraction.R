# -----------------------------------------------
# Compile HUD Picture Data (2014–2024) for Top Unit Cities
# -----------------------------------------------

# Clear the environment
rm(list = ls())

# Load required libraries
library(tidyverse)    # Data manipulation and piping
library(openxlsx)     # Reading Excel files
library(stringdist)   # Optional: string matching
library(here)         # Platform-independent file paths

# -----------------------------------------------
# Step 1: Generate URLs for HUD Picture Files
# -----------------------------------------------

# Define the range of years to process
years <- 2014:2024

# Construct the appropriate URL for each year
urls <- map_chr(
  years,
  ~ if_else(
    .x >= 2022,
    paste0("https://www.huduser.gov/portal/datasets/pictures/files/PLACE_", .x, "_2020census.xlsx"),
    paste0("https://www.huduser.gov/portal/datasets/pictures/files/PLACE_", .x, ".xlsx")
  )
)

# -----------------------------------------------
# Step 2: Load 2024 Data and Identify Top Unit Cities
# -----------------------------------------------

# This assumes the 2024 file was downloaded and saved earlier as "2024CityData.RData"
load("2024CityData.RData")  # Loads `df`

# Identify top 5 cities by total units across programs 3 (HCV) and 5 (PBV)
TopUnitCity <- df %>%
  filter(program %in% c(3, 5)) %>%
  group_by(code) %>%
  summarise(
    total_units = sum(total_units, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(total_units, n = 5) %>%
  select(code)

# -----------------------------------------------
# Step 3: Loop Through Years to Collect Matched Data
# -----------------------------------------------

# Initialize list to hold data for each year
all_data <- vector("list", length = length(urls))

# Loop through each year's file
for (i in seq_along(urls)) {
  # Read Excel file
  df_year <- openxlsx::read.xlsx(urls[i])
  
  # Standardize numeric columns
  df_year <- df_year %>%
    mutate(
      total_units  = as.numeric(total_units),
      pct_occupied = as.numeric(pct_occupied),
      hh_income    = as.numeric(hh_income)
    )
  
  # Filter for top cities and relevant programs
  matched_df <- df_year %>%
    filter(
      code %in% TopUnitCity$code,
      program %in% c(3, 5)
    ) %>%
    mutate(year = years[i])
  
  # Store result
  all_data[[i]] <- matched_df
}

# -----------------------------------------------
# Step 4: Combine and Save Final Time Series Dataset
# -----------------------------------------------

# Combine into a tidy data frame
combined_df <- bind_rows(all_data) %>%
  select(
    year,
    program_label,
    name,
    total_units,
    pct_occupied,
    hh_income,
    code,
    spending_per_month
  )

# Save the final dataset
save(combined_df, file = here("data", "TimeSeriesTop5City.RData"))

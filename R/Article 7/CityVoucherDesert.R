# ── Chicago Voucher Density by Rent Decile (per 1,000 Rental Units) ───────────
# Author: Pratish Patel
# Date: 2025-06-16

# ── Load packages ─────────────────────────────────────────────────────────────
library(tidyverse)
library(tidycensus)
library(openxlsx)
library(scales)
library(forcats)

# ── Confirm B25003_003 is "Renter-occupied housing units" ─────────────────────
vars_acs <- load_variables(2021, "acs5", cache = TRUE)
print(vars_acs %>% filter(name == "B25003_003"))

# ── Okabe-Ito palette for deciles ─────────────────────────────────────────────
okabe_ito <- c(
  orange     = "#E69F00",
  sky_blue   = "#56B4E9",
  green      = "#009E73",
  yellow     = "#F0E442",
  blue       = "#0072B2",
  vermillion = "#D55E00",
  purple     = "#CC79A7",
  black      = "#000000",
  gray       = "#999999"
)

# Repeat color set to match 10 deciles
decile_colors <- rep(okabe_ito[c("yellow", "green", "sky_blue", "blue", "vermillion")], length.out = 10)

# ── 1. Load and clean SAFMR data ───────────────────────────────────────────────
safmr_url <- "https://www.huduser.gov/portal/datasets/fmr/fmr2024/fy2024_safmrs_revised.xlsx"
safmr_clean <- read.xlsx(safmr_url) %>%
  select(
    ZIP.Code,
    SAFMR.2BR,
    HUD.Metro.Fair.Market.Rent.Area.Name
  ) %>%
  distinct(ZIP.Code, .keep_all = TRUE) %>%
  rename(
    zip_code       = ZIP.Code,
    safmr_rent_2br = SAFMR.2BR,
    safmr_area     = HUD.Metro.Fair.Market.Rent.Area.Name
  )

# ── 2. Load and clean HUD voucher data ────────────────────────────────────────
hud_url <- "https://www.huduser.gov/portal/datasets/pictures/files/Zipcode_2024_2020census.xlsx"
hud_raw <- read.xlsx(hud_url)

hcv_clean <- hud_raw %>%
  filter(program == 3) %>%
  distinct(code, .keep_all = TRUE) %>%
  transmute(
    zip_code       = as.character(code),
    total_units    = total_units,
    occupied_units = number_reported,
    program_type   = "Housing Choice Vouchers"
  )

pbv_clean <- hud_raw %>%
  filter(program == 5) %>%
  distinct(code, .keep_all = TRUE) %>%
  transmute(
    zip_code       = as.character(code),
    total_units    = total_units,
    occupied_units = number_reported,
    program_type   = "Project Based Vouchers"
  )

hud_clean <- bind_rows(hcv_clean, pbv_clean)

# ── 3. Combine HUD and SAFMR datasets ─────────────────────────────────────────
combined_data <- hud_clean %>%
  left_join(safmr_clean, by = "zip_code")

# ── 4. Fetch ACS renter-occupied unit counts ───────────────────────────────────
acs_rentals <- get_acs(
  geography = "zcta",
  variables = c(rental_units = "B25003_003"),
  year      = 2021,
  cache     = TRUE,
  geometry  = FALSE
) %>%
  select(
    zip_code     = GEOID,
    rental_units = estimate
  )

# ── 5. Join and compute density per 1,000 rental units ─────────────────────────
combined_with_rentals <- combined_data %>%
  left_join(acs_rentals, by = "zip_code") %>%
  filter(!is.na(total_units), !is.na(rental_units), rental_units > 0) %>%
  mutate(
    total_per_1k    = (total_units / rental_units) * 1000,
    occupied_per_1k = (occupied_units / rental_units) * 1000
  )

# ── 6. Add SAFMR Rent Deciles ──────────────────────────────────────────────────
combined_with_deciles <- combined_with_rentals %>%
  filter(!is.na(safmr_rent_2br), safmr_rent_2br > 0) %>%
  mutate(
    rent_decile = ntile(safmr_rent_2br, 10),
    rent_decile = factor(rent_decile, labels = paste0("D", 1:10))
  )

# ── 7. Aggregate average densities by program and decile ───────────────────────
agg_summary <- combined_with_deciles %>%
  group_by(program_type, rent_decile) %>%
  summarise(
    avg_total_per_1k    = mean(total_per_1k, na.rm = TRUE),
    avg_occupied_per_1k = mean(occupied_per_1k, na.rm = TRUE),
    .groups = "drop"
  )

# ── 8. Plotting function for bar charts ────────────────────────────────────────
plot_bar_density <- function(df, yvar, title_suffix) {
  y_label <- paste("Avg", gsub("_", " ", yvar), "(per 1,000 Rental Units)")
  
  ggplot(df, aes(x = rent_decile, y = .data[[yvar]], fill = rent_decile)) +
    geom_col(width = 0.6) +
    scale_fill_manual(values = decile_colors) +
    scale_y_continuous(labels = label_number(accuracy = 1)) +
    labs(
      title = paste(unique(df$program_type), title_suffix),
      x = "SAFMR Rent Decile",
      y = y_label,
      caption = "Data: HUD Picture of Subsidized Households & FY2024 SAFMR"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 15),
      axis.title.y  = element_text(face = "bold"),
      legend.position = "none"
    )
}

# ── 9. Generate and Display All Four Plots ─────────────────────────────────────
hcv_data <- filter(agg_summary, program_type == "Housing Choice Vouchers")
pbv_data <- filter(agg_summary, program_type == "Project Based Vouchers")

# Generate plots
plot_hcv_total    <- plot_bar_density(hcv_data, "avg_total_per_1k",    "– Total Units per 1K")
plot_hcv_occupied <- plot_bar_density(hcv_data, "avg_occupied_per_1k", "– Occupied Units per 1K")
plot_pbv_total    <- plot_bar_density(pbv_data, "avg_total_per_1k",    "– Total Units per 1K")
plot_pbv_occupied <- plot_bar_density(pbv_data, "avg_occupied_per_1k", "– Occupied Units per 1K")

# Display
print(plot_hcv_total)
print(plot_hcv_occupied)
print(plot_pbv_total)
print(plot_pbv_occupied)

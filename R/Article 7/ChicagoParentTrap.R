###############################################################################
#  Chicago Two-Parent Gap (Voucher vs Census) – Pretty, Two-Colour Edition
#  Author: Pratish Patel
#  Date: 2025-06-10
#
#  This script visualizes the disparity in two-parent household prevalence
#  between voucher households (HUD PSH data) and all families (ACS data)
#  across Chicago census tracts. It produces:
#    - Individual choropleth maps using a two-color palette
#    - A scatterplot comparing tract-level gaps
#
#  Data sources:
#    - HUD Picture of Subsidized Households (2024)
#    - American Community Survey 5-Year Estimates (2023)
###############################################################################

# ── 0. Packages & API Setup ─────────────────────────────────────────────────
library(tidycensus)
library(sf)
library(openxlsx)
library(dplyr)
library(ggplot2)

# Register API key and enable caching
census_api_key("64b43acef154d75c006c7d87a0dbda7d465c0d9c", install = FALSE)
options(tigris_use_cache = TRUE)

# ── 1. Load Chicago Tract Geometry ──────────────────────────────────────────
cook <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  year      = 2023,
  state     = "IL",
  county    = "Cook",
  survey    = "acs5",
  geometry  = TRUE,
  cb        = TRUE
) |>
  select(GEOID, geometry)

chi_outline <- get_acs(
  geography = "place",
  variables = "B01001_001",
  year      = 2023,
  state     = "IL",
  survey    = "acs5",
  geometry  = TRUE,
  cb        = TRUE
) |>
  filter(NAME == "Chicago city, Illinois") |>
  st_union()

tracts <- cook[chi_outline, ]

# ── 2. Load HUD PSH 2024 Voucher Data ───────────────────────────────────────
# Helper function to clean HUD values
num <- function(x) suppressWarnings(as.numeric(replace(x, x %in% c(-4, -1), NA)))

psh <- read.xlsx(
  "https://www.huduser.gov/portal/datasets/pictures/files/TRACT_AK_MN_2024_2020census.xlsx"
) |>
  filter(
    program == 3,
    state == "IL",
    substr(code, 1, 5) == "17031"
  ) |>
  transmute(
    GEOID     = code,
    gap_vouch = num(pct_2adults) - num(pct_1adult)
  )

# ── 3. Load ACS 2023 Family Structure Data ──────────────────────────────────
# ── 3. Load ACS 2023 True Two-Parent Household Data from B09005 ─────────────
acs <- get_acs(
  geography = "tract",
  state     = "IL",
  county    = "Cook",
  year      = 2023,
  survey    = "acs5",
  geometry  = FALSE,
  variables = c(
    total_kids     = "B09005_001",  # Total children
    married_couple = "B09005_002",  # Married-couple household
    cohabit_couple = "B09005_003",  # Cohabiting couple household
    male_only      = "B09005_004",  # Male, no partner
    female_only    = "B09005_005"   # Female, no partner
  )
) |>
  select(GEOID, variable, estimate) |>
  tidyr::pivot_wider(names_from = variable, values_from = estimate) |>
  mutate(
    two_parent = married_couple + cohabit_couple,
    one_parent = male_only + female_only,
    gap_cen = 100 * (two_parent - one_parent) / total_kids
  ) |>
  select(GEOID, gap_cen)

# ── 4. Merge & Classify Tract-Level Gaps ────────────────────────────────────
chi <- tracts |>
  left_join(psh, by = "GEOID") |>
  left_join(acs, by = "GEOID") |>
  mutate(
    gap_v_cat = factor(
      if_else(gap_vouch < 0, "Negative", "Positive"),
      levels = c("Negative", "Positive")
    ),
    gap_c_cat = factor(
      if_else(gap_cen < 0, "Negative", "Positive"),
      levels = c("Negative", "Positive")
    )
  )

# ── 5. Define Color Palette ─────────────────────────────────────────────────
pal <- c(
  Negative = "#E69F00",  # Orange
  Positive = "#0072B2"   # Blue
)

# ── 6A. Map: Voucher Households ─────────────────────────────────────────────
ggplot(chi) +
  geom_sf(aes(fill = gap_v_cat), colour = "white", linewidth = 0.85) +
  geom_sf(data = chi_outline, fill = NA, colour = "black", linewidth = 1.35) +
  scale_fill_manual(
    values = pal,
    drop = FALSE,
    na.value = "grey90"
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 11) +
  theme(
    legend.position     = "top",
    legend.title        = element_blank(),
    legend.key.width    = unit(2.5, "cm"),
    plot.title          = element_text(size = 18, face = "bold", hjust = 0),
    plot.caption        = element_text(size = 10, hjust = 1),
    plot.title.position = "panel",
    plot.margin         = margin(t = 20, r = 10, b = 10, l = 10)
  ) +
  labs(
    title   = "Where the Vouchers Aren’t:\nChicago’s Two-Parent Strongholds",
    caption = "Source: HUD Picture of Subsidized Households (2024)"
  )

# Save voucher-based gap map
ggsave(
  filename = here::here("plots","voucher_gap_map_chicago.png"),,
  plot = last_plot(),  # assumes the last plot was the voucher map
  width = 8,
  height = 8,
  dpi = 300,
  bg = "white"
)

# ── 6B. Map: All Families (ACS) ─────────────────────────────────────────────
ggplot(chi) +
  geom_sf(aes(fill = gap_c_cat), colour = "white", linewidth = 0.85) +
  geom_sf(data = chi_outline, fill = NA, colour = "black", linewidth = 1.35) +
  scale_fill_manual(
    values = pal,
    drop = FALSE,
    na.value = "grey90"
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 11) +
  theme(
    legend.position     = "top",
    legend.title        = element_blank(),
    legend.key.width    = unit(3, "cm"),
    plot.title          = element_text(size = 18, face = "bold", hjust = 0),
    plot.caption        = element_text(size = 10, hjust = 1),
    plot.title.position = "panel",
    plot.margin         = margin(t = 20, r = 10, b = 10, l = 10)
  ) +
  labs(
    title   = "Family Structure by Tract in Chicago:\nWhere Two-Parent Households Still Prevail",
    caption = "Source: American Community Survey 5-Year (2023)"
  )

ggsave(
  filename = here::here("plots","census_gap_map_chicago.png"),
  plot = last_plot(),
  width = 8,
  height = 8,
  dpi = 300,
  bg = "white"
)

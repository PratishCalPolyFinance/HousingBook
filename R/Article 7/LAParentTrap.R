###############################################################################
#  LA Two-Parent Gap (Voucher vs Census) – Pretty, Two-Colour Edition
#  Author: Pratish Patel
#  Date: 2025-06-10 (Adapted for LA)
#
#  This script visualizes the disparity in two-parent household prevalence
#  between voucher households (HUD PSH data) and all families (ACS data)
#  across LA census tracts.
###############################################################################

# ── 0. Packages & API Setup ─────────────────────────────────────────────────
library(tidycensus)
library(sf)
library(openxlsx)
library(dplyr)
library(ggplot2)

census_api_key("64b43acef154d75c006c7d87a0dbda7d465c0d9c", install = FALSE)
options(tigris_use_cache = TRUE)

# ── 1. Load LA Tract Geometry ───────────────────────────────────────────────
la_tracts <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  year      = 2023,
  state     = "CA",
  county    = "Los Angeles",
  survey    = "acs5",
  geometry  = TRUE,
  cb        = TRUE
) |>
  select(GEOID, geometry)

la_outline <- get_acs(
  geography = "place",
  variables = "B01001_001",
  year      = 2023,
  state     = "CA",
  survey    = "acs5",
  geometry  = TRUE,
  cb        = TRUE
) |>
  filter(NAME == "Los Angeles city, California") |>
  st_union()

tracts <- la_tracts[la_outline, ]

# ── 2. Load HUD PSH 2024 Voucher Data ───────────────────────────────────────
num <- function(x) suppressWarnings(as.numeric(replace(x, x %in% c(-4, -1), NA)))

psh <- read.xlsx(
  "https://www.huduser.gov/portal/datasets/pictures/files/TRACT_AK_MN_2024_2020census.xlsx"
) |>
  filter(
    program == 3,
    state == "CA",
    substr(code, 1, 5) == "06037"
  ) |>
  transmute(
    GEOID     = code,
    gap_vouch = num(pct_2adults) - num(pct_1adult)
  )

# ── 3. Load ACS 2023 Family Structure Data ──────────────────────────────────
acs <- get_acs(
  geography = "tract",
  state     = "CA",
  county    = "Los Angeles",
  year      = 2023,
  survey    = "acs5",
  geometry  = FALSE,
  variables = c(
    total_kids     = "B09005_001",
    married_couple = "B09005_002",
    cohabit_couple = "B09005_003",
    male_only      = "B09005_004",
    female_only    = "B09005_005"
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
la <- tracts |>
  left_join(psh, by = "GEOID") |>
  left_join(acs, by = "GEOID") |>
  mutate(
    gap_v_cat = factor(if_else(gap_vouch < 0, "Negative", "Positive"), levels = c("Negative", "Positive")),
    gap_c_cat = factor(if_else(gap_cen < 0, "Negative", "Positive"), levels = c("Negative", "Positive"))
  )

# ── 5. Define Color Palette ─────────────────────────────────────────────────
pal <- c(
  Negative = "#E69F00",
  Positive = "#0072B2"
)

# ── 6A. Map: Voucher Households ─────────────────────────────────────────────
ggplot(la) +
  geom_sf(aes(fill = gap_v_cat), colour = "white", linewidth = 0.85) +
  geom_sf(data = la_outline, fill = NA, colour = "black", linewidth = 1.35) +
  scale_fill_manual(values = pal, drop = FALSE, na.value = "grey90") +
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
    title   = "Where the Vouchers Aren’t:\nLA’s Two-Parent Strongholds",
    caption = "Source: HUD Picture of Subsidized Households (2024)"
  )

ggsave(
  filename = here::here("plots", "voucher_gap_map_la.png"),
  plot = last_plot(),
  width = 8,
  height = 8,
  dpi = 300,
  bg = "white"
)

# ── 6B. Map: All Families (ACS) ─────────────────────────────────────────────
ggplot(la) +
  geom_sf(aes(fill = gap_c_cat), colour = "white", linewidth = 0.85) +
  geom_sf(data = la_outline, fill = NA, colour = "black", linewidth = 1.35) +
  scale_fill_manual(values = pal, drop = FALSE, na.value = "grey90") +
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
    title   = "Family Structure by Tract in LA:\nWhere Two-Parent Households Still Prevail",
    caption = "Source: American Community Survey 5-Year (2023)"
  )

ggsave(
  filename = here::here("plots", "census_gap_map_la.png"),
  plot = last_plot(),
  width = 8,
  height = 8,
  dpi = 300,
  bg = "white"
)

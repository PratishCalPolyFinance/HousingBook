# ============================================================
# Chicago Voucher-Economics Map
# ------------------------------------------------------------
# This script:
#   1. Downloads the official HUD FY-2024 FMR workbook
#   2. Pulls the 2-bedroom FMR for the Chicago metro
#   3. Grabs tract-level median *gross* rent for 2-BR units
#   4. Adjusts for a tenant-paid utility allowance
#   5. Flags tracts where HUD’s offer beats the market
#   6. Exports a simple choropleth for the article
#
# Author: Pratish Patel
# Date: 2025-06-24
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 1. Load Required Packages
# ------------------------------------------------------------

required <- c(
  "tidyverse", "tidycensus", "tigris", "sf", "readxl",
  "here", "glue", "showtext"
)

for (pkg in required) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

options(tigris_use_cache = TRUE)

# ------------------------------------------------------------
# 2. Fonts and Color Palette
# ------------------------------------------------------------

font_add_google("Montserrat", "montserrat")
showtext_auto()

okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "black"
)

# ------------------------------------------------------------
# 3. Set Parameters
# ------------------------------------------------------------

census_year       <- 2022               # Latest ACS 5-year estimate
utility_allowance <- 0                  # Monthly utility allowance ($)
rent_var          <- "B25031_004"       # Median gross rent for 2-BR
state_fips        <- "17"               # Illinois
cook_fips         <- "031"              # Cook County
plots_dir         <- here("plots")

if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# ------------------------------------------------------------
# 4. Download HUD FY-2024 FMR Data for Chicago
# ------------------------------------------------------------

fmr_url  <- "https://www.huduser.gov/portal/datasets/fmr/fmr2022/FY22_FMRs_revised.xlsx"
tmp_file <- tempfile(fileext = ".xlsx")

download.file(fmr_url, destfile = tmp_file, mode = "wb", quiet = TRUE)

fmr_tbl <- read_excel(tmp_file)

fmr_2br <- fmr_tbl %>%
  filter(str_detect(tolower(areaname), "chicago")) %>%
  pull(fmr_2) %>%
  as.double()

fmr_2br <- fmr_2br[1] - utility_allowance

message(glue("✔  FY-2024 2-BR FMR for Chicago metro: ${fmr_2br}"))

# ------------------------------------------------------------
# 5. Pull ACS Tract-Level Median Gross Rent
# ------------------------------------------------------------

# Note: Add your Census key if you haven’t already.
# tidycensus::census_api_key("YOUR_KEY", install = TRUE, overwrite = TRUE)

rent_tracts <- get_acs(
  geography = "tract",
  variables = rent_var,
  year      = census_year,
  state     = state_fips,
  county    = cook_fips,
  geometry  = TRUE,
  cache     = TRUE
) %>%
  rename(gross_rent_2br = estimate) %>%
  select(GEOID, gross_rent_2br, geometry)

# ------------------------------------------------------------
# 6. Clip to Chicago City Limits
# ------------------------------------------------------------

chi_city <- places(state = state_fips, cb = TRUE, class = "sf") %>%
  filter(NAME == "Chicago") %>%
  st_transform(st_crs(rent_tracts))

rent_tracts_chi <- st_intersection(rent_tracts, chi_city)

# ------------------------------------------------------------
# 7. Economic Gap Calculation (HUD vs Market)
# ------------------------------------------------------------

rent_tracts_chi <- rent_tracts_chi %>%
  mutate(
    contract_rent    = pmax(gross_rent_2br - utility_allowance, 0),
    fmr_gap          = fmr_2br - contract_rent,
    hud_beats_market = fmr_gap > 0
  )

# Console Summary
rent_tracts_chi %>%
  st_drop_geometry() %>%
  summarise(
    tracts         = n(),
    hud_high_bid   = sum(hud_beats_market),
    pct_high_bid   = round(100 * hud_high_bid / tracts, 1),
    median_gap_hi  = median(fmr_gap[hud_beats_market], na.rm = TRUE),
    median_gap_lo  = median(fmr_gap[!hud_beats_market], na.rm = TRUE)
  ) %>%
  print()

# Bin gaps for visualization
rent_tracts_chi <- rent_tracts_chi %>%
  mutate(fmr_gap_bin = cut(
    fmr_gap,
    breaks = c(-Inf, -150, -50, 0, 100, 200, Inf),
    labels = c("≤ -150", "-150 to -50", "-50 to 0", "0 to 100", "100 to 200", "200+"),
    right = TRUE
  ))


# Define colors for bins
gap_colors <- c(
  "≤ -150"         = "#E69F00",
  "-150 to -50"    = "#56B4E9",
  "-50 to 0"       = "#009E73",
  "0 to 100"       = "#F0E442",
  "100 to 200"     = "#CC79A7",
  "200+"           = "#000000"
)


# ------------------------------------------------------------
# 8. Plot Choropleth
# ------------------------------------------------------------

# Build the map ---------------------------------------------------------------
voucher_advantage_map <- ggplot(rent_tracts_chi) +
  # Census-tract polygons, filled by HUD-to-market gap category
  geom_sf(aes(fill = fmr_gap_bin), color = "white", linewidth = 0.1) +
  
  # Chicago city boundary for reference
  geom_sf(data = chi_city, fill = NA, color = "black", linewidth = 0.8) +
  
  # Manual palette you defined earlier
  scale_fill_manual(
    values = gap_colors,
    name   = "HUD Advantage ($)",
    drop   = FALSE
  ) +
  
  coord_sf(expand = FALSE) +
  
  # Typography & layout to match `voucher_map` -------------------------------
theme_void(base_size = 11) +
  theme(
    # legend styling
    legend.position   = "right",
    legend.title      = element_text(face = "bold", size = 24),
    legend.text       = element_text(size = 24),
    legend.key.width  = unit(2.5, "cm"),
    
    # single-panel title styling (no facet strips here)
    plot.title        = element_text(
      size   = 50,
      face   = "bold",
      hjust  = 0,
      margin = margin(b = 25)
    ),
    
    plot.caption      = element_text(size = 10, hjust = 1, color = "grey40"),
    plot.margin       = margin(20, 15, 15, 15),
    
    # keep the whole canvas white
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.background   = element_rect(fill = "white", colour = NA)
  ) +
  labs(
    title   = "Where CHA Pays More Than the Market",
    caption = "Sources: HUD FY-2024 • 2022 ACS 5-Year Estimates"
  )

# save with the same spec you use for voucher_map
ggsave(
  "plots/chicago_fmr_gap_map.png",
  voucher_advantage_map,
  width  = 14,
  height = 8,
  dpi    = 300,
  bg     = "white"
)

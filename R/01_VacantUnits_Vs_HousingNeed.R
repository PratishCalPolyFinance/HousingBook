# Load required libraries
library(tidyverse)
library(systemfonts)
library(here)
library(tidycensus)
library(fuzzyjoin)
library(sf)
library(rmapshaper)
library(sysfonts)
library(showtext)
library(ggtext)
library(waterfalls)

# Set theme
theme_set(theme_light(base_family = "Roboto Condensed", base_size = 20))

# Clean workspace
rm(list = ls())

# Load and clean Housing Need data
housing_data <- read_csv(here("data", "2024HousingDemand.csv")) %>%
  select(-1) %>%
  mutate(CountyName = str_trim(str_remove(CountyName, "In 2023 in")))

source(here::here("CensusKey.R"))

# Define vacancy variables and fetch data for California counties
vacancy_vars <- c(
  total_vacant = "B25004_001",
  for_rent = "B25004_002",
  rented_not_occupied = "B25004_003",
  for_sale_only = "B25004_004",
  sold_not_occupied = "B25004_005",
  seasonal_use = "B25004_006",
  for_migrant_workers = "B25004_007",
  other_vacant = "B25004_008"
)

vacancy_data <- get_acs(
  geography = "county",
  state = "CA",
  variables = vacancy_vars,
  year = 2023,
  survey = "acs5"
) %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    adjusted_vacant = total_vacant - for_rent - rented_not_occupied - for_sale_only - sold_not_occupied - seasonal_use - for_migrant_workers,
    CountyName = str_trim(str_remove(NAME, " County, California"))
  )

# Merge housing need and vacancy data
final_data <- inner_join(vacancy_data, housing_data, by = "CountyName")

# Load and simplify shapefile for CA counties
ca_counties <- st_read(here("img", "Counties.shp")) %>%
  rmapshaper::ms_simplify(keep = 0.05)

# Merge spatial data with final dataset
ca_map <- inner_join(ca_counties, final_data, by = "CountyName") %>%
  mutate(VacancyToNeedRatio = pmin(2, total_vacant / demand_low_income, na.rm = TRUE))

# Add Google Fonts
font_add_google("Playfair Display", "playfair")
font_add_google("Fira Sans", "fira")
showtext_auto()

# Identify outliers based on Vacancy/Need ratio
outlier_threshold <- quantile(ca_map$VacancyToNeedRatio, 0.90, na.rm = TRUE)

ca_map <- ca_map %>%
  mutate(outlier = VacancyToNeedRatio > outlier_threshold)

# Compute centroids for labeling counties
ca_centroids <- ca_map %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1], 
    y = st_coordinates(.)[, 2]
  )

# San Francisco County details
sf_county <- ca_map %>% filter(CountyName == "San Francisco")
sf_centroid <- st_centroid(sf_county)
sf_coords <- as.data.frame(st_coordinates(sf_centroid))
sf_x <- sf_coords$X
sf_y <- sf_coords$Y

# Define Okabe-Ito colors
okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                      "#0072B2", "#D55E00", "#CC79A7", "black")

# Create the map plot
ggplot(ca_map) +
  geom_sf(aes(fill = VacancyToNeedRatio), color = "grey70", size = 0.4, alpha = 1) +
  scale_fill_gradientn(colors = okabe_ito_colors, na.value = "grey80", name = "Vacancy to Need Ratio") +
  geom_text(data = ca_centroids, aes(x = x, y = y, label = CountyName), size = 3, family = "fira", color = "white", check_overlap = TRUE) +
  geom_text(data = filter(ca_centroids, outlier), aes(x = x, y = y, label = CountyName), size = 4, family = "fira", fontface = "bold", color = "black", check_overlap = TRUE) +
  geom_sf(data = sf_county, fill = NA, color = "black", size = 1) +
  annotate("text", x = sf_x - 2, y = sf_y + 1, label = "SF", size = 12, color = "black", family = "fira", hjust = 1.5) +
  labs(
    title = "Empty Homes, Big Needs: A County-Level Look at California’s Housing Gap",
    subtitle = "Counties where vacant homes exceed 200% of affordable housing needs are <span style='color:black'>colored in black</span>.",
    caption = "Sources: US Census Bureau (vacant units), CHPC Housing Need Reports (housing need) - https://chpc.net/publications/housing-need-reports/.",
    fill = "Vacancy/Rent Ratio"
  ) +
  theme_void(base_family = "fira") +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, color = "grey40", hjust = 0.5),
    plot.caption = element_text(size = 10, color = "grey50", hjust = 0.5),
    legend.position = "right"
  ) +
  coord_sf(datum = NA, expand = FALSE)  # Expands map size within the plot

# Prepare waterfall chart
waterfall_data <- final_data %>%
  filter(CountyName == "San Francisco") %>%
  summarise(
    `For Rent` = sum(for_rent, na.rm = TRUE),
    `Seasonal Use` = sum(seasonal_use, na.rm = TRUE),
    `For Sale` = sum(for_sale_only, na.rm = TRUE),
    `Rented Not Occupied` = sum(rented_not_occupied, na.rm = TRUE),
    `Sold Not Occupied` = sum(sold_not_occupied, na.rm = TRUE),
    `For Migrant Workers` = sum(for_migrant_workers, na.rm = TRUE),
    `Other Vacant` = sum(other_vacant, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "value")

total_vacant <- sum(waterfall_data$value, na.rm = TRUE)

ggplot(waterfall_data, aes(x = reorder(category, value), y = value, fill = category)) +
  geom_col(width = 0.6, color = "black", alpha = 0.9) +
  geom_text(aes(label = scales::comma(value)), size = 5, fontface = "bold", color = "white", hjust = 1.2) +
  scale_fill_manual(values = okabe_ito_colors) +
  labs(
    title = "Breakdown of Vacant Units",
    subtitle = "Stepwise decomposition of total vacant units by category",
    caption = "Sources: US Census Bureau (vacant units), CHPC Housing Need Reports (housing need) - https://chpc.net/publications/housing-need-reports/."
  ) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0.5),
    plot.caption = element_text(size = 10, color = "grey50", hjust = 0.5),
    legend.position = "none"
  )

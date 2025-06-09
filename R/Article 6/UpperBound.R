# ===============================================
# HUD Analysis: Plots of Wage, Welfare, and ABAWD Bounds
# ===============================================

rm(list = ls())

# --- Install & load required packages ---
required_packages <- c("tidyverse", "showtext", "geojsonio", 
                       "patchwork", "openxlsx", "readr", "here", 
                       "sf", "cowplot", "stringr", "scales", "extrafont")

for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load font and color scheme
font_add_google("Montserrat", "montserrat")
showtext_auto()

okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "black"
)

# Consistent themes
theme_line <- theme_minimal(base_family = "montserrat", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, face = "italic"),
    plot.caption = element_text(size = 10, hjust = 1),
    axis.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    legend.position = "top",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

theme_bar <- theme_minimal(base_family = "montserrat", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.caption = element_text(size = 10, hjust = 1),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "top"
  )

# ===============================================
# Load HUD Data (2014–2024)
# ===============================================
years <- 2014:2024
urls <- tibble(
  year = years,
  url = if_else(
    year >= 2022,
    glue("https://www.huduser.gov/portal/datasets/pictures/files/STATE_{year}_2020census.xlsx"),
    glue("https://www.huduser.gov/portal/datasets/pictures/files/STATE_{year}.xlsx")
  )
)

state_data <- urls %>%
  mutate(data = map2(url, year, ~{
    message("Reading: ", .x)
    df <- tryCatch(read.xlsx(.x), error = function(e) return(NULL))
    if (!is.null(df)) mutate(df, year = .y) else NULL
  })) %>%
  filter(!map_lgl(data, is.null)) %>%
  pull(data) %>%
  bind_rows()

valid_states <- c(state.abb, "DC")
latest_year <- max(state_data$year)

# ===============================================
# Wage Income Trends: Line Plot
# ===============================================
ranked_data <- state_data %>%
  mutate(state = str_sub(name, 1, 2)) %>%
  filter(!is.na(pct_wage_major), program == 3, state %in% valid_states) %>%
  group_by(year) %>%
  mutate(rank = rank(-pct_wage_major, ties.method = "first")) %>%
  ungroup()

top_states <- ranked_data %>%
  filter(year == latest_year) %>%
  arrange(rank) %>%
  slice_head(n = 5) %>%
  pull(state)

plot_data <- ranked_data %>%
  filter(state %in% top_states) %>%
  mutate(year = as.integer(year))

line_plot <- ggplot(plot_data, aes(x = year, y = pct_wage_major, group = state, color = state)) +
  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "gray80", alpha = 0.5) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = rep(okabe_ito, length.out = length(unique(plot_data$state)))) +
  scale_y_continuous(limits = c(0, 100), labels = label_number(suffix = "%", accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), by = 2),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "A Voucher Program Without Wages",
    subtitle = "Wage income has never been a majority source for HCV households\nin any state over the past decade.",
    x = "Year",
    y = "% of Households with Wage as Major Income Source",
    caption = "Source: HUD Picture of Subsidized Households"
  ) +
  theme_line

ggsave("plots/line_plot_wage_trends.jpg", line_plot, width = 1200 / 96, height = 800 / 96, dpi = 96)

# ===============================================
# Bar Chart: Top 10 States by Wage Income
# ===============================================
bar_data <- ranked_data %>%
  filter(year == latest_year) %>%
  arrange(desc(pct_wage_major)) %>%
  slice_head(n = 10) %>%
  mutate(
    pct_wage_major = if_else(pct_wage_major > 1, pct_wage_major, pct_wage_major * 100),
    state = fct_reorder(state, pct_wage_major)
  )

bar_plot <- ggplot(bar_data, aes(x = state, y = pct_wage_major, fill = state)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct_wage_major, 1), "%")),
            hjust = 1.2, size = 4, family = "montserrat", fontface = "bold", color = "white") +
  scale_fill_manual(values = rep(okabe_ito, length.out = nrow(bar_data))) +
  scale_y_continuous(
    limits = c(0, max(bar_data$pct_wage_major) * 1.1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_flip() +
  labs(
    title = glue("Top 10 States by Wage as Major Income Source ({latest_year})"),
    x = NULL,
    y = "% of Households",
    caption = "Source: HUD Picture of Subsidized Households"
  ) +
  theme_bar

ggsave("plots/bar_plot_top10_wage.jpg", bar_plot, width = 1200 / 96, height = 800 / 96, dpi = 96)

# ===============================================
# Bar Chart: Household Bounds (Upper vs Lower)
# ===============================================

# Step 0: Standardize 'state' across all uses
state_data <- state_data %>%
  mutate(state = str_sub(name, 1, 2))

# Define latest year and valid states
latest_year <- max(state_data$year, na.rm = TRUE)
valid_states <- c(state.abb, "DC")

# Step 1: Construct bounds_households_2024
bounds_households_2024 <- state_data %>%
  filter(
    year == latest_year,
    program == 3,
    state %in% valid_states,
    !is.na(pct_wage_major),
    !is.na(pct_disabled_all),
    !is.na(pct_age25_50),
    !is.na(pct_age51_61),
    !is.na(pct_female_head_child),
    !is.na(pct_2adults),
    !is.na(pct_1adult),
    !is.na(total_units)
  ) %>%
  mutate(
    total_households = total_units,
    abawd_prob_lower = pct_wage_major / 100,
    pAge = (pct_age25_50 + pct_age51_61) / 100,
    pD = 1 - pct_disabled_all / 100,
    pC = pmax(pct_2adults, pct_1adult, pct_female_head_child, na.rm = TRUE) / 100,
    bound1 = pAge * pmin(1, pD / pAge) * pmin(1, pC / (pAge * pD)),
    bound2 = pC * pmin(1, pD / pC) * pmin(1, pAge / (pC * pD)),
    bound3 = pD * pmin(1, pC / pD) * pmin(1, pAge / (pD * pC)),
    abawd_prob_upper = pmin(bound1, bound2, bound3),
    abawd_upper_count = abawd_prob_upper * total_households,
    abawd_lower_count = abawd_prob_lower * total_households
  ) %>%
  select(state, abawd_upper_count, abawd_lower_count)

# Step 2: Pivot and relabel bound types
bounds_cleaned <- bounds_households_2024 %>%
  pivot_longer(cols = starts_with("abawd_"), names_to = "bound_type", values_to = "count") %>%
  mutate(
    bound_type = recode(bound_type,
                        "abawd_upper_count" = "Upper Bound",
                        "abawd_lower_count" = "Wage-Majority")
  )

# Step 3: Get correct total_units for each state
total_units_by_state <- state_data %>%
  filter(
    year == latest_year,
    program == 3,
    state %in% valid_states,
    !is.na(total_units)
  ) %>%
  group_by(state) %>%
  summarise(total_units = max(total_units, na.rm = TRUE), .groups = "drop")

# Step 4: Join total_units and calculate percent of total
bounds_with_total <- bounds_cleaned %>%
  left_join(total_units_by_state, by = "state") %>%
  filter(!is.na(total_units) & total_units > 0) %>%
  mutate(percent_of_total = 100 * count / total_units)

# Step 5: Summarize statistics by bound type
summary_stats <- bounds_with_total %>%
  group_by(bound_type) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),
    average = mean(count, na.rm = TRUE),
    median = median(count, na.rm = TRUE),
    min = min(count, na.rm = TRUE),
    max = max(count, na.rm = TRUE),
    number_of_states = n_distinct(state),
    average_percent_of_total = mean(percent_of_total, na.rm = TRUE)
  )

print(summary_stats)

# Load hex grid GeoJSON file
map_hex <- geojsonio::geojson_read(here::here("data", "us_states_hexgrid.geojson.json"), what = "sp")

# Clean names and convert to sf
map_hex@data <- map_hex@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

map_hex_sf <- sf::st_as_sf(map_hex) %>%
  mutate(id = iso3166_2)


# Prepare hex data for joining
bounds_hex <- bounds_with_total %>%
  filter(bound_type == "Upper Bound") %>%
  mutate(id = state)  # match to `map_hex_sf$id`

# Join with hex map
df_bounds_hex <- map_hex_sf %>%
  left_join(bounds_hex, by = "id")

# Compute centroids for state labels
centroids_bounds <- st_centroid(df_bounds_hex)
coords_bounds <- st_coordinates(centroids_bounds)
centroids_bounds <- as.data.frame(coords_bounds) %>%
  mutate(
    id = df_bounds_hex$id,
    id_long = stringr::str_wrap(df_bounds_hex$google_name, 12)
  )

# Plot hex map of percent_of_total
hex_upper_percent <- ggplot(df_bounds_hex) +
  geom_sf(aes(fill = percent_of_total), color = "white") +
  geom_text(data = centroids_bounds,
            aes(x = X, y = Y + 0.35, label = id),
            family = "Montserrat", fontface = "bold") +
  geom_text(data = centroids_bounds,
            aes(x = X, y = Y - 0.3, label = id_long),
            family = "Montserrat",
            color = "grey30", fontface = "bold",
            size = 2, lineheight = 0.8, vjust = 1) +
  scale_fill_gradientn(
    colors = c("#FDE725", "#35B779", "#31688E"),  # Yellow → Green → Blue,
    name = "Upper Bound %",
    limits = c(20, 60),
    breaks = seq(20, 60, by = 10),
    labels = scales::label_number(suffix = "%", accuracy = 1),
    oob = scales::squish
  ) +
  labs(
    title = "Where the Policy Could Hit Hardest?",
    subtitle = "Estimated upper bound of able-bodied HCV households by state.",
    caption = "Source: HUD Picture of Subsidized Households, 2024"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(3, "cm"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0),
    plot.caption = element_text(size = 10, hjust = 1),
    plot.title.position = "plot"
  )

# Display and save
print(hex_upper_percent)

ggsave(
  filename = here::here("plots", "UpperBound_Percent_Hex_2024.jpg"),
  plot = hex_upper_percent,
  width = 1280 / 96,
  height = 800 / 96,
  dpi = 96,
  units = "in"
)



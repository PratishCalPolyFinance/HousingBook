# ===============================================
# PHV_hexagon_map.R
# Analysis of total amount of people affected by the order
# ===============================================

# Clear workspace
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

# --- Set up fonts ---
font_add_google("Lobster", "Lobster")
font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Roboto", "roboto")
showtext_auto()

# --- ggplot theme updates ---
theme_set(theme_bw(base_family = "Lobster"))

# Define the bounding function
estimate_abawd_bounds <- function(total_units, people_per_unit, pct_age62plus,
                                  pct_disabled_all, pct_1adult, pct_2adults,
                                  pct_wage_major) {
  # Probabilities
  pE <- 1 - pct_age62plus / 100
  pD <- 1 - pct_disabled_all / 100
  pK <- 1 - (pct_1adult + pct_2adults) / 100
  people_estimated <- total_units * people_per_unit
  
  # Upper bound via Fréchet inequalities
  upper_prob <- min(
    pE,
    pD,
    pK,
    pE + pD - 1,
    pE + pK - 1,
    pD + pK - 1
  )
  upper_count <- upper_prob * people_estimated
  
  # Lower bound via % wage-major
  lower_prob <- pct_wage_major / 100
  lower_count <- lower_prob * people_estimated
  
  tibble(
    abawd_prob_upper = upper_prob,
    abawd_upper_count = upper_count,
    abawd_prob_lower = lower_prob,
    abawd_lower_count = lower_count
  )
}

# US level analysis
PHV_data_US <- openxlsx::read.xlsx("https://www.huduser.gov/portal/datasets/pictures/files/US_2024_2020census.xlsx")
PHV_abawd_US <- PHV_data_US %>%
  group_by(name) %>%
  summarise(across(
    c(total_units, people_per_unit, pct_age62plus, pct_disabled_all,
      pct_1adult, pct_2adults, pct_wage_major, months_from_movein, hh_income,spending_per_month),
    mean, na.rm = TRUE
  )) %>%
  rowwise() %>%
  mutate(
    out = list(estimate_abawd_bounds(
      total_units, people_per_unit, pct_age62plus,
      pct_disabled_all, pct_1adult, pct_2adults, pct_wage_major
    ))
  ) %>%
  unnest(out) %>%
  ungroup() %>%
  mutate(budget = total_units*spending_per_month)

PHV_abawd_US <- PHV_abawd_US %>%
  mutate(program = recode(name,
                          "U.S. Total,202/PRAC" = "Section 202 Supportive Housing",
                          "U.S. Total,811/PRAC" = "Section 811 Supportive Housing",
                          "U.S. Total,Housing Choice Vouchers" = "Housing Choice Vouchers",
                          "U.S. Total,Mod Rehab" = "Moderate Rehabilitation (Mod Rehab)",
                          "U.S. Total,Project Based Section 8" = "Project-Based Section 8",
                          "U.S. Total,Public Housing" = "Public Housing",
                          "U.S. Total,S236/BMIR" = "Section 236 / BMIR",
                          "U.S. Total,Summary of All HUD Programs" = "All HUD Programs"
  ))


# Define Okabe-Ito color palette
okabe_ito_colors <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
  "#0072B2", "#D55E00", "#CC79A7", "black"
)

# Reorder programs by descending budget
PHV_abawd_US <- PHV_abawd_US %>%
  arrange(desc(-budget)) %>%
  mutate(program = factor(program, levels = unique(program)))

# Final plot
ggplot(PHV_abawd_US, aes(x = program, y = budget)) +
  geom_col(fill = okabe_ito_colors[1], width = 0.9) +
  coord_flip() +
  labs(
    title = "Where the Money Goes: HUD’s Housing Support in 2024",
    subtitle = "Vouchers—both tenant- and project-based—account for the \n majority of federal housing aid",
    caption = "Source: HUD Picture of Subsidized Households (2024)",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = " M"), expand = expansion(mult = c(0, 0.05))) +
  theme_light(base_family = "roboto", base_size = 20) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0),
    plot.caption = element_text(size = 12, color = "gray50", hjust = 1),
    axis.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.3, color = "grey90", linetype = "dashed"),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey90", linetype = "dashed"),
    panel.border = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()
  )

# Save plot as 1280px wide JPEG
ggsave(
  filename = here::here("plots", "hud_budget_plot_2024.jpg"),
  width = 1280 / 96,   # Convert pixels to inches (assuming 96 dpi)
  height = 800 / 96,   # You can adjust the height for aspect ratio
  dpi = 96,
  units = "in",
  device = "jpeg"
)

# Average Stay of tenants by State

# --- Load and clean Public Housing Voucher data with move-in info ---
PHV_data_State <- openxlsx::read.xlsx("https://www.huduser.gov/portal/datasets/pictures/files/STATE_2024_2020census.xlsx")

PHV_data_filter <- PHV_data_State %>%
  filter(program == 3) %>%  # Housing Choice Vouchers
  slice(1:51) %>%
  select(name, months_from_movein) %>%
  mutate(
    ISO2 = substr(name, 1, 2),
    state_name = sub("^..\\s+", "", name)
  )

# --- Load state metadata ---
df_states <- readr::read_csv(here::here("data", "50_us_states_all_data.csv"),
                             col_names = FALSE) %>%
  dplyr::select(state = "X2", ISO2 = "X3") %>%
  add_row(state = "District of Columbia", ISO2 = "DC")

# --- Load hexagon grid ---
map_hex <- geojsonio::geojson_read(here::here("data", "us_states_hexgrid.geojson.json"), what = "sp")
map_hex@data <- map_hex@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

map_hex_sf <- st_as_sf(map_hex) %>%
  mutate(id = iso3166_2)

# --- Join Data ---
df_phv_hex <- map_hex_sf %>%
  left_join(df_states, by = c("id" = "state")) %>%
  left_join(PHV_data_filter, by = c("id" = "ISO2"))

# --- Join into hex map ---
df_phv_hex_movein <- map_hex_sf %>%
  left_join(df_states, by = c("id" = "state")) %>%
  left_join(PHV_data_filter, by = c("id" = "ISO2"))

# --- Compute centroids for labeling ---
centroids_movein <- st_centroid(df_phv_hex_movein)
centroids_coords_movein <- st_coordinates(centroids_movein)
centroids_movein <- as.data.frame(centroids_coords_movein) %>%
  mutate(
    id = df_phv_hex_movein$id,
    id_long = str_wrap(df_phv_hex_movein$google_name, 12)
  )

hex_phv_movein <- ggplot(df_phv_hex_movein) +
  geom_sf(aes(fill = months_from_movein), color = "white") +
  geom_text(data = centroids_movein,
            aes(x = X, y = Y + 0.35, label = id),
            family = "Montserrat",
            fontface = "bold") +
  geom_text(data = centroids_movein,
            aes(x = X, y = Y - 0.3, label = id_long),
            family = "Montserrat",
            color = "grey30",
            fontface = "bold",
            size = 2,
            lineheight = 0.8,
            vjust = 1) +
  scale_fill_gradientn(
    colors = okabe_ito_colors[1:4],
    name = "Months Since Move-In",
    limits = quantile(df_phv_hex_movein$months_from_movein, c(0.05, 0.95), na.rm = TRUE),
    oob = scales::squish
  ) +
  labs(
    title = "How Long Have Voucher Residents Stayed Put?",
    subtitle = "North Dakota shows the shortest average tenure at 70 months; some states exceed 10 years.",
    caption = "Source: HUD Picture of Subsidized Households, 2024"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(3, "cm"),
    plot.title = element_text(size = 24, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.title.position = "plot"
  )


# --- Display the map ---
print(hex_phv_movein)

# Save plot as 1280px wide JPEG
ggsave(
  filename = here::here("plots", "hud_MoveIn_plot_2024.jpg"),
  width = 1280 / 96,   # Convert pixels to inches (assuming 96 dpi)
  height = 800 / 96,   # You can adjust the height for aspect ratio
  dpi = 96,
  units = "in",
  device = "jpeg"
)



# --- Load Public Housing Voucher data ---
PHV_data <- openxlsx::read.xlsx("https://www.huduser.gov/portal/datasets/pictures/files/STATE_2024_2020census.xlsx")

PHV_abawd_state <- PHV_data %>%
  filter(program == 3) %>%
  group_by(name) %>%
  summarise(across(
    c(total_units, people_per_unit, pct_age62plus, pct_disabled_all,
      pct_1adult, pct_2adults, pct_wage_major),
    mean, na.rm = TRUE
  )) %>%
  rowwise() %>%
  mutate(
    out = list(estimate_abawd_bounds(
      total_units, people_per_unit, pct_age62plus,
      pct_disabled_all, pct_1adult, pct_2adults, pct_wage_major
    ))
  ) %>%
  unnest(out) %>%
  ungroup()

PHV_abawd_state <- PHV_abawd_state %>%
  mutate(ISO2 = substr(name, 1, 2))


# Merge into hex grid
df_abawd_hex <- map_hex_sf %>%
  left_join(df_states, by = c("id" = "state")) %>%
  left_join(PHV_abawd_state, by = c("id" = "ISO2"))

# Compute centroids
centroids_abawd <- st_centroid(df_abawd_hex)
centroids_coords_abawd <- st_coordinates(centroids_abawd)
centroids_abawd <- as.data.frame(centroids_coords_abawd) %>%
  mutate(
    id = df_abawd_hex$id,
    id_long = stringr::str_wrap(df_abawd_hex$google_name, 12)
  )

# Define percentile limits
lower_limit <- quantile(df_abawd_hex$abawd_prob_upper, 0.05, na.rm = TRUE)
upper_limit <- quantile(df_abawd_hex$abawd_prob_upper, 0.95, na.rm = TRUE)

# Plot the map
hex_abawd_prob <- ggplot(df_abawd_hex) +
  geom_sf(aes(fill = abawd_prob_upper), color = "white") +
  geom_text(data = centroids_abawd,
            aes(x = X, y = Y + 0.35, label = id),
            family = "Montserrat",
            fontface = "bold") +
  geom_text(data = centroids_abawd,
            aes(x = X, y = Y - 0.3, label = id_long),
            family = "Montserrat",
            color = "grey30",
            fontface = "bold",
            size = 2,
            lineheight = 0.8,
            vjust = 1) +
  scale_fill_gradientn(
    colors = okabe_ito_colors[1:4],
    name = "ABAWD Upper Bound",
    limits = c(0.20, 0.45),
    breaks = seq(0.20, 0.45, by = 0.05),
    labels = scales::label_percent(accuracy = 1),
    oob = scales::squish
  ) +
  labs(
    title = "Where the Rules Will Bite Hardest",
    subtitle = "Estimated Upper Bound of Able-Bodied Adults Without Dependents in HCV Households",
    caption = "Source: HUD Picture of Subsidized Households, 2024"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.key.width = unit(3, "cm"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.title.position = "plot"
  )

# Display the map
print(hex_abawd_prob)

# Save plot as 1280px wide JPEG
ggsave(
  filename = here::here("plots", "UpperBound_plot_2024.jpg"),
  width = 1280 / 96,   # Convert pixels to inches (assuming 96 dpi)
  height = 800 / 96,   # You can adjust the height for aspect ratio
  dpi = 96,
  units = "in",
  device = "jpeg"
)




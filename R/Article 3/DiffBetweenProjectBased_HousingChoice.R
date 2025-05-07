# ===============================================
# PHV_hexagon_map.R
# Landlord Participation Difference: Housing Choice vs Project-Based Vouchers
# ===============================================

# Clear workspace
rm(list = ls())

# --- Install & load required packages ---
required_packages <- c("tidyverse", "showtext", "geojsonio", 
                       "patchwork", "openxlsx", "readr", "here", 
                       "sf", "cowplot", "stringr", "scales")

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
showtext_auto()

# --- ggplot theme updates ---
theme_set(theme_bw(base_family = "Lobster"))

# --- Load Public Housing Voucher data ---
PHV_data <- openxlsx::read.xlsx("https://www.huduser.gov/portal/datasets/pictures/files/STATE_2024_2020census.xlsx")

# --- Filter and clean the data ---
PHV_data_filter <- PHV_data %>%
  filter(program %in% c(3, 5)) %>%
  mutate(
    ISO2 = substr(name, 1, 2),
    state_name = sub("^..\\s+", "", name)
  ) %>%
  select(ISO2, state_name, program, pct_occupied)

# --- Pivot wider: one row per state ---
PHV_data_wide <- PHV_data_filter %>%
  pivot_wider(names_from = program, values_from = pct_occupied, names_prefix = "program_") %>%
  mutate(diff_pct_occupied = program_3 - program_5)

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

# --- Join data ---
df_phv_hex <- map_hex_sf %>%
  left_join(df_states, by = c("id" = "state")) %>%
  left_join(PHV_data_wide, by = c("id" = "ISO2"))

# --- Compute centroids for labeling ---
centroids_sf <- st_centroid(df_phv_hex)
centroids_coords <- st_coordinates(centroids_sf)
centroids <- as.data.frame(centroids_coords) %>%
  mutate(
    id = df_phv_hex$id,
    id_long = str_wrap(df_phv_hex$google_name, 12)
  )

# --- Create diverging color scale (centered at 0) ---
hex_phv_diff <- ggplot(df_phv_hex) +
  geom_sf(aes(fill = diff_pct_occupied), color = "white") +
  geom_text(data = centroids,
            aes(x = X, y = Y + 0.35, label = id),
            family = "Montserrat",
            fontface = "bold") +
  geom_text(data = centroids,
            aes(x = X, y = Y - 0.3, label = id_long),
            family = "Montserrat",
            color = "grey30",
            fontface = "bold",
            size = 3,
            lineheight = 0.8,
            vjust = 1) +
  scale_fill_gradient2(
    low = "#56B4E9",   # Blue for negative
    mid = "white",    # White for zero
    high = "#D55E00", # Red for positive
    midpoint = 0,
    name = "Housing Choice - Project Based (%)"
  ) +
  ggtitle("Difference in Landlord Participation:\nHousing Choice vs Project-Based Vouchers by State") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.title = element_text(size = 25, face = "bold", hjust = 0)
  )

# --- Display the map ---
print(hex_phv_diff)

# --- Prepare data for Top 10 lollipop plot ---

# --- Prepare data ---

# Take Top 10 states
# --- Prepare data for slope graph ---

# --- Prepare data for side-by-side bar chart ---

# Take Top 10 states with biggest negative difference
# --- Prepare data for side-by-side bar chart ---

# Take Top 10 states with biggest negative difference
# --- Prepare data for side-by-side bar chart ---

# Take Top 10 states with biggest negative difference
top10_states <- PHV_data_wide %>%
  arrange(diff_pct_occupied) %>%
  slice(1:10) %>%
  pull(state_name)

# Filter and reshape
PHV_bar_top10 <- PHV_data_wide %>%
  filter(state_name %in% top10_states) %>%
  select(state_name, program_3, program_5) %>%
  pivot_longer(cols = starts_with("program_"),
               names_to = "Program",
               names_prefix = "program_",
               values_to = "Pct_Occupied") %>%
  mutate(
    Program = case_when(
      Program == "3" ~ "Housing Choice Vouchers",
      Program == "5" ~ "Project-Based Section 8"
    ),
    state_name = factor(state_name, levels = rev(top10_states))  # Flip order for nicer display
  )

# --- Build side-by-side bar chart with coord_flip() and labels inside ---

fill_colors <- c("Housing Choice Vouchers" = "#E69F00", 
                 "Project-Based Section 8" = "#56B4E9")

side_by_side_plot_flipped <- ggplot(PHV_bar_top10, aes(x = state_name, y = Pct_Occupied, fill = Program)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(
    aes(label = paste0(Pct_Occupied, "%")),
    position = position_dodge(width = 0.8),
    hjust = 1.1,  # Inside the bar
    color = "white",
    fontface = "bold",
    size = 4,
    family = "Montserrat"
  ) +
  scale_fill_manual(values = fill_colors) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = NULL  # Remove y-axis labels
  ) +
  labs(
    title = "2024 Housing Choice vs Project-Based Section 8 Occupancy",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  coord_flip() +
  theme_minimal(base_family = "Montserrat") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.text.y = element_text(face = "bold"),
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )

# --- Display the plot ---
print(side_by_side_plot_flipped)

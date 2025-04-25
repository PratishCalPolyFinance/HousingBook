# ===============================================
# PHV_hexagon_map.R
# Public Housing Voucher Occupancy by State — Hexagon Map
# (Modified to analyze the difference in pct_occupancy from 2014)
# ===============================================

# Clear workspace
rm(list = ls())

# --- Install & load required packages ---
required_packages <- c("tidyverse", "showtext", "geojsonio", 
                       "patchwork", "openxlsx", "readr", "here", 
                       "sf", "cowplot", "stringr")

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

# --- Define Okabe-Ito 8-color palette ---
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7", "black")

# --- Load and clean Public Housing Voucher data for 2024 ---
PHV_data_2024 <- openxlsx::read.xlsx("https://www.huduser.gov/portal/datasets/pictures/files/STATE_2024_2020census.xlsx")

PHV_data_filter_2024 <- PHV_data_2024 %>%
  filter(program == 3) %>% 
  slice(1:51) %>%
  select(name, pct_occupied) %>%
  mutate(
    ISO2 = substr(name, 1, 2),
    state_name = sub("^..\\s+", "", name)
  )

# --- Load and clean Public Housing Voucher data for 2014 ---
PHV_data_2014 <- openxlsx::read.xlsx("https://www.huduser.gov/portal/datasets/pictures/files/STATE_2014.xlsx")

PHV_data_filter_2014 <- PHV_data_2014 %>%
  filter(program == 3) %>% 
  slice(1:51) %>%
  select(name, pct_occupied) %>%
  mutate(
    ISO2 = substr(name, 1, 2),
    state_name = sub("^..\\s+", "", name)
  )

# --- Merge 2014 and 2024 data ---
df_merged <- PHV_data_filter_2024 %>%
  left_join(PHV_data_filter_2014, by = "ISO2", suffix = c("_2024", "_2014")) %>%
  mutate(
    pct_diff = pct_occupied_2024 - pct_occupied_2014
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
  left_join(df_merged, by = c("id" = "ISO2"))

# --- Create 8 bins for pct_diff values ---
df_phv_hex <- df_phv_hex %>%
  mutate(pct_diff_bin = cut(pct_diff,
                            breaks = c(-Inf, -5, -2, 0, 2, 5, Inf),
                            labels = c("-5% or less", "-5% to -2%", "-2% to 0%", 
                                       "0% to 2%", "2% to 5%", "5% or more"),
                            include.lowest = TRUE))

# --- Compute centroids for labeling ---
centroids_sf <- st_centroid(df_phv_hex)
centroids_coords <- st_coordinates(centroids_sf)
centroids <- as.data.frame(centroids_coords) %>%
  mutate(
    id = df_phv_hex$id,
    id_long = str_wrap(df_phv_hex$google_name, 12)
  )

# --- Create the Hexagon Map ---
hex_phv_diff <- ggplot(df_phv_hex) +
  geom_sf(aes(fill = pct_diff_bin), color = "white") +
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
  scale_fill_manual(values = okabe_ito,
                    name = "Pct Change in Occupied %",
                    guide = guide_legend(title.position = "top",
                                         title.hjust = 0.5,
                                         label.position = "bottom",
                                         keywidth = unit(1, "cm"),
                                         keyheight = unit(0.5, "cm"),
                                         nrow = 1))  +
  ggtitle("Change in Public Housing Voucher Occupancy \nby State from 2014 to 2024") + 
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  guides(
    fill = guide_legend(
      title = "Pct Change in Occupied %",
      title.position = "top", 
      title.hjust = 0.5,
      direction = "horizontal",
      label.position = "top", 
      keywidth = unit(1, "cm"),
      keyheight = unit(0.5, "cm"),
      nrow = 1
    )
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0)  
  )

# --- Display the map ---
print(hex_phv_diff)

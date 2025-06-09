# -----------------------------------------------
# Clear environment and load libraries
# -----------------------------------------------
rm(list = ls())

library(tidyverse)
library(openxlsx)
library(here)
library(scales)
library(ggtext)

# -----------------------------------------------
# Step 1: Generate URLs
# -----------------------------------------------
years <- 2014:2024

urls <- map_chr(
  years,
  ~ if_else(
    .x >= 2022,
    paste0("https://www.huduser.gov/portal/datasets/pictures/files/US_", .x, "_2020census.xlsx"),
    paste0("https://www.huduser.gov/portal/datasets/pictures/files/US_", .x, ".xlsx")
  )
)

# -----------------------------------------------
# Step 2: Read and Combine All Data
# -----------------------------------------------
all_data <- map2_dfr(urls, years, ~{
  message("Reading year: ", .y)
  df <- tryCatch(
    read.xlsx(.x),
    error = function(e) return(NULL)
  )
  
  if (!is.null(df)) {
    df <- df %>%
      mutate(
        year = .y,
        total_units = as.numeric(total_units),
        people_total = as.numeric(people_total),
        spending_per_month = as.numeric(spending_per_month),
        program = as.factor(program_label)
      )
  }
  df
})

# -----------------------------------------------
# Step 3: Summarize Data
# -----------------------------------------------
summary_data <- all_data %>%
  filter(
    !is.na(program),
    !is.na(spending_per_month),
    !is.na(pct_occupied),
    !is.na(total_units),
    program_label != "Summary of All HUD Programs"
  ) %>%
  mutate(
    occupied_units = total_units * pct_occupied / 100
  ) %>%
  group_by(year) %>%
  summarise(
    total_expenditure = sum(spending_per_month * occupied_units*12, na.rm = TRUE),
    total_people = sum(people_total, na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)
library(scales)

ggplot(summary_data, aes(x = year)) +
  # Bar for total expenditure (left axis)
  geom_col(aes(y = total_expenditure / 1e9), fill = "#D55E00", width = 0.6) +
  
  # Line for total people (right axis)
  geom_line(aes(y = total_people / 1e6), color = "#0072B2", size = 1.5) +
  geom_point(aes(y = total_people / 1e6), color = "#0072B2", size = 3) +
  
  # Axis scales
  scale_y_continuous(
    name = "Total Expenditure (Billions USD)",
    limits = c(0, NA),
    labels = dollar_format(suffix = " B"),
    sec.axis = sec_axis(~ ., name = "Total People Served (Millions)", labels = label_comma(suffix = " M"))
  ) +
  scale_x_continuous(breaks = summary_data$year) +
  labs(
    title = "More <span style='color:#D55E00'><b>Money</b></span>, Fewer <span style='color:#0072B2'><b>People</b></span>: HUD Voucher Trends (2014–2024)",
    subtitle = "Spending on housing vouchers has risen, even as the number of people served has steadily declined",
    x = "Year",
    caption = "Source: HUD Picture of Subsidized Households, 2014–2024"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = ggtext::element_markdown(size = 18, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 13, face = "italic", hjust = 0),
    axis.title.y = element_text(color = "#D55E00", face = "bold"),
    axis.title.y.right = element_text(color = "#0072B2", face = "bold"),
    axis.text.y = element_text(color = "#D55E00"),
    axis.text.y.right = element_text(color = "#0072B2"),
    legend.position = "none",
    panel.grid.major = element_line(linetype = "dashed", color = "grey70"),
    panel.grid.minor = element_line(linetype = "dashed", color = "grey85"),
    axis.title.x = element_blank()
  )

# Save plot as 1280px wide JPEG
ggsave(
  filename = here::here("plots", "ExpenditureThruTime.jpg"),
  width = 1280 / 96,   # Convert pixels to inches (assuming 96 dpi)
  height = 800 / 96,   # You can adjust the height for aspect ratio
  dpi = 96,
  units = "in",
  device = "jpeg"
)


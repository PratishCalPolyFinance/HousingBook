# ------------------------------------------------------------
# Title: Section 8 Reddit Analysis - Quarterly Post Volume
# Author: Pratish Patel
# Date: 2025-07-01
#
# Description:
# This script analyzes Reddit discussions related to Section 8 
# housing. It includes data wrangling, classification via GPT-4o-mini,
# and visualizations that highlight landlord perspectives on
# housing voucher programs over time and across subreddits.
# ------------------------------------------------------------

# ------------------------------------------------------------
# 0. Environment Setup
# ------------------------------------------------------------
rm(list = ls())

# Load Required Packages
required_pkgs <- c(
  "tidyverse", "tidycensus", "tigris", "sf", "readxl", "here",
  "glue", "showtext", "lubridate", "camcorder", "httr2", 
  "jsonlite", "scales", "fmsb", "tidytext"
)

invisible(lapply(required_pkgs, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}))

options(tigris_use_cache = TRUE)

# Fonts & Color Palette
font_add_google("Montserrat", "montserrat")
font_add_google("Roboto", "roboto")
showtext_auto()

okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "black"
)

# ------------------------------------------------------------
# 1. Load and Clean Reddit Data
# ------------------------------------------------------------
reddit_raw <- read.csv2(here("Input_data", "Reddit_S8_Scraper.csv"), sep = ",")

target_subreddits <- c(
  "r/realestateinvesting", "r/Landlord", "r/Section8PublicHousing",
  "r/povertyfinance", "r/bostonhousing", "r/PropertyManagement", 
  "r/TenantsInTheUK"
)

reddit_clean <- reddit_raw %>%
  filter(dataType != "community") %>%
  filter(title != "" | body != "") %>%
  mutate(
    community  = coalesce(communityName, parsedCommunityName),
    createdAt  = as.POSIXct(createdAt, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    across(c(title, body), trimws)
  ) %>%
  filter(community %in% target_subreddits) %>%
  select(title, body, username, createdAt, community, upVotes, numberOfComments, dataType)

# ------------------------------------------------------------
# 2. Aggregate Reddit Posts by Quarter
# ------------------------------------------------------------
quarterly_posts <- reddit_clean %>%
  mutate(quarter = floor_date(as.Date(createdAt), unit = "quarter")) %>%
  count(quarter, name = "num_posts")

# ------------------------------------------------------------
# 3. Shared Plot Theme
# ------------------------------------------------------------
plot_title_style    <- element_text(family = "montserrat", size = 80, face = "bold", hjust = 0)
plot_subtitle_style <- element_text(family = "montserrat", size = 0, color = "gray40", hjust = 0, margin = margin(b = 10))
plot_caption_style  <- element_text(family = "montserrat", size = 40, face = "italic", color = "gray40", hjust = 1)
axis_text_style     <- element_text(family = "montserrat", size = 40, face = "bold")
common_grid_style   <- theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
  panel.grid.minor   = element_blank()
)

# ------------------------------------------------------------
# 4. Plot: Quarterly Post Volume
# ------------------------------------------------------------
date_breaks <- seq(min(quarterly_posts$quarter), max(quarterly_posts$quarter), by = "6 months")

ggplot(quarterly_posts, aes(x = quarter, y = num_posts)) +
  geom_col(fill = okabe_ito[1], width = 65) +
  scale_x_date(breaks = date_breaks, labels = label_date(format = "%b '%y")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = label_comma()) +
  labs(
    title    = "Section 8 Reddit Opinions Over Time",
    subtitle = "Post volume aggregated by quarter",
    caption  = "Source: Reddit scraper run on Section 8-related keywords"
  ) +
  theme_light(base_family = "montserrat", base_size = 20) +
  theme(
    plot.title       = plot_title_style,
    plot.subtitle    = plot_subtitle_style,
    plot.caption     = plot_caption_style,
    axis.text        = axis_text_style,
    axis.title       = element_blank(),
    plot.background  = element_blank(),
    panel.border     = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  ) + common_grid_style

# ------------------------------------------------------------
# 5. GPT Prompt Builder and API Caller
# ------------------------------------------------------------
build_prompt <- function(body, community) {
  paste0(
    "You are an assistant analyzing Reddit posts to understand why landlords accept or reject Section 8 vouchers.\n\n",
    "POST:\n", body, "\n\n",
    "COMMUNITY: ", community, "\n\n",
    "Think step by step:\n",
    "1. Based on the language and content, is the author likely a landlord?\n",
    "2. Does the post mention Section 8 or housing vouchers?\n",
    "3. What is their stance?\n",
    "4. Identify reasons and return a JSON with these dummies:\n\n",
    "- inspection_delay\n- rent_payment_delay\n- tenant_quality\n- government_burden\n",
    "- profit_motive\n- social_good\n- unclear\n\n",
    "Respond like this:\n",
    "{ \"is_landlord\": true, \"mentions_vouchers\": true, \"stance\": \"reject\", ... }"
  )
}

call_gpt4o_mini <- function(prompt) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OPENAI_API_KEY not found.")
  
  response <- request("https://api.openai.com/v1/chat/completions") %>%
    req_headers(
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) %>%
    req_body_json(list(
      model    = "gpt-4o-mini",
      messages = list(
        list(role = "system", content = "You are a helpful assistant."),
        list(role = "user", content = prompt)
      ),
      temperature = 0.3
    )) %>%
    req_perform() %>%
    resp_body_json()
  
  response$choices[[1]]$message$content
}

# Optional: Uncomment and run to reclassify
# classified_sample <- reddit_clean %>%
#   mutate(
#     gpt_prompt   = map2_chr(body, community, build_prompt),
#     gpt_response = map_chr(gpt_prompt, call_gpt4o_mini),
#     parsed       = map(gpt_response, ~ tryCatch(fromJSON(.x), error = function(e) NULL))
#   ) %>%
#   unnest_wider(parsed)
# save(classified_sample, file = here("Classified_data.rda"))

load(here("Classified_data.rda"))

quote <- classified_sample %>%
          filter(is_landlord == TRUE,
                 inspection_delay == 1,
                 government_burden == 1)

# ------------------------------------------------------------
# 6. Heatmap of Complaints by Community
# ------------------------------------------------------------
pain_vars <- c(
  "inspection_delay", "rent_payment_delay", "tenant_quality",
  "government_burden", "profit_motive", "social_good"
)

pain_labels <- c(
  inspection_delay   = "Inspection",
  rent_payment_delay = "Late rent",
  tenant_quality     = "Screening",
  government_burden  = "Red tape",
  profit_motive      = "Income",
  social_good        = "Help"
)

pain_labels_wrapped <- setNames(str_wrap(unname(pain_labels), width = 12), names(pain_labels))

heatmap_df <- classified_sample %>%
  filter(is_landlord == TRUE, community != "") %>%
  select(community, all_of(pain_vars)) %>%
  pivot_longer(cols = all_of(pain_vars), names_to = "pain_point", values_to = "value") %>%
  filter(value %in% c(0, 1)) %>%
  group_by(community, pain_point) %>%
  summarise(share = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(pain_label = pain_labels_wrapped[pain_point]) %>%
  filter(community != "r/TenantsInTheUK")

community_order_clean <- heatmap_df %>%
  group_by(community) %>%
  summarise(total_complaints = sum(share), .groups = "drop") %>%
  arrange(desc(total_complaints)) %>%
  pull(community)

ggplot(
  heatmap_df,
  aes(
    x = factor(pain_label, levels = unique(pain_label)),
    y = factor(community, levels = community_order_clean),
    fill = share
  )
) +
  geom_tile(color = "black", linewidth = 0.5, linetype = "dashed") +
  scale_fill_gradientn(
    colours = c("white", okabe_ito[1], okabe_ito[3]),
    limits = c(0, 1),
    labels = percent_format(),
    name = "Fraction of posts"
  ) +
  labs(
    title    = "What Landlords Complain About?",
    subtitle = "Proportion of landlord-identified posts mentioning each pain point",
    caption  = "Source: Reddit + GPT classification",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "montserrat", base_size = 16) +
  theme(
    plot.title       = plot_title_style,
    plot.subtitle    = plot_subtitle_style,
    plot.caption     = plot_caption_style,
    plot.margin      = margin(t = 20, r = 20, b = 5, l = 20),
    axis.text.x      = element_text(size = 40, lineheight = 0.5, face = "bold"),
    axis.text.y      = element_text(size = 40, lineheight = 0.5, face = "bold"),
    legend.position  = "top",
    legend.direction = "horizontal",
    legend.title     = element_text(size = 40, face = "bold"),
    legend.text      = element_text(size = 40),
    legend.key.width = unit(2.5, "cm"),
    legend.key.height= unit(2, "cm"),
    legend.margin    = margin(t = 10, b = 10),
    panel.grid       = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

# ------------------------------------------------------------
# 7. Bar Chart of Complaint Frequencies
# ------------------------------------------------------------
painpoint_freq <- classified_sample %>%
  filter(is_landlord == TRUE) %>%
  select(all_of(pain_vars)) %>%
  pivot_longer(cols = everything(), names_to = "pain_point", values_to = "value") %>%
  filter(value == 1) %>%
  count(pain_point) %>%
  mutate(pain_label = pain_labels_wrapped[pain_point]) %>%
  arrange(desc(n)) %>%
  mutate(pain_label = factor(pain_label, levels = rev(pain_label)))

ggplot(painpoint_freq, aes(x = pain_label, y = n, fill = pain_label)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = n),
    hjust = 1.3,
    size = 20,
    color = "white",
    family = "montserrat",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values = okabe_ito[1:length(pain_vars)], guide = "none") +
  labs(
    title    = "Frequency of Pain Points",
    subtitle = "Counts of GPT-identified complaints across all landlord posts",
    x        = NULL,
    y        = NULL,
    caption  = "Source: Reddit + GPT-4o-mini classification"
  ) +
  theme_light(base_family = "montserrat", base_size = 20) +
  theme(
    plot.title         = plot_title_style,
    plot.subtitle      = plot_subtitle_style,
    plot.caption       = plot_caption_style,
    axis.text.y        = element_text(size = 40, face = "bold"),
    axis.text.x        = element_blank(),
    axis.ticks         = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

library(httr)
library(jsonlite)
library(dplyr)

# Replace with your Apify API token
api_token <- "apify_api_jOS2RYVgJgBHOCLf3w542e0PFIoupB1YfELq"

# Set your keywords and subreddits
search_keywords <- c("section 8", "voucher tenant", "landlord experience", "refuse section 8")
subreddits <- c("landlord", "realestateinvesting", "landlordadvice")

# Set Apify actor and task details
actor_id <- "trudax~reddit-scraper-lite"
base_url <- "https://api.apify.com/v2/actor-tasks"
task_url <- paste0("https://api.apify.com/v2/acts/", actor_id, "/runs?token=", api_token)

# Run scraper with desired input
input_payload <- list(
  searchTerms = search_keywords,
  subreddits = subreddits,
  maxItems = 100, # You can increase as needed
  searchType = "comment" # or "post" if needed
)

# Start actor run
response <- POST(task_url, body = toJSON(input_payload, auto_unbox = TRUE), encode = "json")
run_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

# Poll run status until finished
run_id <- run_data$data$id
status_url <- paste0("https://api.apify.com/v2/actor-runs/", run_id, "?token=", api_token)

repeat {
  status_resp <- fromJSON(content(GET(status_url), "text", encoding = "UTF-8"))
  run_status <- status_resp$data$status
  cat("Status:", run_status, "\n")
  if (run_status %in% c("SUCCEEDED", "FAILED", "TIMED-OUT")) break
  Sys.sleep(5)
}

# If succeeded, fetch dataset
if (run_status == "SUCCEEDED") {
  dataset_id <- status_resp$data$defaultDatasetId
  dataset_url <- paste0("https://api.apify.com/v2/datasets/", dataset_id, "/items?clean=true&token=", api_token)
  
  reddit_data <- fromJSON(dataset_url)
  
  # Simplify and view
  reddit_df <- reddit_data %>%
    select(title, url, text, author, created, subreddit, score)
  
  # Save to CSV
  write.csv(reddit_df, "reddit_landlord_comments.csv", row.names = FALSE)
  
  View(reddit_df)
  
} else {
  cat("Run did not succeed. Status:", run_status)
}

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

api_token <- "apify_api_jOS2RYVgJgBHOCLf3w542e0PFIoupB1YfELq"

actor_id <- "trudax~reddit-scraper-lite"
run_url <- paste0("https://api.apify.com/v2/acts/", actor_id, "/runs?token=", api_token)

# Define search keywords and build search URLs
search_keywords <- c("section 8", "voucher tenant", "landlord experience", "refuse section 8")
search_urls <- lapply(search_keywords, function(k) {
  list(url = paste0("https://www.reddit.com/search/?q=", URLencode(k)))
})

# Input payload (small batch for trial)
input_payload <- list(
  startUrls = search_urls,
  sort = "new",
  maxItems = 10,
  maxPostCount = 10,
  maxComments = 10,
  maxCommunitiesCount = 2,
  maxUserCount = 2,
  scrollTimeout = 40,
  searchPosts = FALSE,
  searchComments = TRUE,
  searchCommunities = FALSE,
  searchUsers = FALSE,
  skipComments = FALSE,
  skipUserPosts = TRUE,
  skipCommunity = TRUE,
  ignoreStartUrls = FALSE,
  includeNSFW = FALSE,
  proxy = list(
    useApifyProxy = TRUE,
    apifyProxyGroups = list("RESIDENTIAL")
  ),
  debugMode = TRUE
)


cat("Starting scraper run...\n")
response <- POST(run_url, body = toJSON(input_payload, auto_unbox = TRUE), encode = "json")
run_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

# Extract run ID and check status
run_id <- run_data$data$id
status_url <- paste0("https://api.apify.com/v2/actor-runs/", run_id, "?token=", api_token)


cat("Scraper started. Run ID:", run_id, "\n")

output_file <- "reddit_landlord_trial.csv"
already_saved_ids <- character()

# Start polling
cat("Polling every 5 seconds. This may take a few minutes...\n")

for (i in 1:60) {
  cat("Polling cycle:", i, "\n")
  try({
    status_resp <- fromJSON(content(GET(status_url), "text", encoding = "UTF-8"))
    run_status <- status_resp$data$status
    cat("Current run status:", run_status, "\n")
    
    dataset_id <- status_resp$data$defaultDatasetId
    
    if (!is.null(dataset_id)) {
      dataset_url <- paste0("https://api.apify.com/v2/datasets/", dataset_id, "/items?clean=true&token=", api_token)
      reddit_data <- fromJSON(dataset_url)
      
      if (!is.null(reddit_data) && nrow(reddit_data) > 0) {
        cat("Total items fetched so far:", nrow(reddit_data), "\n")
        
        new_data <- reddit_data %>%
          filter(!url %in% already_saved_ids)
        
        if (nrow(new_data) > 0) {
          write_csv(new_data, output_file, append = file.exists(output_file))
          already_saved_ids <- c(already_saved_ids, new_data$url)
          cat("Saved", nrow(new_data), "new rows at", Sys.time(), "\n")
        } else {
          cat("No new data to save this cycle.\n")
        }
      } else {
        cat("No data yet in dataset.\n")
      }
    } else {
      cat("Dataset ID not yet available...\n")
    }
    
    if (run_status %in% c("SUCCEEDED", "FAILED", "TIMED-OUT")) {
      cat("Run finished with status:", run_status, "\n")
      break
    }
    
  }, silent = TRUE)
  
  cat("Waiting 10 seconds for actor to initialize...\n")
  Sys.sleep(10)
  
}

cat("Polling loop complete.\n")

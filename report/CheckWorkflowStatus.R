library(httr)
library(jsonlite)
library(lubridate)

# ---- Inputs ----
github_token <- Sys.getenv("GITHUB_PAT")
repo <- "SwampThingPaul/EvergladesConditions"
workflow_name <- "Render & Deploy Report Site"

# ---- API Headers ----
headers <- add_headers(Authorization = paste("token", github_token))

# ---- Get Workflows ----
workflows_url <- paste0("https://api.github.com/repos/", repo, "/actions/workflows")
resp <- GET(workflows_url, headers)
workflows <- fromJSON(content(resp, "text"))

workflow_id <- workflows$workflows$id[workflows$workflows$name == workflow_name]

if (length(workflow_id) == 0) {
  stop("Workflow not found.")
}

# ---- Get Runs ----
runs_url <- paste0("https://api.github.com/repos/", repo, "/actions/workflows/", workflow_id, "/runs")
runs_resp <- GET(runs_url, headers)
runs <- fromJSON(content(runs_resp, "text"))$workflow_runs

# ---- Filter Today's Runs ----
today <- as.Date(Sys.time(), tz = "UTC")
runs$created_at <- ymd_hms(runs$created_at, tz = "UTC")
today_runs <- runs[as.Date(runs$created_at) == today, ]

# ---- Expected Times ----
expected_times <- c(
  today + hours(12) + minutes(55),
  today + hours(13) + minutes(55)
)

cat("Expected runs today:", length(expected_times), "\n")
cat("Actual runs today:", nrow(today_runs), "\n")

if (nrow(today_runs) < length(expected_times)) {
  cat("Some scheduled runs may have been missed!\n")
} else {
  cat("All scheduled runs occurred.\n")
  
  
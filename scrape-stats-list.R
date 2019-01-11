# Scrape GOV.UK's statistics publications from the Content API via the Search
# API

# Content API: https://content-api.publishing.service.gov.uk/#gov-uk-content-api
# Search API: https://docs.publishing.service.gov.uk/apis/search/search-api.html

# content-subtypes in this spreadsheet https://docs.google.com/spreadsheets/d/1GGf5hQLBTfrRksvrhpLGKhwbdZwrVp8N2ogLrj1GbWU/edit#gid=1293825819
# linked to from this blog post https://dataingovernment.blog.gov.uk/2016/05/26/use-the-search-api-to-get-useful-information-about-gov-uk-content/

library(tidyverse)
library(ratelimitr)
library(polite)
library(httr)
library(jsonlite)
library(lubridate)
library(here)

my_user_agent <- "duncan.garmonsway@digital.cabinet-office.gov.uk"

content_url <- "https://www.gov.uk/api/content"

search_count_url <- "https://www.gov.uk/api/search.json?q=&filter_any_detailed_format=statistics&filter_any_detailed_format=statistics-national-statistics&filter_any_detailed_format=statistical-data-set&count=0&aggregate_detailed_format=0,scope:all_filters"

search_base_url <- "q=&filter_any_detailed_format=statistics&filter_any_detailed_format=statistics-national-statistics&filter_any_detailed_format=statistical-data-set&fields=title&fields=link"

page_size <- 1500 # The maximum is 1500

delay <- 0.1 # Delay between requests, per the documented api rate limit

# Total number of results to page through.
results_count <- read_json(search_count_url)$total
pages_count <- results_count %/% page_size + (results_count %% page_size != 0L)

# Session to use for scraping the search API
session_search <-
  bow("https://www.gov.uk/api/search.json",
      user_agent = my_user_agent,
      # needs robotstxt version >=0.7.1
      # https://github.com/ropensci/robotstxt/issues/48
      # until https://github.com/alphagov/govuk-puppet/issues/8518 is fixed.
      delay = 0.1, # will warn too fast but is allowed by GOV.UK API docs
      force = TRUE)

# Store the search results here
results <- vector(length = pages_count, mode = "list")

# Perform the search
for (page_number in seq_len(pages_count)) {
  cat(page_number, "\n")
  start_number = page_size * (page_number - 1) + 1
  url <- paste0(search_base_url, "&count=", page_size, "&start=", start_number)
  results[[page_number]] <- scrape(session_search, url)
}

# Extract the URLs of the reports in the search results
report_urls <-
  results %>%
  map(pluck, "results") %>%
  purrr::flatten() %>%
  map_chr(pluck, "link")

# Scrape the content API for each report

# Don't use {polite} for the content API because it doesn't support different
# URLs, only different parameters to one URL.
httr_get_limited <-
  ratelimitr::limit_rate(httr::GET,
                         ratelimitr::rate(n = 1, period = delay))

possibly_fromJSON <- possibly(fromJSON, list())

reports_count <- length(report_urls)

reports_json <- vector(length = reports_count, mode = "list")

for (report_number in seq_len(reports_count)) {
  cat(report_number, "/", reports_count, "\n")
  text <-
    httr_get_limited(paste0(content_url, report_urls[report_number])) %>%
    content("text")
  reports_json[[report_number]] <- possibly_fromJSON(text)
}

saveRDS(reports_json, here("reports-json.Rds"))
reports_json <- readRDS(here("reports-json.Rds"))

reports <-
  tibble(base_path = report_urls,
         reports = reports_json) %>%
  dplyr::filter(as.logical(map_int(reports, length))) # Some reports didn't download

saveRDS(reports, here("reports.Rds"))
reports <- readRDS(here("reports.Rds"))


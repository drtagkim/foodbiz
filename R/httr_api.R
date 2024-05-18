# Library to get data from API
library(httr)
library(jsonlite)
library(dplyr)
library(rlist)

# A function to get data in JSON format from API
# given URL
get_data <- function(base_url,service_key,query_parameter) {
  b = parse_url(base_url)
  b$query = list(
    serviceKey=service_key
  )
  suppressWarnings({
    b$query = c(b$query,query_parameter)
  })
  url = build_url(b)
  response <- GET(url)
  stop_for_status(response)
  content(response, "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE)
}



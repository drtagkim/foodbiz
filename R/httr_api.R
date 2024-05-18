# Library to get data from API
library(httr)
library(jsonlite)
library(dplyr)
library(rlist)

# A function to get data in JSON format from API
# given URL
gg_data <- function(base_url,service_key,query_parameter) {
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

# Food
food_data <- function(service_code,service_key,start=1,end=5,bar_cd=NULL) {
  url = paste0(
    'http://openapi.foodsafetykorea.go.kr/api/',
    service_key,
    '/',
    service_code,
    '/json/',
    start,'/',end
  )
  if(!is.null(bar_cd)) {
    url = paste0(url,'/BAR_CD=',bar_cd)
  }
  response = GET(url)
  stop_for_status(response)
  content(response, "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE)
}

food_data('I2510',service_key=service_keys$food$tkkim,11,20)

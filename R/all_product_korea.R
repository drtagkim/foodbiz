library(rvest)
library(stringr)
library(dplyr)
library(jsonlite)

# mainKeyword 값을 받아서 페이지 코드를 가져오는 함수
get_page_code <- function(mainKeyword) {
  # base URL 정의
  base_url <- "https://www.allproductkorea.or.kr/products/info"
  
  # mainKeyword를 사용해 URL 생성
  url <- sprintf("%s?q=%%s&page=1&size=10", base_url)
  
  # mainKeyword를 JSON 형태로 인코딩된 상태로 포함
  query <- sprintf("{\"mainKeyword\":\"%s\",\"subKeyword\":\"\"}", mainKeyword)
  
  # URL 완성
  full_url <- sprintf(url, URLencode(query, reserved = TRUE))
  
  # 페이지 요청 및 HTML 코드 읽기
  page <- read_html(full_url)
  
  # 페이지 코드 반환
  return(page)
}

# 페이지 코드에서 상품의 데이터베이스 UID를 추출하는 함수
get_data_prd_no <- function(page_code) {
  # data-biz-no 값을 추출
  biz_no_values <- page_code %>% html_nodes("li[data-type='korcham']") %>% html_attr("data-prd-no")
  
  # 추출한 data-biz-no 값 반환
  biz_no_values
}

#상품 페이지를 받는 함수
get_prd_page <- function(prd_no) {
  # base URL 정의
  base_url <- "https://www.allproductkorea.or.kr/products/info/korcham"
  
  # prd_no를 사용해 URL 생성
  full_url <- sprintf("%s/%s", base_url, prd_no)
  
  # 페이지 요청 및 HTML 코드 읽기
  page <- read_html(full_url)
  
  # 페이지 코드 반환
  return(page)
}

#data-biz-no추출, button 엘리멘트에서, prd_page 객체 받아서
get_data_biz_no <- function(prd_page) {
  # data-biz-no 값을 추출
  biz_no_values <- prd_page %>% html_nodes("button") %>% html_attr("data-biz-no")
  
  # 추출한 data-biz-no 리스트에서 NA가 아닌 값만 추출것,
  #값이 복수로 있으면 1개만
  biz_no_values <- biz_no_values[!is.na(biz_no_values)]
  biz_no_values <- biz_no_values[1]
  biz_no_values
}

#https://www.allproductkorea.or.kr/platform/nicednb/companies/biz-no/3128149680/credit-information
#get_data_biz_no에서 추출된 string값을 치환, /credit-information 앞에 들어가게. URL
#rvest로 page 추출
get_credit_page <- function(biz_no) {
  # base URL 정의
  base_url <- "https://www.allproductkorea.or.kr/platform/nicednb/companies/biz-no"
  
  # biz_no를 사용해 URL 생성
  full_url <- sprintf("%s/%s/credit-information", base_url, biz_no)
  
  # 페이지 요청 및 HTML 코드 읽기
  page <- read_html(full_url)
  
  # 페이지 코드 반환
  return(page)
}
#credit page 분석
#JSON데이터로 되어 있음. JSON 데이터 부분만 추려서 list 데이터 형태로 변환
get_credit_info <- function(credit_page) {
  # JSON 데이터 추출
  json_data <- credit_page %>% as.character()
  json_text <- sub(".*\\{(.+?)\\}.*", "{\\1}", json_data)
  # JSON 데이터를 list로 변환
  credit_info <- fromJSON(json_text) |>
    lapply(function(x) if (is.null(x)) "" else x) |>
    as_tibble()
  # list 반환
  return(credit_info)
}

get_product_info <- function(prd_page) {
  # 바코드
  barcode <- prd_page %>%
    html_nodes(".gtin") %>%
    html_text(trim = TRUE) %>%
    .[.!='바코드']
  
  # 상품분류명
  category <- prd_page %>%
    html_node(".clsTotalNm") %>%
    html_text(trim = TRUE)
  
  # 상품명 (국문)
  product_name <- prd_page %>%
    html_node(".prdNmKor") %>%
    html_text(trim = TRUE)
  
  # 회사정보
  company_info <- prd_page %>%
    html_node(".companies .company-info") %>%
    html_text(trim = TRUE)
  
  # 브랜드
  brand <- prd_page %>%
    html_node(".brandNames") %>%
    html_text(trim = TRUE)
  
  # 국가정보
  country_info <- prd_page %>%
    html_node(".countries") %>%
    html_text(trim = TRUE)
  
  # 구성정보
  product_comp <- prd_page %>%
    html_node(".prdComp") %>%
    html_text(trim = TRUE)
  
  # 순중량 (NetWeight)
  net_weight <- prd_page %>%
    html_node(".originVolume") %>%
    html_text(trim = TRUE)
  
  # 상품형태
  product_form <- prd_page %>%
    html_node(".prdPacTyp") %>%
    html_text(trim = TRUE)
  
  # 품목보고번호
  report_number <- prd_page %>%
    html_node(".attribute") %>%
    html_text(trim = TRUE)
  
  # 결과를 데이터프레임으로 반환
  product_info <- tibble(
    Barcode = barcode,
    Category = category,
    Product_Name_Korean = product_name,
    Company_Info = company_info,
    Brand = brand,
    Country_Info = country_info,
    Composition_Info = product_comp,
    Net_Weight = net_weight,
    Product_Form = product_form,
    Report_Number = report_number,
    stringsAsFactors = FALSE
  )
  
  return(product_info)
}

page_code <- get_page_code(mainKeyword)
prd_page <- page_code |> get_data_prd_no() |>
  get_prd_page()
biz_no <- prd_page |> get_data_biz_no()
credit_page <- get_credit_page(biz_no)

analyze_product_by_barcode <- function(barcode) {
  page_code = get_page_code(mainKeyword)
  prd_page = page_code |> 
    get_data_prd_no() |>
    get_prd_page()
  biz_no = prd_page |> 
    get_data_biz_no()
  credit_page = get_credit_page(biz_no)
  part1 = prd_page |> get_product_info()
  part2 = get_credit_info(credit_page)
  bind_cols(part1,part2)
}

mainKeyword <- test_bar_code <- "8800253620008" #입력값. 텍스트 상자로 입력할 것.

test_result <- analyze_product_by_barcode(test_bar_code)
View(test_result)

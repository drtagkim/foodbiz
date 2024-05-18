#

library(dplyr)
library(rvest)
library(httr)
library(purrr)
library(stringr)

get_gs1_code <- function(keyword,page=1,size=10) {
  part1="https://www.allproductkorea.or.kr/products/search?q=%7B%22mainKeyword%22:%22"
  part2=URLencode(keyword)
  part3="%22,%22subKeyword%22:%22%22%7D&"
  part4="page="
  part4a=page
  part5="&size="
  part5a=size
  url=paste0(part1,part2,part3,part4,part4a,part5,part5a)
  url
}

get_gs1_search <- function(gs1_code_url) {
  page = read_html(gs1_code_url)
  total = page %>% html_element('body > div > div > div.lct_wrap > div.spl_wrap > div.pl_total > span')
  total = total %>% html_text() %>%
    str_extract('[0-9,]+') %>% as.numeric()
  items = page %>% html_elements("body > div > div > div.lct_wrap > div.spl_list > ul > li")
  items %>% map(function(x){
    v1 = x %>% html_text() %>% strsplit('\t')
    v1=v1[[1]]
    barcode=v1[5] %>% trimws()
    product=v1[6] %>% trimws()
    category=v1[8] %>% trimws()
    tibble(total,barcode,product,category)
  }) %>% bind_rows()
}



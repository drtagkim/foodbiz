library(shiny)
library(dplyr)
library(rvest)
library(jsonlite)
library(stringr)
library(tibble)
library(writexl)  # 엑셀 파일 저장을 위한 패키지

# 기존 함수들 정의 (함수 정의는 생략, 이전 코드 그대로 사용)

# Shiny UI 정의
ui <- fluidPage(
  titlePanel("Product Information Lookup by Barcode"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("barcode_input", "Enter Barcodes (Separate with Enter)", "", rows = 10),
      actionButton("submit", "Get Product Info"),
      actionButton("input_reset", "Reset Input"),
      actionButton("output_reset", "Reset Output"),
      downloadButton("download_data", "Download Excel File")
    ),
    mainPanel(
      tableOutput("result_table")
    )
  )
)

# Shiny 서버 정의
server <- function(input, output, session) {
  
  # 결과 저장을 위한 리액티브 변수
  result_data <- reactiveVal(data.frame())
  
  observeEvent(input$submit, {
    # 사용자가 입력한 바코드를 추출하고 엔터로 구분하여 리스트로 변환
    barcodes <- str_split(input$barcode_input, "\n")[[1]] %>% 
      str_trim() %>% 
      .[. != ""]
    
    # 각 바코드에 대해 정보를 수집하며 진행률 표시
    results <- withProgress(message = "Processing Barcodes", value = 0, {
      n <- length(barcodes)
      lapply(seq_along(barcodes), function(i) {
        incProgress(1 / n, detail = paste("Processing barcode", barcodes[i]))
        analyze_product_by_barcode(barcodes[i])
      })
    })
    
    # 결과들을 하나의 데이터프레임으로 병합
    result_table <- bind_rows(results)
    result_data(result_table)  # 결과 저장
    
    # 결과를 출력
    output$result_table <- renderTable({
      result_table
    })
  })
  
  # 다운로드 버튼 클릭 시 엑셀 파일로 저장
  output$download_data <- downloadHandler(
    filename = function() {
      "product_information.xlsx"
    },
    content = function(file) {
      write_xlsx(result_data(), path = file)
    }
  )
  
  # input_reset 버튼 클릭 시 입력 초기화
  observeEvent(input$input_reset, {
    updateTextAreaInput(session, "barcode_input", value = "")
  })
  
  # output_reset 버튼 클릭 시 출력 초기화
  observeEvent(input$output_reset, {
    result_data(data.frame())  # 빈 데이터프레임으로 초기화
    output$result_table <- renderTable({ NULL })  # 출력 초기화
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)

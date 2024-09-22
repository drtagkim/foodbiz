# Description: This is a Shiny app that allows users to search for product names using the GS1 Korea website.
# Author: Taekyung Kim, PhD.
# 2024-09-22 Updated
# Coworking with Food Biz Lab, Seoul National University 
#
library(shiny)
library(dplyr)
library(rvest)
library(httr)
library(purrr)
library(stringr)
library(tibble)
library(writexl)  # For saving data to Excel

# UI Definition
ui <- fluidPage(
  titlePanel("Product Name Data Search"),  # Title changed
  
  sidebarLayout(
    sidebarPanel(
      # Input for keyword and search button
      textInput("keyword", "Enter Keyword:", value = "Example"),
      numericInput("size", "Number of Results:", value = 10, min = 1),
      
      # Search button
      actionButton("search", "Search"),
      
      # Page navigation buttons
      actionButton("prev_page", "Previous Page"),
      actionButton("next_page", "Next Page"),
      
      # Reset and Download buttons
      actionButton("reset", "Reset"),
      downloadButton("downloadData", "Download Data"),
      
      # Display current page number
      textOutput("current_page")
    ),
    
    mainPanel(
      tableOutput("results"),
      
      # Horizontal line to separate content
      tags$hr(),
      
      # Footer with author information (blue color, centered, font size 8pt)
      tags$div(
        style = "color:blue; text-align:center; font-size:8pt; margin-top:20px;",
        "Digital Wellness Lab, Kyung Hee University",
        tags$br(),
        "Taekyung Kim, Associate Professor, PhD.",
        tags$br(),
        "2024."
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Store page number, keyword, and accumulated data in reactiveValues
  search_state <- reactiveValues(page = 1, keyword = NULL, data = tibble())
  
  # Search function definition
  search_results <- function() {
    req(search_state$keyword)  # Ensure that keyword is provided
    
    url <- get_gs1_code(search_state$keyword, search_state$page, input$size)
    result <- tryCatch({
      get_gs1_search(url)
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(result)) {
      search_state$data <- bind_rows(search_state$data, result)  # Append new results to existing data
    }
    
    output$results <- renderTable({
      if (is.null(result)) {
        return("No results found or an error occurred.")
      } else {
        result
      }
    })
  }
  
  # Function to generate the GS1 URL
  get_gs1_code <- function(keyword, page = 1, size = 10) {
    part1 = "https://www.allproductkorea.or.kr/products/search?q=%7B%22mainKeyword%22:%22"
    part2 = URLencode(keyword)
    part3 = "%22,%22subKeyword%22:%22%22%7D&"
    part4 = "page="
    part4a = page
    part5 = "&size="
    part5a = size
    url = paste0(part1, part2, part3, part4, part4a, part5, part5a)
    url
  }
  
  # Function to parse GS1 search results
  get_gs1_search <- function(gs1_code_url) {
    page = read_html(gs1_code_url)
    total = page %>% html_element('body > div > div > div.lct_wrap > div.spl_wrap > div.pl_total > span')
    total = total %>% html_text() %>%
      str_extract('[0-9,]+') %>% as.numeric()
    items = page %>% html_elements("body > div > div > div.lct_wrap > div.spl_list > ul > li")
    items %>% map(function(x){
      v1 = x %>% html_text() %>% strsplit('\t')
      v1 = v1[[1]]
      barcode = v1[5] %>% trimws()
      product = v1[6] %>% trimws()
      category = v1[8] %>% trimws()
      tibble(total, barcode, product, category)
    }) %>% bind_rows()
  }
  
  # Handling next/previous page buttons
  observeEvent(input$next_page, {
    search_state$page <- search_state$page + 1
    search_results()  # Search for the next page
  })
  
  observeEvent(input$prev_page, {
    if (search_state$page > 1) {
      search_state$page <- search_state$page - 1
      search_results()  # Search for the previous page
    }
  })
  
  # Display current page number
  output$current_page <- renderText({
    paste("Current Page:", search_state$page)
  })
  
  # Detect 'Enter' key press and initiate search
  observeEvent(input$keyword, {
    # Start search when user presses 'Enter' in the keyword input
    observe({
      if (input$keyword != "" && !is.null(input$keyword)) {
        search_state$keyword <- input$keyword
      }
    })
  })
  
  # Search button click event
  observeEvent(input$search, {
    search_state$page <- 1  # Reset to page 1 on new search
    search_state$keyword <- input$keyword  # Update keyword
    search_results()  # Execute search
  })
  
  # Reset button click event
  observeEvent(input$reset, {
    search_state$page <- 1  # Reset page number
    search_state$data <- tibble()  # Clear accumulated data
    updateTextInput(session, "keyword", value = "")  # Clear keyword input field
    output$results <- renderTable(NULL)  # Clear displayed results
  })
  
  # Excel file download functionality
  output$downloadData <- downloadHandler(
    filename = function() {
      "data_export.xlsx"  # Default filename
    },
    content = function(file) {
      writexl::write_xlsx(search_state$data, path = file)
    }
  )
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)

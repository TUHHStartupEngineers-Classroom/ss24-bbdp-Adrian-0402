# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)
library(quantmod)

library(rvest)
library(glue)

source(file = "00_scripts/stock_analysis_functions.R")

stock_list_tbl <- get_stock_list("SP500")
stock_data_test <- get_stock_data("AAPL")

# UI ----

ui <- fluidPage(
  title = "Stock Analyzer",
  
  # 1.0 HEADER ----
  div(
    h1("Stock Analyzer"),
    p("This is my second shiny project")
  ),
  
  # 2.0 APPLICATION UI -----
  div(
    column(
      width = 4,
      wellPanel(
        
        # Add content here 
        pickerInput(inputId = "stock_selection", 
                    choices = stock_list_tbl$label,
                    multiple = FALSE,
                    options = list(actionsBox = FALSE,
                                   liveSearch = TRUE,
                                   size = 10)),
        
        actionButton(inputId = "analyze",
                     label = "Analyze",
                     icon = icon("download"))
        
      )
    ),
    column(
      width = 8,
      div(
        h4("Plot")
      ),
      div(
        #get_stock_data(),
        plot_stock_data(stock_data_test)
      )
    )
  ),
  
  div(
    column(
      width = 12,
      div(
        h4("Analyst Commentary", style = "font-weight: bold;"),
        p(generate_commentary(stock_data_test, user_input = "Placeholder"))
      )
    )
  )
  
)

    # 3.0 ANALYST COMMENTARY ----

# SERVER ----

server <- function(input, output, session) {
  
}

# RUN APP ----

shinyApp(ui = ui, server = server)
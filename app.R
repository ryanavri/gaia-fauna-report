#library----
library(shiny)
library(shinydashboard)
library(data.validator)
library(assertr)
library(dplyr)
library(DT)
library(rmarkdown)
library(markdown)
library(bslib)
library(kableExtra)
library(tidyverse)
library(shinybusy)


# Function for data validator
between <- function(a, b) {
  function(x) {
    ifelse(!is.na(x), a <= x & x <= b, FALSE)
  }
}

# UI----
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Gaia-Fauna", titleWidth = 600
  ),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     HTML(paste0(
                       "<br>",
                       "<a href='https://gaia.id/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='GAIA.png' width = '186'></a>",
                       "<br>"
                     )), # Add logo and link here
                     
                     menuItem("Home", tabName = "home", icon = icon("home")),
                     
                     menuItem("Data Validator", tabName = "data_check", icon = icon("check")),
                     
                     menuItem("Report Generator", tabName = "report_generator", icon = icon("file"))
                   )
                   
  ),
  
  
  ## Dashboard Body ----    
  dashboardBody(
    tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }'))),
    tabItems(
      tabItem(tabName = "home",
              includeMarkdown("rmd/home.Rmd")),
      
      tabItem(tabName = "data_check",
              fluidPage(
                titlePanel("Data Validator"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput(inputId = "dataFile", label = "Choose CSV File", multiple = FALSE, accept = c(".csv")),
                    checkboxInput(inputId = "header", label = "Data has a Header row", value = TRUE),
                    selectInput(inputId = "validationType", label = "Validation Type",
                                choices = c("Avifauna", "Herpetofauna", "Mamalia"))
                  ),
                  mainPanel(
                    tableOutput(outputId = "datasetHead"),
                    uiOutput(outputId = "validation")
                  )
                )
              )
      ),
      
      tabItem(tabName = "report_generator",
              fluidPage(
                titlePanel("Report Generator"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput(
                      inputId = "file1", label = "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                    )
                  ),
                  mainPanel(
                    htmlOutput("contents"),
                    downloadButton("downloadReport")
                  )
                )
              )
      )
    )
  )
)


# Server logic ----
server <- function(input, output, session) {
  
  # Data Validator Function----
  
  readData <- reactive({
    req(input$dataFile)
    
    tryCatch(
      {
        df <- read.csv(file = input$dataFile$datapath, header = input$header)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })
  
  # Function to perform data validation based on the selected validation type
  validateData <- reactive({
    report <- data_validation_report() 
    
    if (input$validationType == "Avifauna") {
      data.validator::validate(readData(), description = "Vegetation Dataset Validation Test") %>%
        validate_cols(in_set(c("Setosa", "Virginica", "Versicolor")), variety, description = "Correct species category") %>%
        validate_cols(predicate = not_na, sepal.length:variety, description = "No missing values") %>%
        validate_cols(predicate = between(0, 10), sepal.length, description = "Column sepal.length between 0 and 10") %>%
        validate_cols(predicate = between(0, 10), sepal.length, description = "Column sepal.width between 0 and 10") %>%
        validate_cols(predicate = between(0, 10), petal.length, description = "Column petal.length between 0 and 10") %>%
        validate_cols(predicate = between(0, 10), petal.width, description = "Column petal.width between 0 and 10") %>%
        add_results(report)
    } else if (input$validationType == "Herpetofauna") {
      data.validator::validate(readData(), description = "Vegetation Dataset Validation Test") %>%
        validate_cols(in_set(c("Setosa", "Virginica", "Versicolor")), variety, description = "Correct species category") %>%
        validate_cols(predicate = not_na, sepal.length:variety, description = "No missing values") %>%
        validate_cols(predicate = between(0, 10), sepal.length, description = "Column sepal.length between 0 and 10") %>%
        validate_cols(predicate = between(0, 10), sepal.length, description = "Column sepal.width between 0 and 10") %>%
        validate_cols(predicate = between(0, 10), petal.length, description = "Column petal.length between 0 and 10") %>%
        validate_cols(predicate = between(0, 10), petal.width, description = "Column petal.width between 0 and 10") %>%
        add_results(report)
    } else if (input$validationType == "Mamalia") {
      data.validator::validate(readData(), description = "Mammal Dataset Validation Test") %>%
        validate_cols(in_set(c("Setosa", "Virginica", "Versicolor")), variety, description = "Correct species category") %>%
        validate_cols(predicate = not_na, sepal.length:variety, description = "No missing values") %>%
        validate_cols(predicate = between(0, 10), sepal.length, description = "Column sepal.length between 0 and 10") %>%
        validate_cols(predicate = between(0, 10), sepal.length, description = "Column sepal.width between 0 and 10") %>%
        validate_cols(predicate = between(0, 10), petal.length, description = "Column petal.length between 0 and 10") %>%
        validate_cols(predicate = between(0, 10), petal.width, description = "Column petal.width between 0 and 10") %>%
        add_results(report)
    } 
    report
  })
  
  # Render the table with the first 5 rows
  output$datasetHead <- renderTable({
    return(head(readData(), 5))
  })
  
  # Render the data validation report
  output$validation <- renderUI({
    render_semantic_report_ui(get_results(report = validateData()))
  })
  
  dataset <- reactive({
    req(input$file1)
    read.csv(file = input$file1$datapath,
             na.strings = ".", 
             sep = ",",
             header = TRUE,
             nrows=10)               
  })
  
  output$contents <- renderUI({
    req(dataset())
    
    data_table <- dataset() %>%
      head() %>%
      kable(format = "html", escape = FALSE) %>%
      kable_styling(
        full_width = FALSE,
        font_size = 15,
        bootstrap_options = c("striped", "hover", "condensed")
      )
    
    HTML(data_table)
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", ".docx", sep = "")
    },
    
    content = function(file) {
      
      show_modal_spinner(
        spin = "orbit",
        color = "#112446",
        text = "Please wait...")
      
      src <- normalizePath("biodive_basic_v2.0.Rmd")
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "biodive_basic_v2.0.Rmd", overwrite = TRUE)
      
      out <- render("biodive_basic_v2.0.Rmd", word_document(), params = list(data = input$file1$datapath))
      file.rename(out, file)
      
      remove_modal_spinner()
    }
  )
  
}

shinyApp(ui = ui, server = server)

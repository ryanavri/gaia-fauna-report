#library----
library(shiny)
library(shinydashboard)
library(data.validator)
library(assertr)
library(dplyr)
library(DT)
library(rmarkdown)
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
                    uiOutput(outputId = "validation"),
                    tableOutput(outputId = "datasetHead")
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
      data.validator::validate(readData(), description = "Avifauna Dataset Validation Test") %>%
        validate_cols(predicate = not_na, Landscape:Activity, description = "No missing values") %>%
        validate_cols(in_set(c("Species", "Genus", "Family", "Ordo")), Taxon.Rank, description = "Correct Taxon Rank category") %>%
        validate_if(description = "positive value", Indv > 0) %>%
        validate_if(description = "positive value", Distance > 0) %>%
        add_results(report)
      
    } else if (input$validationType == "Herpetofauna") {
      data.validator::validate(readData(), description = "herpetofauna Dataset Validation Test") %>%
        validate_cols(predicate = not_na, Landscape:Taxon.Rank, description = "No missing values") %>%
        validate_cols(in_set(c("Species", "Genus", "Family", "Ordo")), Taxon.Rank, description = "Correct Taxon Rank category") %>%
        validate_cols(in_set(c("Amphibia", "Reptilia")), Group1, description = "Correct Group1 category") %>%
        validate_cols(in_set(c("Frogs", "Snakes", "Agamids", "Lizards", "Varanids", "Turtles", "Crocodiles", "Geckoes")), Group2, description = "Correct Group2 category") %>%
        add_results(report)
      
    } else if (input$validationType == "Mamalia") {
      data.validator::validate(readData(), description = "Mammal Dataset Validation Test") %>%
        validate_cols(predicate = not_na, Landscape:Site, description = "No missing values") %>%
        validate_cols(predicate = not_na, Scientific.Name:Observation, description = "No missing values") %>%
        validate_cols(predicate = between(95, 142), Longitude, description = "Indonesia Longitude Range") %>%
        validate_cols(predicate = between(-11, 7), Latitude, description = "Indonesia Latitude Range") %>%
        validate_cols(in_set(c("Species", "Genus", "Family", "Ordo")), Taxon.Rank, description = "Correct Taxon Rank category") %>%
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
        text = "Please hold on, don't fret, this app is doing its thing. Go grab a coffee and savor life's caffeinated wonders!")
      
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
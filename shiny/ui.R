# Preps ----

## Library ----
library(shiny)
library(DT)
library(bslib)
library(shinydashboard)
library(flextable)
library(U.Taxonstand)
library(SpadeR)
library(iNEXT)
library(shinybusy)
library(vegan)
library(treemap)



# UI----
## Sidebar ----
sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem(tabName = "ovw", text = "Overview", icon = icon("map")),
    menuItem(tabName = "dat_proc", text = "Data Processing", icon = icon("map")),
    menuItem(tabName = "res_sum", text = "Result", icon = icon("map")),
    menuItem(tabName = "stat_sum", text = "Conservation Status", icon = icon("map"))
  ),
  tags$footer(
    p("Maintained by ", a(href = 'https://www.linkedin.com/in/ryan-avriandy-951b97156/', "Ryan Avriandy")),
    align = "left",
    style = "position:absolute; bottom:0; width:100%; height:50px; color: white; padding: 10px; background-color: black; z-index: 1000;"
  )
)

## Body ----
body <- dashboardBody(
  
tabItems(

### Overview ----
tabItem(
  tabName = "ovw",
  fluidRow(
    box(width = 12,
        h1("Data Analysis for transect based survey"),
        tags$p("Lorem Ipsum"),
        tags$p("Lorem Ipsum")
    )
  ), 

  fluidRow(
    box(width = 12,
        h1("Input data and design"),
        tags$p("Lorem Ipsum"),
        tags$p("Lorem Ipsum")
      )
    ), 
    
  fluidRow(
    box(width = 12,
        h1("Acknowledgment and collaboration"),
        tags$p("Lorem Ipsum"),
        tags$p("Lorem Ipsum")
    )
  )),

### Data Processing ----
tabItem(
  tabName = "dat_proc",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      selectInput(inputId = "validationType", label = "Dataset",
                  choices = c("Avifauna", "Herpetofauna", "Mammals")),
      uiOutput("columnSelectUI"),
      tags$hr(),
      actionButton(inputId = "faunastart", label = "1. Validate"),
      tags$br(),
      tags$br(),
      actionButton(inputId = "faunacalculate", label = "2. Calculate"),
      tags$br(),
      tags$br(),
      #downloadButton("downloadReport", "Download Report"),
      #tags$br(),
      #tags$br(),
      actionButton(inputId = "searchcs", label = "3. Conservation Status"),
      #tags$br(),
      #tags$br(),
      #downloadButton("downloadReportCS", "Download Conservation status")
    ),
    mainPanel(
      width = 10,
      box(uiOutput(outputId = "uspecies", width = "100%")),
      box(title = "Snapshot of your dataset",
        uiOutput(width = "100%", outputId = "datasetHead")),
      box(title = "Data Validation Report",
        verbatimTextOutput("validationReport")))
    )),

### Result ----
tabItem(
  tabName = "res_sum",
  
  fluidRow(
    box(title = "Species Richness Across Transect",
        collapsible = TRUE,
        DTOutput(outputId = "spindex")),

    box(title = "Species Richness and Abundance",
        collapsible = TRUE,
        plotOutput(outputId = "rs_abd_plot"))),
  
  fluidRow(
    box(title = "Species Richness Estimation",
        collapsible = TRUE,
        DTOutput(outputId = "est_table")),
    
    box(title = "Species Accumulation Curve",
        collapsible = TRUE,
        plotOutput(outputId = "inext"))),
  
  fluidRow(
    box(title = "Cluster Plot-Bray",
        collapsible = TRUE,
        plotOutput(outputId = "cluster_plot_b")),
    
    box(title = "Dissimilarities Between Transect",
        collapsible = TRUE,
        DTOutput(outputId = "cluster_table_b"))),
  
  fluidRow(
    box(title = "Cluster Plot-Jaccard",
        collapsible = TRUE,
        plotOutput(outputId = "cluster_plot_j")),
    
    box(title = "Dissimilarities Between Transect",
        collapsible = TRUE,
        DTOutput(outputId = "cluster_table_j")))
  
  ),


### Conservation status ----
tabItem(
  tabName = "stat_sum",

fluidRow(
  box(title = "Species and Conservation Status",
      collapsible = TRUE,
      DTOutput(outputId = "splistcs", width = "100%"))),

fluidRow(
  box(title = "Species Composition",
      collapsible = TRUE,
      plotOutput(outputId = "treemap")))
  
)))

# Statement----
dashboardPage(
  skin = "green",
  dashboardHeader(title = "FaunaTransectBased", titleWidth = 300),
  sidebar,
  body
)

# Preps ----
library(shiny)
library(bslib)
library(data.validator)
library(DT)
library(dplyr)
library(flextable)
library(ggplot2)
library(gridlayout)
library(iNEXT)
library(kableExtra)
library(rmarkdown)
library(shinybusy)
library(shinydashboard)
library(tidyverse)
library(treemap)
library(vegan)
library(rredlist)
library(rcites)
library(assertr)
library(U.Taxonstand)
library(formattable)
library(ecolTest)

# Data source----
source("source/iucn_code.R")
load("source/fauna_db.RData")

# UI ----
ui <- page_navbar(
  title = "Fauna Rapid Analysis",
  selected = "Validation",
  collapsible = TRUE,
  theme = bslib::bs_theme(preset = "minty"),
  
  # Validation Panel----
  nav_panel(
    title = "Validation",
    grid_container(
      layout = c("area0 area1"),
      row_sizes = c("1fr"),
      col_sizes = c("165px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "area0",
        card_header("Input"),
        card_body(
          fileInput(inputId = "filefauna", label = "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          selectInput(inputId = "validationType", label = "Dataset",
                      choices = c("Avifauna", "Herpetofauna", "Mammals")),
          actionButton(inputId = "faunastart", label = "Validate"),
          checkboxInput(inputId = "myCheckboxInput", label = "IUCN & CITES", value = FALSE),
          actionButton(inputId = "faunacalculate", label = "Calculate"),
          downloadButton("downloadReport")
        )
      ),
      grid_card(
        area = "area1",
        card_body(
          grid_container(
            layout = c(
              "area0 area3",
              "area2 area3"
            ),
            row_sizes = c("1fr", "1fr"),
            col_sizes = c("1fr", "1fr"),
            gap_size = "10px",
            
            ## data overview----
            grid_card(
              area = "area0",
              full_screen = TRUE,
              card_header("Data Overview"),
              card_body(
                div(
                  style = "text-align: center; background-color: #f0ad4e; padding: 10px; border-radius: 5px; margin-top: 20px;",
                  p(
                    style = "font-size: 18px; color: #fff;",
                    "Important: Before proceeding, please review each column name in your dataset carefully.",
                    "Ensure that your dataset's column names match the mandatory columns listed below.",
                    "The app's functionality depends on this matching.",
                    "If there are discrepancies, consider renaming your dataset columns accordingly."
                  )
                ),
                uiOutput(width = "100%", outputId = "diff_outputs"),
                uiOutput(width = "100%", outputId = "datasetHead")
              )
            ),
            
            ## data validation----          
            grid_card(
              area = "area2",
              full_screen = TRUE,
              card_header("Validation Report"),
              card_body(
                div(
                  style = "text-align: center; background-color: #f0ad4e; padding: 10px; border-radius: 5px; margin-top: 20px;",
                  p(
                    style = "font-size: 18px; color: #fff;",
                    "Important: Please ensure that all columns have passed validation.",
                    "If there are failures, your dataset needs to be edited manually to meet the required rules."
                  )
                ),
                uiOutput(outputId = "validation")
              )
            ),
            
            ## species list----          
            grid_card(
              area = "area3",
              full_screen = TRUE,
              card_header("List of Species and Suggested Latin Names"),
              card_body(uiOutput(outputId = "uspecies", width = "100%"))
            )
          )
        )
      )
    )
  ),
  
  # Summary Panel----
  nav_panel(
    title = "Summary",
    grid_container(
      layout = c("area0 area1", "area2 area2"),
      row_sizes = c("1fr", "1fr"),
      col_sizes = c("1fr", "1fr"),
      gap_size = "10px",
      
      ## species richness----
      grid_card(
        area = "area0",
        full_screen = TRUE,
        card_header("Species Richness Across Transect"),
        card_body(DTOutput(outputId = "spindex"))
      ),
      
      ## species composition----
      grid_card(
        area = "area1",
        full_screen = TRUE,
        card_header("Species Composition"),
        card_body(plotOutput(outputId = "treemap"))
      ),
      
      ## status sonservation----
      grid_card(
        area = "area2",
        full_screen = TRUE,
        card_header("Species and Conservation Status"),
        card_body(DTOutput(outputId = "splistcs", width = "100%"))
      )
    )
  ),
  
  # Plot Panel----
  nav_panel(
    title = "Plots",
    grid_container(
      layout = c("area0 area1", "area2 area3", "area4 area5"),
      row_sizes = c("1fr", "1fr", "1fr"),
      col_sizes = c("1fr", "1fr"),
      gap_size = "10px",
      
      ## abundance plot----
      grid_card(
        area = "area0",
        full_screen = TRUE,
        card_header("Species Richness and Abundance"),
        card_body(plotOutput(outputId = "rs_abd_plot"))
      ),
      
      ## diversity plot----
      grid_card(
        area = "area1",
        full_screen = TRUE,
        card_header("Diversity Indices"),
        card_body(plotOutput(outputId = "di_plot"))
      ),
      
      ## cluster plot----
      grid_card(
        area = "area2",
        full_screen = TRUE,
        card_header("Cluster Plot"),
        card_body(plotOutput(outputId = "cluster_plot"))
      ),
      
      ## cluster value----
      grid_card(
        area = "area3",
        full_screen = TRUE,
        card_header("Dissimilarities Between Transect"),
        card_body(
          # Title for the Bray Curtis Dissimilarity table
          div(
            tags$h4("Bray-Curtis Distance", style = "margin-bottom: 10px;"),  # Adjusted margin
            DTOutput(outputId = "cluster_table"),
            style = "padding-bottom: 20px;"  # Added padding between the tables
          ),
          
          # Title for the Huctenson t-test table
          div(
            tags$h4("Hutcheson's t-test", style = "margin-bottom: 10px;"),  # Adjusted margin
            DTOutput(outputId = "ht_table")
          )
        ),
        style = "overflow-y: auto;"  # Ensure card content is scrollable if it overflows
      ),
      
      ## accumulation curve----
      grid_card(
        area = "area4",
        full_screen = TRUE,
        card_header("Species Accumulation Curve"),
        card_body(plotOutput(outputId = "inext"))
      ),
      
      ## species estimation----
      grid_card(
        area = "area5",
        full_screen = TRUE,
        card_header("Species Richness Estimation"),
        card_body(DTOutput(outputId = "est_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactivity for dataset ----    
  datasetvalidate <- reactive({
    req(input$filefauna)
    data <- read.csv(file = input$filefauna$datapath,
                     na.strings = ".", 
                     sep = ",",
                     header = TRUE) %>%
      mutate(Scientific.Name = trimws(Scientific.Name))
    return(data)
  })
  
  datasetcalculate <- reactive({
    req(input$filefauna)
    data <- read.csv(file = input$filefauna$datapath,
                     na.strings = ".", 
                     sep = ",",
                     header = TRUE) %>%
      mutate(Scientific.Name = trimws(Scientific.Name))
    return(data)
  })  
  
  # Mandatory columns---- 
  createDataTableUI <- function(df, title) {
    
    table_title <- div(style = "text-align: center;", h3(title))
    
    table_ui <- df %>%
      head(3) %>%
      regulartable() %>% 
      autofit() %>%
      htmltools_value()
    
    div(style = "text-align: center;", table_title, table_ui)
  }
  
  # Create UI elements for each dataframe
  avifauna_ui <- createDataTableUI(avifauna, "Avifauna Dataset")
  mammals_ui <- createDataTableUI(mammals, "Mammals Dataset")
  herpetofauna_ui <- createDataTableUI(herpetofauna, "Herpetofauna Dataset")
  
  # Use renderUI to switch between different UI elements
  output$diff_outputs <- renderUI({
    if (is.null(input$validationType))
      return()
    switch(
      input$validationType,
      "Avifauna" = avifauna_ui,
      "Mammals" = mammals_ui,
      "Herpetofauna" = herpetofauna_ui
    )
  })
  
  # Render your data----
  output$datasetHead <- renderUI({
    req(datasetvalidate())
    
    table_title <- div(style = "text-align: center;",
                       h3("Your Dataset"))
    
    table_ui <- datasetvalidate() %>%
      head(3) %>%
      regulartable() %>% 
      autofit() %>%
      htmltools_value()
    
    div(table_title, table_ui)
  })  
  
  
  # Validate action----  
  observeEvent(input$faunastart, {
    
    show_modal_spinner(
      spin = "orbit",
      color = "#112446",
      text = "Tenang, app-nya jalan kok. Seruput kopi dan ngudud dulu~")
    
    ## Data validation----
    validateData <- reactive({
      
      report <- data_validation_report() 
      
      if (input$validationType == "Avifauna") {
        data.validator::validate(datasetvalidate(), description = "avifauna dataset validation") %>%
          validate_cols(predicate = not_na, Landscape:Observation.Type, description = "No missing values") %>%
          validate_cols(in_set(c("Species", "Genus", "Family", "Ordo")), Taxon.Rank, description = "Correct Taxon Rank category") %>%
          validate_if(description = "positive value", Indv > 0) %>%
          validate_if(description = "positive value", Distance > 0) %>%
          add_results(report)
        
      } else if (input$validationType == "Herpetofauna") {
        data.validator::validate(datasetvalidate(), description = "herpetofauna dataset validation") %>%
          validate_cols(predicate = not_na, Landscape:Day.Night, description = "No missing values") %>%
          validate_cols(predicate = not_na, Observer:Taxon.Rank, description = "No missing values") %>%
          validate_cols(predicate = not_na, Family:Group2, description = "No missing values") %>%
          validate_cols(in_set(c("Species", "Genus", "Family", "Ordo")), Taxon.Rank, description = "Correct Taxon Rank category") %>%
          validate_cols(in_set(c("Amphibians", "Reptiles")), Group1, description = "Correct Group1 category") %>%
          validate_cols(in_set(c("Frogs", "Snakes","Caecilians", "Agamids","Saurias", "Lizards", "Varanids", "Turtles", "Crocodiles", "Geckoes")), Group2, description = "Correct Group2 category") %>%
          add_results(report)
        
      } else if (input$validationType == "Mammals") {
        data.validator::validate(datasetvalidate(), description = "mammals dataset validation") %>%
          validate_cols(predicate = not_na, Landscape:Site, description = "No missing values") %>%
          validate_cols(predicate = not_na, Scientific.Name:Taxon.Rank, description = "No missing values") %>%
          validate_cols(predicate = between(95, 142), Longitude, description = "Indonesia Longitude Range") %>%
          validate_cols(predicate = between(-11, 7), Latitude, description = "Indonesia Latitude Range") %>%
          validate_cols(in_set(c("Species", "Genus", "Family", "Ordo")), Taxon.Rank, description = "Correct Taxon Rank category") %>%
          add_results(report)
      } 
      
      report
      
    })
    
    ## Render the data validation report----
    output$validation <- renderUI({
      render_semantic_report_ui(get_results(report = validateData()))
    })
    
    ## Render species list and cross with database----
    output$uspecies <- renderUI({
      req(datasetvalidate()) 
      req(input$validationType)
      
      # Select the appropriate database based on validation type
      validation_db <- switch(input$validationType,
                              "Avifauna" = brd_db,
                              "Herpetofauna" = herp_db,
                              "Mammals" = mml_db)
      
      # Running the nameMatch function
      uspl <- datasetvalidate() %>%
        arrange(Scientific.Name) 
      uspl <- unique(uspl$Scientific.Name)
      
      res <- nameMatch(spList = uspl, spSource = validation_db, author = FALSE, max.distance = 1)
      
      remove_modal_spinner()
      
      # Render the table with DT and custom formatting
      DT::datatable(res %>% select("Submitted Name"=Submitted_Name, "Different with database"=Fuzzy, Score, "Suggested Name"=Accepted_SPNAME), 
                    extensions = 'Buttons',  # Add this line to enable DataTables Button extension
                    options = list(
                      paging = FALSE,
                      scrollX = TRUE, 
                      searching = TRUE,
                      ordering = TRUE,
                      autoWidth = TRUE,
                      dom = 'Bfrtip',  # Defines the table control elements to appear on the page
                      buttons = c('copy', 'csv', 'excel', 'pdf'),  # Define the buttons for download and copy
                      scrollY = "500px"),
                    escape = FALSE) %>%
        DT::formatStyle("Different with database", 
                        target = 'cell', 
                        color = styleEqual(c(TRUE, FALSE), c('red', 'forestgreen')))
    })
    
  })
  
  # Run the analysis----     
  observeEvent(input$faunacalculate, {
    
    show_modal_spinner(
      spin = "orbit",
      color = "#112446",
      text = "Tenang, app-nya jalan kok. Seruput kopi dan ngudud dulu~")
    
    req(datasetcalculate())
    
    if (input$myCheckboxInput) {
      
      Species_df <- datasetcalculate() %>%
        filter(Taxon.Rank == "Species") %>%
        distinct(Species = Scientific.Name)
      
      sp1 <- retrieve_IUCN_data(Species_df$Species)
      sp2 <- retrieve_CITES_data(Species_df$Species)
      sp2 <- sp2 %>%
        distinct(Species, .keep_all = TRUE)
      sp3 <- spp_cites_legislation(taxon_id = sp2$taxon_id, verbose = FALSE)
      sp3 <- as.data.frame (sp3[["cites_listings"]])
      sp3 <- sp3 %>%
        distinct(taxon_id, .keep_all = TRUE)
      
      species_list <- left_join(sp1, sp2, by='Species') %>%
        left_join(., sp3, by='taxon_id') %>%
        left_join(., db, by="Species") %>%
        select(Class, Order, Family, Species, Status, Trend, appendix, Protected, Endemic, Migratory) %>%
        mutate_at(vars(Class, Order, Family), tolower) %>%
        mutate_at(vars(Class, Order, Family), str_to_title) %>%
        rename(Appendix = appendix)
      
      species_list <- arrange(species_list, Order, Family, Species)
    }
    
    remove_modal_spinner()
    
    ## Table of species richness----
    output$spindex <- renderDT({
      
      req(datasetcalculate())
      
      dataspr <- datasetcalculate() %>%
        filter(Taxon.Rank %in% c("Species", "Genus")) %>%
        {
          if ("Observation.Type" %in% names(.)) {
            filter(., Observation.Type == "PointLoc")
          } else {
            .
          }
        } %>%
        {
          if ("Indv" %in% names(.)) {
            drop_na(., Indv)
          } else {
            mutate(., Indv = 1)
          }
        } %>%
        group_by(Transect, Scientific.Name) %>%
        summarize(n = sum(Indv, na.rm = TRUE)) %>%
        summarize(Richness = n_distinct(Scientific.Name),
                  Abundance = sum(n),
                  Shannon = -sum(prop.table(n) * log(prop.table(n))),
                  Margalef = (n_distinct(Scientific.Name) - 1) / log(sum(n)),
                  Evenness = (-sum(prop.table(n) * log(prop.table(n))))/log(length(n)),
                  Simpson = sum(prop.table(n)^2)) %>%
        mutate(across(4:last_col(), round, 2))
      
      datatable(dataspr, extensions = "Buttons", 
                options = list(
                  paging = TRUE,
                  scrollX = TRUE, 
                  searching = TRUE,
                  ordering = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf'),
                  pageLength = 10, 
                  lengthMenu = c(3, 5, 10)))
    })
    
    ## Table of taxon and conservation status ----
    output$splistcs <- renderDT({
      datatable(species_list, extensions = "Buttons", 
                options = list(paging = TRUE,
                               scrollX = '400px', 
                               searching = TRUE,
                               ordering = TRUE,
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf'),
                               pageLength=10,
                               lengthMenu=c(3,5,10) ))
      
    })
    
    ## Conditional plot of treemap ----
    if (input$myCheckboxInput) {
      output$treemap <- renderPlot({
        rearranged_species <- species_list %>%
          group_by(Order, Family) %>%
          filter(Order != "Na") %>%
          summarise(Num_Species = n())
        # Custom labels:
        treemap(rearranged_species, index = c("Order", "Family"),
                vSize = "Num_Species", type = "index",
                title = "", 
                fontsize.labels = c(15, 12),
                fontcolor.labels = c("white", "orange"),
                fontface.labels = c(2, 1),
                bg.labels = c("transparent"),
                align.labels = list(
                  c("center", "center"),
                  c("right", "bottom")
                ),
                overlap.labels = 0.5,
                inflate.labels = F
        )
      })
    } 
    
    ## abundance plot----
    output$rs_abd_plot <- renderPlot({
      req(datasetcalculate())
      
      dataspr <- datasetcalculate() %>%
        filter(Taxon.Rank %in% c("Species", "Genus")) %>%
        {
          if ("Observation.Type" %in% names(.)) {
            filter(., Observation.Type == "PointLoc")
          } else {
            .
          }
        } %>%
        {
          if ("Indv" %in% names(.)) {
            drop_na(., Indv)
          } else {
            mutate(., Indv = 1)
          }
        } %>%
        group_by(Transect, Scientific.Name) %>%
        summarize(n = sum(Indv, na.rm = TRUE)) %>%
        summarize(Richness = n_distinct(Scientific.Name),
                  Abundance = sum(n),
                  Shannon = -sum(prop.table(n) * log(prop.table(n))),
                  Margalef = (n_distinct(Scientific.Name) - 1) / log(sum(n)),
                  Evenness = (-sum(prop.table(n) * log(prop.table(n))))/log(length(n)),
                  Simpson = sum(prop.table(n)^2)) %>%
        mutate(across(4:last_col(), round, 2))
      
      dataspr %>%
        select(c(Transect, Richness, Abundance)) %>% 
        pivot_longer(-Transect, names_to = "Category", values_to = "values") %>%
        ggplot(aes(fill=Category, y=values, x=Transect)) + 
        geom_col(position="dodge", width = 0.8) + 
        geom_text(aes(label = round(values,1)), 
                  position = position_dodge(0.8), vjust = -0.5, hjust = 0.5) + 
        theme_bw()
    })  
    
    ## diversity plot----
    output$di_plot <- renderPlot({
      req(datasetcalculate())
      
      dataspr <- datasetcalculate() %>%
        filter(Taxon.Rank %in% c("Species", "Genus")) %>%
        {
          if ("Observation.Type" %in% names(.)) {
            filter(., Observation.Type == "PointLoc")
          } else {
            .
          }
        } %>%
        {
          if ("Indv" %in% names(.)) {
            drop_na(., Indv)
          } else {
            mutate(., Indv = 1)
          }
        } %>%
        group_by(Transect, Scientific.Name) %>%
        summarize(n = sum(Indv, na.rm = TRUE)) %>%
        summarize(Richness = n_distinct(Scientific.Name),
                  Abundance = sum(n),
                  Shannon = -sum(prop.table(n) * log(prop.table(n))),
                  Margalef = (n_distinct(Scientific.Name) - 1) / log(sum(n)),
                  Evenness = (-sum(prop.table(n) * log(prop.table(n))))/log(length(n)),
                  Simpson = sum(prop.table(n)^2)) %>%
        mutate(across(4:last_col(), round, 2))
      
      dataspr %>%
        select(c(Transect,Shannon,Margalef)) %>% 
        pivot_longer(-Transect, names_to = "Richness Index", values_to = "values") %>%
        ggplot(aes(fill=`Richness Index`, y=values, x=Transect)) + 
        geom_col(position="dodge", width = 0.8) + 
        geom_text(aes(label = round(values,2)), 
                  position = position_dodge(0.8), vjust = -0.5, hjust = 0.5) + 
        theme_bw()
    })
    
    ## Render cluster plot----
    output$cluster_plot <- renderPlot({
      
      req(datasetcalculate())
      
      rawsp <- datasetcalculate() %>%
        filter(Taxon.Rank %in% c("Species", "Genus")) %>%
        {
          if ("Observation.Type" %in% names(.)) {
            filter(., Observation.Type == "PointLoc")
          } else {
            .
          }
        } %>%
        {
          if ("Indv" %in% names(.)) {
            drop_na(., Indv)
          } else {
            mutate(., Indv = 1)
          }
        } 
      
      # Aggregate data and transform into matrix format
      aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
      data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
      data_matrix[is.na(data_matrix)] <- 0
      data_matrix_table <- as.matrix(data_matrix[, -1])  # Exclude the Transect column
      rownames(data_matrix_table) <- data_matrix$Transect
      
      # Hierarchical clustering
      hc_transect <- data_matrix_table %>%
        vegdist(method = "bray") %>%
        hclust(method = "average")
      
      # Plot
      plot(hc_transect, xlab = "", ylab = "Dissimilarity", sub = "Transect", hang = -1)
    })
    
    ## Render cluster table----
    output$cluster_table <- renderDT({
      req(datasetcalculate())
      
      rawsp <- datasetcalculate() %>%
        filter(Taxon.Rank %in% c("Species", "Genus")) %>%
        {
          if ("Observation.Type" %in% names(.)) {
            filter(., Observation.Type == "PointLoc")
          } else {
            .
          }
        } %>%
        {
          if ("Indv" %in% names(.)) {
            drop_na(., Indv)
          } else {
            mutate(., Indv = 1)
          }
        } 
      
      # Aggregate data and transform into matrix format
      aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
      data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
      data_matrix[is.na(data_matrix)] <- 0
      data_matrix_table <- as.matrix(data_matrix[, -1])  # Exclude the Transect column
      rownames(data_matrix_table) <- data_matrix$Transect
      
      # Hierarchical clustering
      hc_table <- data_matrix_table %>%
        vegdist(method = "bray") 
      
      # table
      hct <- hc_table %>%
        as.matrix() %>%
        as.data.frame() %>%
        round(2) 
      
      datatable(hct, extensions = "Buttons", 
                options = list(
                  paging = FALSE,
                  scrollX = TRUE, 
                  searching = TRUE,
                  ordering = TRUE,
                  dom = 'Bfrtip',  
                  buttons = c('copy', 'csv', 'excel', 'pdf'), 
                  escape = FALSE))
      
    })  
    
    ## Render hutchenson test----
    output$ht_table <- renderDT({
      
      req(datasetcalculate())
      
      rawsp <- datasetcalculate() %>%
        filter(Taxon.Rank %in% c("Species", "Genus")) %>%
        {
          if ("Observation.Type" %in% names(.)) {
            filter(., Observation.Type == "PointLoc")
          } else {
            .
          }
        } %>%
        {
          if ("Indv" %in% names(.)) {
            drop_na(., Indv)
          } else {
            mutate(., Indv = 1)
          }
        } 
      
      # Aggregate data and transform into matrix format
      aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
      data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
      data_matrix[is.na(data_matrix)] <- 0
      data_matrix_table <- as.matrix(data_matrix[, -1])  # Exclude the Transect column
      rownames(data_matrix_table) <- data_matrix$Transect
      
      # table
      ht <- data_matrix_table %>%
        t() %>%
        as.data.frame() %>%
        multiple_Hutcheson_t_test() 
      
      ht_table <- ht[["p.values"]] %>%
        round(2) %>%
        as.data.frame()
      
      datatable(ht_table, extensions = "Buttons", 
                options = list(
                  paging = FALSE,
                  scrollX = TRUE, 
                  searching = TRUE,
                  ordering = TRUE,
                  dom = 'Bfrtip',  
                  buttons = c('copy', 'csv', 'excel', 'pdf'), 
                  escape = FALSE)) %>%
        DT::formatStyle(names(ht_table), 
                        target = 'cell', 
                        backgroundColor = styleInterval(0.05, c('yellow', 'white')))
    })  
    
    
    
    ## Species accumulation curve----
    output$inext <- renderPlot({
      show_modal_spinner(
        spin = "orbit",
        color = "#112446",
        text = "Tenang, app-nya jalan kok. Seruput kopi dan ngudud dulu~"
      )
      
      req(datasetcalculate())
      
      out1 <- datasetcalculate() %>%
        filter(Taxon.Rank %in% c("Species", "Genus")) %>%
        {
          if ("Indv" %in% names(.)) {
            drop_na(., Indv)
          } else {
            mutate(., Indv = 1)
          }
        }  %>%
        group_by(Scientific.Name) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        as.data.frame()
      
      rownames(out1) <- out1$Scientific.Name
      out1$Scientific.Name <- NULL
      
      out1 <- iNEXT(out1, q = 0, datatype = "abundance")
      
      remove_modal_spinner()
      
      ggiNEXT(x = out1, type = 1, color.var = "Order.q") +
        labs(x = "Number of Individuals", y = "Cumulative Species Richness") +
        theme_bw() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              legend.position = "bottom",
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12))
    })
    
    
    ## Estimate species richness----
    output$est_table <- renderDT({
      req(datasetcalculate())
      
      # Calculate observed species richness
      observed <- datasetcalculate() %>% 
        filter(Taxon.Rank %in% c("Species", "Genus")) %>%
        distinct(Scientific.Name) %>%
        nrow()
      
      new_row <- data.frame(
        Method = "    Observed Species",  # Name of the method/row
        Estimate = observed,          # Count value to be added
        s.e. = NA,                       # Fill with NA if not applicable
        "95%Lower" = NA,                 # Fill with NA if not applicable
        "95%Upper" = NA                  # Fill with NA if not applicable
      )
      
      colnames(new_row) <- c("Method", "Estimate", "s.e.", "95%Lower", "95%Upper")
      
      out1 <- datasetcalculate() %>%
        filter(Taxon.Rank %in% c("Species", "Genus")) %>%
        {
          if ("Indv" %in% names(.)) {
            drop_na(., Indv)
          } else {
            mutate(., Indv = 1)
          }
        }  %>%
        group_by(Scientific.Name) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        as.data.frame()
      
      # Convert the output to a data frame
      out1 <- as.data.frame(out1)
      rownames(out1) <- out1$Scientific.Name
      out1$Scientific.Name <- NULL
      
      # Apply the ChaoSpecies method
      out2 <- SpadeR::ChaoSpecies(out1, datatype = "abundance")
      
      # Convert and bind the results
      out3 <- as.data.frame(out2$Species_table)
      out3 <- tibble::rownames_to_column(out3, "Method")
      out3 <- rbind(new_row, out3)
      
      # Select specific rows (1, 2, and 5)
      final_table <- out3[c(1, 2, 6,9), ]
      
      # Render the datatable
      datatable(final_table, extensions = "Buttons", 
                options = list(
                  paging = FALSE,
                  scrollX = TRUE, 
                  searching = TRUE,
                  ordering = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf'),
                  escape = FALSE))
    })
    
    
  })
  
  # Generate Report----  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", ".docx", sep = "")
    },
    
    content = function(file) {
      
      show_modal_spinner(
        spin = "orbit",
        color = "#112446",
        text = "Tenang, app-nya jalan kok. Seruput kopi dan ngudud dulu~")
      
      src <- if (input$myCheckboxInput) {
        normalizePath("fauna_basic_v4.1.Rmd")
      } else {
        normalizePath("fauna_basic_v4.2.Rmd")  # Use v3 template if unchecked
      }
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "fauna_basic.Rmd", overwrite = TRUE)
      
      out <- render("fauna_basic.Rmd", word_document(), params = list(data = input$filefauna$datapath))
      file.rename(out, file)
      
      remove_modal_spinner()
    })  
  
}

shinyApp(ui, server)



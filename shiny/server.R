# Server ----

# Data Processing ----

## Input csv----
server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file1)
    inFile <- input$file1
    read.csv(inFile$datapath)
  })
  
## Select column----  
  output$columnSelectUI <- renderUI({
    req(dataset())
    # Determine default selections based on the validation type
    default_selection <- if (input$validationType == "avifauna") {
      names(dataset())[1:5]  # Select first 5 columns if "avifauna" is chosen
    } else {
      names(dataset())[1:3]  # Select first 3 columns otherwise
    }
    selectInput("selectedColumns", 
                HTML("Pilih 4 kolom secara berurutan; Transect, Scientific.Name, dan Taxon.Rank.<br><br>
                    Khusus untuk avifauna tambahkan Observation.Type dan Indv."), 
                choices = names(dataset()), 
                multiple = TRUE, selected = default_selection)
  })
  
  
# Data validation ---- 
  processedData <- eventReactive(input$faunastart, {
    
    show_modal_spinner(
      spin = "orbit",
      color = "#112446",
      text = "Tenang, app-nya jalan kok. Seruput kopi dan ngudud dulu~"
    )
    
    req(input$selectedColumns)
    data <- dataset()
    data <- data[, input$selectedColumns, drop = FALSE]
    # Dynamically assign column names based on the number of selected columns
    if (length(input$selectedColumns) == 5) {
      names(data) <- c("Transect", "Scientific.Name", "Taxon.Rank", "Observation.Type", "Indv")
    } else {
      names(data) <- c("Transect", "Scientific.Name", "Taxon.Rank")
    }
    data
  })
  

### show your dataset----  
  output$datasetHead <- renderUI({
    req(processedData())
    table_ui <- processedData() %>%
      head(5) %>%
      regulartable() %>% 
      autofit() %>%
      htmltools_value()
    
  }) 
  
### validation report----
  output$validationReport <- renderText({
    req(processedData(), input$validationType)  # Ensure necessary inputs are available
    data <- processedData()
    
    # Start with an empty report list
    report <- list("")
    
    # Validation for Transect
    if (all(sapply(data$Transect, is.character))) {
      report$Transect <- paste("Transect: OK with", length(unique(data$Transect)), "total transects")
    } else {
      stop("Transect: Error, had empty values")
    }
    
    # Validation for Scientific.Name
    if (all(sapply(data$Scientific.Name, is.character))) {
      report$Scientific_Name <- paste("Scientific.Name: OK with", length(unique(data$Scientific.Name)), "total species")
    } else {
      stop("Scientific.Name: Error, had empty values")
    }
    
    # Validation for Taxon.Rank
    if (all(sapply(data$Taxon.Rank, is.character))) {
      unique_taxon_ranks <- unique(data$Taxon.Rank)
      contains_species <- "Species" %in% data$Taxon.Rank
      species_check <- if (contains_species) "and includes 'Species'" else "and does not include 'Species'"
      report$Taxon_Rank <- paste("Taxon.Rank: OK", species_check)
    } else {
      stop("Taxon.Rank: Error, should be all strings")
    }
    
    # Additional validations only for Avifauna
    if (input$validationType == "Avifauna") {
      if ("PointLoc" %in% data$Observation.Type) {
        report$Observation_Type <- "Observation.Type: OK and Contains 'PointLoc'"
      } else {
        stop("Observation.Type: Error, should contain 'PointLoc'")
      }
      if (all(sapply(data$Indv, is.numeric))) {
        report$Indv <- paste("Indv: OK,", "with range values between", min(data$Indv), "-", max(data$Indv), "metres")
      } else {
        stop("Indv: Error - Should be numeric.")
      }
    }
    
    paste(unlist(report), collapse = "\n")
  })
  
  
  
### Render species list and cross with database----
  output$uspecies <- renderUI({
    req(processedData()) 
    req(input$validationType)
    
    validation_db <- switch(input$validationType,
                            "Avifauna" = brd_db,
                            "Herpetofauna" = herp_db,
                            "Mammals" = mml_db)
    
    uspl <- processedData() %>%
      arrange(Scientific.Name) 
    uspl <- unique(uspl$Scientific.Name)
    
    res <- nameMatch(spList = uspl, spSource = validation_db, author = FALSE, max.distance = 1)
    
    remove_modal_spinner()
    
    DT::datatable(res %>% select("Submitted Name"=Submitted_Name, "Different with database"=Fuzzy, Score, "Suggested Name"=Accepted_SPNAME), 
                  extensions = 'Buttons', 
                  options = list(
                    paging = FALSE,
                    scrollX = TRUE, 
                    searching = TRUE,
                    ordering = TRUE,
                    autoWidth = TRUE,
                    dom = 'Bfrtip',  
                    buttons = c('copy', 'csv', 'excel', 'pdf'),  
                    scrollY = "500px"),
                  escape = FALSE) %>%
      DT::formatStyle("Different with database", 
                      target = 'cell', 
                      color = styleEqual(c(TRUE, FALSE), c('red', 'forestgreen')))
    

    
  })
  
# Data calculation ----   
  calculateData <- eventReactive(input$faunacalculate, {
    
    req(processedData())
    calculateData <- req(processedData())
    
  })
  
### Table of species richness----
  output$spindex <- renderDT({
    
    req(calculateData())
    
    dataspr <- calculateData() %>%
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
  
### abundance plot----
  output$rs_abd_plot <- renderPlot({
    req(calculateData())
    
    dataspr <- calculateData() %>%
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
  
  
### Estimate species richness----
  output$est_table <- renderDT({
    req(calculateData())
    
    # Calculate observed species richness
    observed <- calculateData() %>% 
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
    
    out1 <- calculateData() %>%
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
    
    out1 <- as.data.frame(out1)
    rownames(out1) <- out1$Scientific.Name
    out1$Scientific.Name <- NULL
    
    out2 <- SpadeR::ChaoSpecies(out1, datatype = "abundance")
    
    out3 <- as.data.frame(out2$Species_table)
    out3 <- tibble::rownames_to_column(out3, "Method")
    out3 <- rbind(new_row, out3)
    
    final_table <- out3[c(1, 2, 6,9), ]
    
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
  
  
### Species accumulation curve----
  output$inext <- renderPlot({
    
    req(calculateData())
    
    out1 <- calculateData() %>%
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
    
    ggiNEXT(x = out1, type = 1, color.var = "Order.q") +
      labs(x = "Number of Individuals", y = "Cumulative Species Richness") +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position = "bottom",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12))
    
  })
  
### Render cluster plot----
  output$cluster_plot_b <- renderPlot({
    
    req(calculateData())
    
    rawsp <- calculateData() %>%
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
    
    aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
    data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
    data_matrix[is.na(data_matrix)] <- 0
    data_matrix_table <- as.matrix(data_matrix[, -1])  # Exclude the Transect column
    rownames(data_matrix_table) <- data_matrix$Transect
    
    hc_transect <- data_matrix_table %>%
      vegdist(method = "bray") %>%
      hclust(method = "average")

    plot(hc_transect, xlab = "", ylab = "Dissimilarity", sub = "Transect", hang = -1)
  })
  
### Render cluster table----
  output$cluster_table_b <- renderDT({
    
    req(calculateData())
    
    rawsp <- calculateData() %>%
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
    
    aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
    data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
    data_matrix[is.na(data_matrix)] <- 0
    data_matrix_table <- as.matrix(data_matrix[, -1])  # Exclude the Transect column
    rownames(data_matrix_table) <- data_matrix$Transect
    
    hc_table <- data_matrix_table %>%
      vegdist(method = "bray") 
    
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
  
  ### Render cluster plot----
  output$cluster_plot_j <- renderPlot({
    
    req(calculateData())
    
    rawsp <- calculateData() %>%
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
    
    aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
    data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
    data_matrix[is.na(data_matrix)] <- 0
    data_matrix_table <- as.matrix(data_matrix[, -1])  # Exclude the Transect column
    rownames(data_matrix_table) <- data_matrix$Transect
    
    hc_transect <- data_matrix_table %>%
      vegdist(method = "jaccard") %>%
      hclust(method = "average")
    
    plot(hc_transect, xlab = "", ylab = "Dissimilarity", sub = "Transect", hang = -1)
  })
  
  ### Render cluster table----
  output$cluster_table_j <- renderDT({
    
    req(calculateData())
    
    rawsp <- calculateData() %>%
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
    
    aggregated_data <- aggregate(Indv ~ Transect + Scientific.Name, data = rawsp, sum)
    data_matrix <- reshape2::dcast(aggregated_data, Transect ~ Scientific.Name, value.var = "Indv")
    data_matrix[is.na(data_matrix)] <- 0
    data_matrix_table <- as.matrix(data_matrix[, -1])  # Exclude the Transect column
    rownames(data_matrix_table) <- data_matrix$Transect
    
    hc_table <- data_matrix_table %>%
      vegdist(method = "jaccard") 
    
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
  
  
  # Initialize species_list as a reactive expression
  species_list <- eventReactive(input$searchcs, {
    speciesCons <- req(processedData())
    
    # Show a spinner here
    show_modal_spinner(
      spin = "orbit",
      color = "#112446",
      text = "Tenang, app-nya jalan kok. Seruput kopi dan ngudud dulu~"
    )
    
    Species_df <- speciesCons %>%
      filter(Taxon.Rank == "Species") %>%
      distinct(Species = Scientific.Name)
    
    sp1 <- retrieve_IUCN_data(Species_df$Species)
    sp2 <- retrieve_CITES_data(Species_df$Species) %>%
      distinct(Species, .keep_all = TRUE)
    
    # Check if sp2 'taxon_id' column contains only the string "NA"
    if (all(sp2$taxon_id == "NA")) {
      # Proceed without sp2 and sp3 data
      result <- left_join(sp1, db, by = "Species") %>%
        select(Class, Order, Family, Species, Status, Trend, Protected, Endemic, Migratory) %>%
        mutate(across(c(Class, Order, Family), tolower)) %>%
        mutate(across(c(Class, Order, Family), tools::toTitleCase)) %>%
        arrange(Order, Family, Species)
    } else {
      # Proceed with additional data processing involving sp2 and sp3
    sp3 <- spp_cites_legislation(taxon_id = sp2$taxon_id, verbose = FALSE)
    sp3 <- as.data.frame (sp3[["cites_listings"]])
    sp3 <- sp3 %>%
      distinct(taxon_id, .keep_all = TRUE)
    
      
      result <- left_join(sp1, sp2, by='Species') %>%
        left_join(., sp3, by='taxon_id') %>%
        left_join(., db, by="Species") %>%
        select(Class, Order, Family, Species, Status, Trend, appendix, Protected, Endemic, Migratory) %>%
        mutate(across(c(Class, Order, Family), tolower)) %>%
        mutate(across(c(Class, Order, Family), tools::toTitleCase)) %>%
        rename(Appendix = appendix) %>%
        arrange(Order, Family, Species)
    }
    
    remove_modal_spinner() 
    result # Return the result
  })
  
  ### Table of taxon and conservation status ----
  output$splistcs <- renderDT({
    req(species_list())  # Ensure the data is ready before trying to use it
    datatable(species_list(), extensions = "Buttons", options = list(
      paging = FALSE,
      scrollX = TRUE,
      searching = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf'),
      escape = FALSE
    ))
  })
  
  ### Conditional plot of treemap ----
  output$treemap <- renderPlot({
    req(species_list)  # Ensure the data is ready
    rearranged_species <- species_list() %>%
      group_by(Order, Family) %>%
      filter(Order != "Na") %>%
      summarise(Num_Species = n(), .groups = 'drop')
    
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

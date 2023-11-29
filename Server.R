server <- function(input, output, session) {
  observe({
    if (!input$colorBy %in% names(brazil)) {
      output$map <- renderLeaflet({})
      return()
    }
    
    colorVariable <- brazil[[input$colorBy]]
    normalizedValues <- (colorVariable - min(colorVariable, na.rm = TRUE)) / (max(colorVariable, na.rm = TRUE) - min(colorVariable, na.rm = TRUE))
    
    palette <- colorRampPalette(c("white", switch(input$colorBy,
                                                  "carnivore" = "red",
                                                  "detritivore" = "blue",
                                                  "frugivorous" = "green",
                                                  "granivore" = "yellow",
                                                  "herbivore" = "orange",
                                                  "insectivorous" = "firebrick",
                                                  "nectarivore" = "violet",
                                                  "omnivorous" = "grey",
                                                  "piscivorous" = "purple")))
    
    brazil$colorBasedOnVariable <- palette(100)[ceiling(normalizedValues * 99) + 1]
    
    output$map <- renderLeaflet({
      leaflet(data = brazil) %>%
        addTiles() %>%
        addCircleMarkers(
          lat = ~Lat,
          lng = ~Long,
          color = ~colorBasedOnVariable,
          popup = ~paste0("Site: ", SITES, "<br>", input$colorBy, ": ", brazil[[input$colorBy]])
        )
    })
  })
  
  #Map 2 logic
  
  speciesNames <- names(brazil)[13:99]
  
  # Create a color factor for the sites
  colorBySite <- colorFactor(rainbow(length(unique(brazil$SITES))), brazil$SITES)
  
  observe({
    output$map2 <- renderLeaflet({
      leaflet(data = brazil) %>%
        addTiles() %>%
        addCircleMarkers(
          lat = ~Lat,
          lng = ~Long,
          color = ~colorBySite(SITES),
          popup = ~sapply(1:nrow(brazil), function(i) {
            # Extract the counts for each species at each site
            speciesCountsForSite <- brazil[i, speciesNames]
            validSpeciesIndices <- which(speciesCountsForSite >= 1)
            validSpecies <- speciesNames[validSpeciesIndices]
            validCounts <- speciesCountsForSite[validSpeciesIndices]
            
            speciesCounts <- paste0(validSpecies, ": ", validCounts, collapse="<br>")
            paste0("Site: ", brazil$SITES[i], "<br>", speciesCounts)
          })
        )
    })
  })
  
  observeEvent(input$show_instructions_map2, {
    showModal(modalDialog(
      title = "Navigation Instructions",
      "Use your mouse to pan and zoom on the map. Click the circles to see detailed information about each site.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  #Map 3 logic
  
  # Sum of species counts (13:99) for each site
  speciesNames <- names(brazil)[13:99]
  brazil$totalSpeciesCount <- rowSums(brazil[, speciesNames], na.rm = TRUE)
  
  # Define maximum and minimum sizes for markers
  maxSize <- 50
  minSize <- 10
  
  # Scale the total species count for marker sizes
  scaledCounts <- (brazil$totalSpeciesCount - min(brazil$totalSpeciesCount)) / (max(brazil$totalSpeciesCount) - min(brazil$totalSpeciesCount)) * (maxSize - minSize) + minSize
  
  # Create a color bin function for total species counts
  colorByCounts <- colorBin(palette = "viridis", bins = 5, domain = brazil$totalSpeciesCount)
  
  # Render the Leaflet map with circle markers
  output$map3 <- renderLeaflet({
    leaflet(data = brazil) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~Lat,
        lng = ~Long,
        color = ~colorByCounts(totalSpeciesCount), 
        radius = ~scaledCounts, 
        popup = ~paste0("Site: ", SITES, "<br>Total Species Count: ", totalSpeciesCount)
      )
  })
  
  # Show modal dialog with instructions when the button is clicked
  observeEvent(input$show_instructions, {
    showModal(modalDialog(
      title = "Navigation Instructions",
      "Use your mouse to pan and zoom on the map. Click the circles to see detailed information about each site's total species count.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  

  
  
  #visualizations 
#VIS 1
  output$seasonalrichness_plot <- renderPlot({
    req(input$group_var)
    
    subset_data <- brazil_2 %>% filter(Com_Name %in% input$group_var)
    
    if (nrow(subset_data) == 0) {
      return(ggplot() + 
               labs(title = "No data available for the selected groups"))
    }
    
    summarized_data <- subset_data %>%
      group_by(Com_Name, MYear) %>%
      summarise(count = n(), .groups = 'drop')
    
    ggplot(summarized_data, aes(x = MYear, y = count, fill = Com_Name)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Species Richness Across Years",
           x = "Year",
           y = "Count of Individuals") +
      theme_minimal() +
      scale_fill_viridis_d()
  })
  
  output$seasonalrichness_table <- renderDT({
    req(input$group_var)
    
    subset_data <- brazil_2 %>% filter(Com_Name %in% input$group_var)
    
    if (nrow(subset_data) == 0) {
      return(data.frame(Message = "No data available for the selected groups"))
    }
    
    summarized_data <- subset_data %>%
      group_by(Com_Name, MYear) %>%
      summarise(count = n(), .groups = 'drop')
    
    summarized_data
  }, options = list(pageLength = 5, scrollX = TRUE))
  
  observeEvent(input$show_instructions, {
    toggle("instruction_text")
  })
  
    # Show instructions modal on startup
    showModal(modalDialog(
      title = "Welcome to the Aquiduana, Brazil Species App",
      "This app allows you to interact and run analysis in various ways; you can click to select multiple feeding guilds, species and see their distributions across different sites through maps, graphs, tables and analysis.",
      "e.g., The bar plot shows the richness of the selected guilds by site.",
      "also, Below the plot, a table displays the corresponding richness values by site and guild.",
      easyClose = TRUE,
      footer = NULL
    ))
 #VIS 2   
    # Calculate richness for each site
    brazil$richness <- rowSums(brazil[ , grepl("piscivorous|omnivorous|nectarivore|insectivorous|herbivore|granivore|frugivorous|detritivore|carnivore", names(brazil))])
    
    output$richness_comparison_plot <- renderPlot({
      req(input$selected_guilds)
      
      brazil_long <- brazil %>% 
        select(SITES, richness, all_of(input$selected_guilds)) %>% 
        pivot_longer(cols = input$selected_guilds, names_to = "category", values_to = "value")
      
      ggplot(brazil_long, aes(x = SITES, y = value, fill = category)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Comparison of Feeding Guilds Against Species Richness",
             x = "Site",
             y = "Richness") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    })
    
    output$richness_table <- renderDT({
      req(input$selected_guilds)
      
      brazil_long <- brazil %>% 
        select(SITES, richness, all_of(input$selected_guilds)) %>% 
        pivot_longer(cols = input$selected_guilds, names_to = "category", values_to = "value") %>% 
        group_by(SITES, category) %>% 
        summarise(Richness = sum(value, na.rm = TRUE), .groups = 'drop')
      
      datatable(brazil_long, options = list(pageLength = 5))
    })
    
#VIS 3
    brazil$richness <- apply(brazil[, !(names(brazil) %in% c("Long", "Lat", "SITES"))], 1, 
                             function(row) sum(row > 0))
    
    output$SPGuildrichness_comparison_plot <- renderPlot({
      ggplot(brazil, aes(x = SITES, y = richness)) +
        geom_point(size = 3, position = position_jitter(width = 0.2)) + 
        labs(title = paste("Species Guild Richness by Site, Highlighted by", input$selected_category),
             x = "Sites",
             y = "Species Richness") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12)
        )
    })
    
    observeEvent(input$show_instructions, {
      showModal(modalDialog(
        title = "show_instructions3",
        "Use the dropdown menu to select the desired feeding guild. The graph displays species guild richness by site.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
#VIS 4
    
    brazil$richness <- apply(brazil[, !(names(brazil) %in% c("Long", "Lat", "SITES"))], 1, function(row) sum(row > 0))
    
    output$feeding_guild_comparison_plot <- renderPlot({
      brazil$SITES <- factor(brazil$SITES, levels = unique(brazil$SITES), ordered = TRUE)
      
      ggplot(brazil, aes(x = SITES, y = !!sym(input$FGselected_category), group = SITES, color = SITES)) +
        geom_line() + 
        geom_point(size = 3) +
        labs(title = paste("Trend of", input$FGselected_category, "across Sites"),
             x = "Sites",
             y = input$FGselected_category) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12)
        ) +
        scale_color_discrete(name = "Sites")
    })
    
    observeEvent(input$show_instructions, {
      showModal(modalDialog(
        title = "show_instructions4",
        "Use the dropdown menu to select the desired feeding guild. The graph displays the trend of the selected guild across sites.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
#VIS 5
    brazil$richness <- rowSums(brazil[!(names(brazil) %in% c("Long", "Lat", "SITES"))])
    
    filtered_data <- reactive({
      long_brazil <- brazil %>%
        pivot_longer(cols = all_of(input$selected_categories), names_to = "category", values_to = "value") %>%
        filter(category %in% input$selected_categories)
      
      return(long_brazil)
    })
    
    output$guild_interaction_comparison_plot <- renderPlot({
      ggplot(filtered_data(), aes(x = SITES, y = value, group = category, color = category)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        labs(title = "Trends of Feeding Guilds across Sites",
             x = "Sites",
             y = "Count") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12)
        ) +
        scale_color_viridis_d(name = "Guilds")  
    })
    
    observeEvent(input$show_instructions, {
      showModal(modalDialog(
        title = "show_instructions5",
        "Use the checkbox menu to select the desired feeding guilds. The graph displays the trends of the selected guilds across sites using a line graph.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
#VIS 6
    output$spcountbysite_comparison_plot <- renderPlot({
      filtered_data <- brazil %>%
        select(SITES, !!sym(input$selected_species))
      
      ggplot(filtered_data, aes(x = SITES, y = !!sym(input$selected_species), group = SITES, color = SITES)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        labs(title = paste("Distribution of", input$selected_species, "Across Sites"),
             x = "Sites",
             y = "Count of Species") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.text.y = element_text(),
          legend.position = "none" 
        )
    })
    
    observeEvent(input$show_instructions, {
      showModal(modalDialog(
        title = "show_instructions6",
        "Use the dropdown menu to select the desired species. The graph displays the trend of the selected species count across sites.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
#VIS 7
    # Update the selectInput choices
    updateSelectInput(session, "selectedSite", choices = unique(brazil$SITES))
    
    calculateProportions <- function(data) {
      proportions <- colSums(data) / sum(colSums(data))
      return(proportions)
    }
    
    observeEvent(input$calcProportions, {
      req(input$selectedSite)  # Ensure a site is selected
      
      site_data <- your_data[your_data$SITES == input$selectedSite, ]
      feeding_guild_data <- site_data[4:12]  # Adjust the indices as per your data structure
      proportions <- calculateProportions(feeding_guild_data)
      
      data_to_plot <- data.frame(FeedingGuild = names(proportions), Proportion = proportions)
      
      output$proportionsPlot <- renderPlot({
        ggplot(data_to_plot, aes(x = "", y = Proportion, fill = FeedingGuild)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +  
          labs(y = "Proportion", x = "", title = "Feeding Guild Proportions at Selected Site") +
          theme_void() +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14),
                plot.title = element_text(size = 20, hjust = 0.5)) +
          scale_fill_brewer(palette = "Pastel1") +
          guides(fill = guide_legend(title = "Feeding Guild"))
      })
      
      output$textOutput <- renderText({
        paste("Feeding Guild Proportions for", input$selectedSite, ":\n",
              paste(names(proportions), "=", percent(proportions), collapse = "\n"))
      })
      observeEvent(input$show_instructions, {
        showModal(modalDialog(
          title = "show_instructions7",
          "Click on Calculate_Feeding_Guild_Proportions tab each time you select a site to update the pie chart",
          easyClose = TRUE,
          footer = NULL
        ))
      })
    })
    
  # Code for Statistical Inferences

#ANOVA
      observeEvent(input$analysisType, {
        if(input$analysisType == "ANOVA Analysis") {
          # Perform ANOVA
          anova_fit <- reactive({
            req(input$selectedHabit)  # Ensure the habit is selected
            aov(Count ~ SITES, data = short_data[short_data$FeedingHabit == input$selectedHabit, ])
          })
          
          # Render the ANOVA summary table
          output$table <- renderTable({
            req(anova_fit())  # Ensure the ANOVA model is fitted
            anova_summary <- summary(anova_fit())
            anova_table <- as.data.frame(anova_summary[[1]])  # Convert the ANOVA table to a data frame
            anova_table
          })
          
          # Render the text output for summary
          output$text <- renderText({
            req(anova_fit())  # Ensure the ANOVA model is fitted
            capture.output(summary(anova_fit()))
          })
          
          # Render the plot
          output$plot <- renderPlot({
            req(short_data)  # Ensure the short_data is available
            ggplot(short_data[short_data$FeedingHabit == input$selectedHabit, ], aes(x = SITES, y = Count)) +
              geom_bar(stat = "summary", fun = "mean", fill = "pink") +
              geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
              labs(y = "Mean Count", title = paste("Mean Count of", input$selectedHabit, "by Site")) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          })
        }
      })
      
#Species GLM
      observe({
        species_choices <- unique(long_data$Species)
        updateSelectInput(session, "selectedSpecies", choices = species_choices)
      })
      
      observeEvent(input$SpeciesAnalysisType, {
        if(input$SpeciesAnalysisType == "GLM Poisson Analysis") {
          species_glm_fit <- reactive({
            glm(Count ~ SITES, family = poisson(link = "log"), data = long_data[long_data$Species == input$selectedSpecies,])
          })
          
          output$SPGLMtext <- renderText({
            species_glm_summary <- summary(species_glm_fit())
            p_value <- coef(species_glm_summary)[,4]  # Extract p-values for the predictors
            output_text <- capture.output(species_glm_summary)
            output_text <- c(output_text, paste("P-values for the predictors:", paste(p_value, collapse = ", ")))
            paste(output_text, collapse = "\n")
          })
        }
      })
      
      # Plot output with different colors for sites
      output$SPGLMplot <- renderPlot({
        req(input$selectedSpecies)  # Ensure that a species is selected
        current_data <- long_data[long_data$Species == input$selectedSpecies,]
        ggplot(current_data, aes(x = SITES, y = Count, fill = SITES)) +
          geom_bar(stat = "summary", fun = "mean") +
          geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
          labs(y = "Mean Count", title = paste("Mean Count of", input$selectedSpecies, "by Site")) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      # Table output
      output$SPGLMtable <- renderTable({
        req(input$selectedSpecies)  # Ensure that a species is selected
        if(input$analysisType == "GLM Poisson Analysis") {
          tryCatch({
            species_glm_summary <- summary(species_glm_fit())
            anova_table <- as.data.frame(species_glm_summary$coefficients)  # Convert the coefficients table to a data frame
            anova_table$P_Value <- coef(species_glm_summary)[,4]  # Add the p-values
            anova_table
          }, error = function(e) {
            # Return a data frame with the error message
            return(data.frame(Error = as.character(e$message)))
          })
        }
      })
       
      
#Guild GLM

# Update habit choices based on the loaded data
observe({
  habit_choices <- unique(short_data$FeedingHabit)
  updateSelectInput(session, "selectedFeedingHabit", choices = habit_choices)
})

observeEvent(input$GuildAnalysisType, {
# GLM Poisson Analysis
guild_glm_fit <- reactive({
  req(input$selectedFeedingHabit)
  glm(Count ~ SITES, family = poisson(link = "log"), data = short_data[short_data$FeedingHabit == input$selectedFeedingHabit,])
})

# Display GLM summary text
output$GuildSptext <- renderText({
  req(guild_glm_fit())
  glm_summary <- summary(guild_glm_fit())
  p_value <- coef(glm_summary)[,4]
  output_text <- capture.output(glm_summary)
  output_text <- c(output_text, paste("P-values for the predictors:", paste(p_value, collapse = ", ")))
  paste(output_text, collapse = "\n")
})

# Plot output for GLM Poisson Analysis
output$GuildSpplot <- renderPlot({
  req(input$selectedFeedingHabit)
  current_data <- short_data[short_data$FeedingHabit == input$selectedFeedingHabit,]
  ggplot(current_data, aes(x = SITES, y = Count, fill = SITES)) +
    geom_bar(stat = "summary", fun = "mean") +
    geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.2) +
    labs(y = "Mean Count", title = paste("Mean Count of", input$selectedFeedingHabit, "by Site")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Table output for GLM

output$GuildSptable <- renderTable({
  req(guild_glm_fit())
  guild_glm_summary <- summary(guild_glm_fit())
  anova_table <- as.data.frame(guild_glm_summary$coefficients)
  anova_table
})
})
}
# Run the application
shinyApp(ui, server)

      menuItem("Maps", tabName = "maps", icon = icon("globe")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Statistical Inferences", tabName = "stats", icon = icon("table"))

      #Map tab 1
                tabPanel("Map 1",
              titlePanel("Map Interaction: Forage by Site"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("colorBy", "Choose a category to color by:", 
                              choices = c("Carnivore" = "carnivore", 
                                          "Detritivore" = "detritivore", 
                                          "Frugivorous" = "frugivorous",
                                          "Granivore" = "granivore",
                                          "Herbivore" = "herbivore",
                                          "Insectivorous" = "insectivorous",
                                          "Nectarivore" = "nectarivore",
                                          "Omnivorous" = "omnivorous",
                                          "Piscivorous" = "piscivorous")),
                  tags$hr(),
                  tags$p("Navigation Instructions"),
                  tags$p("Select a feeding guild category from the dropdown menu then use your mouse to pan and zoom on the map. Click the circles to see detailed information about each site.")
                ),
                mainPanel(
                  leafletOutput("map")
                )
              )
      ),
      #Map tab 2
      tabPanel("Map 2",
               titlePanel("Map Interaction: Species by Site"),
              fillPage(
                leafletOutput("map2", width = "100%", height = "100vh"),
                absolutePanel(top = 10, right = 10, class = "btn-instructions",
                              actionButton("show_instructions_map2", "Show Instructions"))
              )
      ),
      
      #Map 3
      tabPanel("Map 3",
               titlePanel("Map Interaction: Species Richness by Site"),
              fillPage(
                leafletOutput("map3", width = "100%", height = "100vh"),
                absolutePanel(top = 10, right = 10, 
                              actionButton("show_instructions_map3", "Show Instructions"))
              )
      )
      )
      ),
      # Visualizations Tab
                         
        tabItem(tabName = "visualizations",
              fluidPage(
                tabsetPanel(
                  tabPanel("Visual 1",
                useShinyjs(),  
                titlePanel("Seasonal Species Abundance & Richness"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("group_var", "Choose Groups:", choices = unique(brazil_2$Com_Name), multiple = TRUE),
                    
                    div(id = "instruction_text",
                        h4("Instructions"),
                        p("Use the dropdown menu to select multiple species groups."),
                        p("The bar plot will display the species richness for each selected group across different years."),
                        p("The table will compare the counts of individuals across years for the selected species groups."),
                        style = "display: none; padding: 10px; background-color: #f7f7f7; border-radius: 8px; box-shadow: 2px 2px 5px #666666;"
                    )
                  ),
                  mainPanel(
                    plotOutput("seasonalrichness_plot"),
                    DTOutput("seasonalrichness_table"),
                    tags$hr(),
                    tags$p("Navigation Instructions"),
                    tags$p("The bar plot will display the species richness for each selected group across different years. The table will compare the counts of individuals across years for the selected species groups.")
                  )
                )
              ),
      
      tabPanel("Visual 2",
                titlePanel("Richness Against Forage by Site"),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput("selected_guilds", "Select Feeding Guilds:",
                                       choices = c("piscivorous", "omnivorous", "nectarivore", "insectivorous", 
                                                   "herbivore", "granivore", "frugivorous", "detritivore", "carnivore"),
                                       selected = "piscivorous")
                  ),
                  mainPanel(
                    plotOutput("richness_comparison_plot"),
                    DTOutput("richness_table")
                  )
                )
              ),
      
      tabPanel("Visual 3",
      titlePanel("Species and Feeding Guild Richness by Site"),
      selectInput("selected_category", "Select Category:",
                  choices = c("piscivorous", "omnivorous", "nectarivore", 
                              "insectivorous", "herbivore", "granivore", 
                              "frugivorous", "detritivore", "carnivore"),
                  selected = "piscivorous"),
      plotOutput("SPGuildrichness_comparison_plot"),
      tags$hr(),
      tags$p("Navigation Instructions"),
      tags$p("Use the dropdown menu to select the desired feeding guild. The graph displays species guild richness by site.")
                ),
      
      tabPanel("Visual 4",
               titlePanel("Feeding Guild by Site"),
      selectInput("FGselected_category", "Feeding Guilds:",
                  choices = c("piscivorous", "omnivorous", "nectarivore", "insectivorous", 
                              "herbivore", "granivore", "frugivorous", "detritivore", "carnivore"),
                  selected = "piscivorous"),
      
      plotOutput("feeding_guild_comparison_plot"),
      tags$hr(),
      tags$p("Navigation Instructions"),
      tags$p("Use the dropdown menu to select the desired feeding guild. The graph displays the trend of the selected guild across sites.")
                ),
      
      tabPanel("Visual 5",
      titlePanel("Guild Interaction by Sites"),
      checkboxGroupInput("selected_categories", "Feeding Guilds:",
                         choices = c("piscivorous", "omnivorous", "nectarivore", "insectivorous", 
                                     "herbivore", "granivore", "frugivorous", "detritivore", "carnivore"),
                         selected = "piscivorous"),
      plotOutput("guild_interaction_comparison_plot"),
      tags$hr(),
      tags$p("Navigation Instructions"),
      tags$p("Use the checkbox menu to select the desired feeding guilds. The graph displays the trends of the selected guilds across sites using a line graph.")
              ),
      
      
      tabPanel("Visual 6",
                titlePanel("Species Distribution Across Sites"),
                selectInput("selected_species", "Select Species:",
                            choices = speciesNames,
                            selected = speciesNames[1]),
                plotOutput("spcountbysite_comparison_plot"),
               tags$hr(),
               tags$p("Navigation Instructions"),
               tags$p("Use the dropdown menu to select the desired species. The graph displays the trend of the selected species count across sites.")
               
      
      ),
      
      tabPanel("Visual 7",
               titlePanel("Feed Guild Observations Analysis by Site"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("selectedSite", "Select Site:", choices = NULL),  # Choices set in server
                   actionButton("calcProportions", "Calculate Feeding Guild Proportions")
                 ),
                 mainPanel(
                   plotOutput("proportionsPlot", width = "100%", height = "600px"),  # Adjusted width and height
                   verbatimTextOutput("textOutput"),
                   tags$hr(),
                   tags$p("Navigation Instructions"),
                   tags$p("Click on Calculate_Feeding_Guild_Proportions tab each time you select a site to update the pie chart")
        ))),
      )
              )
        ),
      
# Statistical Inferences Tab
      tabItem(tabName = "stats",
              fluidPage(
              tabsetPanel(
                tabPanel("Temporal Trends", 
                titlePanel("Aquiduana Species Time Series Plot"),
                plotOutput("timeSeriesPlot")),
                
              tabPanel("ANOVA",
                titlePanel("Field Guild Observations Analysis by Site"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("analysisType", "Select Analysis Type:", 
                                choices = c("ANOVA Analysis")),
                    selectInput("selectedHabit", "Select Feeding Habit for ANOVA:", 
                                choices = names(your_data)[4:12])
                  ),
                  mainPanel(
                    plotOutput("plot"),
                    verbatimTextOutput("text"),
                    tableOutput("table")  # Display the ANOVA table
                  )
              )
      ),
      
      tabPanel("Species by Site GLM",
               titlePanel("Species Observations Analysis by Site"),
               
               sidebarLayout(
                 sidebarPanel(
                   selectInput("analysisType", "Select Analysis Type:", 
                               choices = c("GLM Poisson Analysis")),
                   selectInput("selectedSpecies", "Select Species for Analysis:", 
                               choices = NULL)  # Initialize with no species options
                 ),
                 mainPanel(
                   plotOutput("SPGLMplot"),
                   verbatimTextOutput("SPGLMtext"),
                   tableOutput("SPGLMtable")  # Add this line for table display
                 )
               )
      ),
      
      tabPanel("Guild & Species GLM",
               titlePanel("Feeding Habits Analysis by Site"),
               
               sidebarLayout(
                 sidebarPanel(
                   selectInput("analysisType", "Select Analysis Type:", 
                               choices = c("GLM Poisson Analysis")),
                   selectInput("selectedFeedingHabit", "Select Feeding Habit for Analysis:", 
                               choices = NULL)  # Initialize with no habit options
                 ),
                 mainPanel(
                   plotOutput("GUildSpGLMplot"),
                   verbatimTextOutput("GuildSpGLMtext"),
                   tableOutput("GuildSpGLMtable")  # Add this line for table display
                 )
    )
  ),
))
))
)
)

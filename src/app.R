options(shiny.port = 8050, shiny.autoreload = TRUE)


library(shiny)
library(here)
library(readr)
library(dplyr)
library(arrow)
library(shinyjs)
library(geojsonio)
library(ggplot2)
library(spdplyr)
library(tidyr)


### Layout
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = 'litera'),
  h1("2020 Mexico Census"),


  useShinyjs(), # Disable dropdowns until updated
  

  # State dropdown
  selectInput(
    "state_dropdown",
    "Select a state",
    choices = c(),
    selected = NULL,
  ),
  
  # Municipality dropdown
  selectInput(
    "municipality_dropdown",
    "Select a municipality",
    choices = c(),  
    selected = NULL
  ),
  
  # Locality dropdown
  selectInput(
    "locality_dropdown",
    "Select a locality",
    choices = c(),
    selected = NULL
  ),
  
  # Plot area
  plotOutput("map_plot"),
  
  # Population Pyramid
  plotOutput("population_pyramid"),
  
  br()

)





### Callbacks
server <- function(input, output, session) {
  
  # Import states names
  states <- reactive({
    file_path <- here("data", "processed", "entity_names.csv")
    if (file.exists(file_path)) {
      state_data <- read_csv(file_path)
      state_data$NOM_ENT
    } else {
      character(0)
    }
  })
  
  # Import census data
  census_dataset <- reactive({
    open_dataset(here("data", "processed", "parquet_data_coords")) |> 
      collect()
  })
  
  # Import geographic information
  geojson_file <- reactive({
    geojsonio::geojson_read(here("data", "processed", "mexico.geojson"), what = "sp")
  })
  
  # Filtering census data by current state
  filtered_data <- reactive({
    req(input$state_dropdown)
    filter_df <- filter(census_dataset(), NOM_ENT == input$state_dropdown)
    return(filter_df)
  })
  
  # Load states to state dropdown
  observe({
    updateSelectInput(session, "state_dropdown", choices = states())
  })
  
  # Load Municipalities to municipalities dropdown
  observe({
    req(input$state_dropdown)
        # Disable dropdowns
        shinyjs::disable("state_dropdown")
        shinyjs::disable("municipality_dropdown")
        shinyjs::disable("locality_dropdown")
    updateSelectInput(session, "municipality_dropdown", choices = unique(filtered_data()$NOM_MUN))
        # Enable dropdowns
        shinyjs::enable("state_dropdown")
        shinyjs::enable("municipality_dropdown")
        shinyjs::enable("locality_dropdown")
  })
  
  # Update localities dropdown based on selected state and municipality
  observeEvent(c(input$state_dropdown, input$municipality_dropdown), {
    req(input$state_dropdown, input$municipality_dropdown)
        # Disable dropdowns
        shinyjs::disable("state_dropdown")
        shinyjs::disable("municipality_dropdown")
        shinyjs::disable("locality_dropdown")
    filter_df <- filter(census_dataset(), NOM_ENT == input$state_dropdown, NOM_MUN == input$municipality_dropdown)
    updateSelectInput(session, "locality_dropdown", choices = unique(filter_df$NOM_LOC))
        # Enable dropdowns
        shinyjs::enable("state_dropdown")
        shinyjs::enable("municipality_dropdown")
        shinyjs::enable("locality_dropdown")
  })
  
  # Plot map
  output$map_plot <- renderPlot({
    req(input$state_dropdown)
    
    # Initial plot setup
    gg <- ggplot() +
      theme_void() +
      coord_map()
    
    if (input$state_dropdown == "Total nacional") {
      filtered_df <- geojson_file()
    } else {
      filtered_df <- geojson_file() |> 
        filter(name == input$state_dropdown)
    }
    
    gg <- gg + geom_polygon(data = filtered_df,
                            aes(x = long, y = lat, group = group),
                            fill = "lightgray", color = "white")
    
    # Check if locality dropdown is selected
    if (!is.null(input$locality_dropdown) && input$locality_dropdown != "") {
      coord_df <- filter(census_dataset(), 
                         NOM_ENT == input$state_dropdown,
                         NOM_MUN == input$municipality_dropdown,
                         NOM_LOC == input$locality_dropdown)
      
      # Filter out invalid coordinates and extract the first row
      if (nrow(coord_df) > 0 && !any(is.na(coord_df$longitude_decimal)) && !any(is.na(coord_df$latitude_decimal))) {
        coordinates <- tibble(
          long = coord_df$longitude_decimal[1],
          lat = coord_df$latitude_decimal[1]
        )
        
        gg <- gg + geom_point(data = coordinates, aes(x = long, y = lat), color = "red", size = 3)
      }
    }
    
    print(gg)
  })
  
  # Plot pyramid
  output$population_pyramid <- renderPlot({
    req(input$state_dropdown)
    
    filter_df <- filter(
      census_dataset(),
      NOM_ENT == input$state_dropdown,
      NOM_MUN == input$municipality_dropdown,
      NOM_LOC == input$locality_dropdown
    )
    
    # Cohorts for pyramid plot
    cohort_names_m <- c(
      "P_0A4_M", "P_5A9_M", "P_10A14_M", "P_15A19_M", "P_20A24_M",
      "P_25A29_M", "P_30A34_M", "P_35A39_M", "P_40A44_M", "P_45A49_M",
      "P_50A54_M", "P_55A59_M", "P_60A64_M", "P_65A69_M", "P_70A74_M",
      "P_75A79_M", "P_80A84_M", "P_85YMAS_M"
    )
    
    cohort_names_f <- c(
      "P_0A4_F", "P_5A9_F", "P_10A14_F", "P_15A19_F", "P_20A24_F",
      "P_25A29_F", "P_30A34_F", "P_35A39_F", "P_40A44_F", "P_45A49_F",
      "P_50A54_F", "P_55A59_F", "P_60A64_F", "P_65A69_F", "P_70A74_F",
      "P_75A79_F", "P_80A84_F", "P_85YMAS_F"
    )
    
    # New common cohort name
    new_ages <- c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", 
                  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                  "65-69", "70-74", "75-79", "80-84", "85+")
    
    # Convert info into a tibble
    population_data <- tibble(
      Age = paste0(new_ages),
      Male = as.numeric(filter_df[1, cohort_names_m]),
      Female = as.numeric(filter_df[1, cohort_names_f])
    )
    
    # Pivot Longer
    population_long <- pivot_longer(
      population_data,
      cols = c(Male, Female),
      names_to = "Sex",
      values_to = "Population"
    )
    
    gg <- ggplot(population_long, aes(x = Age, y = ifelse(Sex == "Male", -Population, Population), fill = Sex)) +
          geom_bar(stat = "identity") +
          scale_y_continuous(labels = abs, limits = c(-1, 1) * max(population_long$Population)) +
          coord_flip() +
          theme_minimal() +
          labs(x = "Age", y = "Population", fill = "Sex", title = "Population Pyramid")
    
    print(gg)
  
    
  })
  
  
}

# Run the app/dashboard
shinyApp(ui, server)

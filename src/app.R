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
}

# Run the app/dashboard
shinyApp(ui, server)

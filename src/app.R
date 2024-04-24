options(shiny.port = 8050, shiny.autoreload = TRUE)


library(shiny)
library(here)
library(dplyr)
library(arrow)
library(shinyjs)
library(shinycssloaders) 


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
    # Disable dropdowns
    shinyjs::disable("state_dropdown")
    shinyjs::disable("municipality_dropdown")
    
    # Waiting animation
    # showSpinner(spin = "foldingCube", color = "#337ab7")
    
    updateSelectInput(session, "municipality_dropdown", choices = unique(filtered_data()$NOM_MUN))
    
    # Enable dropdowns
    shinyjs::enable("state_dropdown")
    shinyjs::enable("municipality_dropdown")
    
    # Hide Waiting animation
    # hideSpinner()
    
  })
  
  
  
  
}


### Run the app/dashboard
shinyApp(ui, server)

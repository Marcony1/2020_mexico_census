options(shiny.port = 8050, shiny.autoreload = TRUE)


library(shiny)
library(here)
library(dplyr)




# Layout
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = 'litera'),
  h1("2020 Mexico Census"),

  selectInput(
    "state_dropdown",
    "Select a state",
    choices = c("New York", "Montreal", "San Fransico"),
    selected = "Montreal",
  ),
  br()

)

# Callbacks
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
  
  # Load states to state dropdown
  observe({
    updateSelectInput(session, "state_dropdown", choices = states())
  })
  
}

# Run the app/dashboard
shinyApp(ui, server)

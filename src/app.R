options(shiny.port = 8050, shiny.autoreload = TRUE)


library(shiny)



# Layout
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = 'litera'),
  h1("2020 Mexico Census"),
  p('Shiny converts R classes into HTML'),
  p("This conversion happens behind the scenes by Dash's JavaScript front-end"),
)

# Server side callbacks/reactivity
server <- function(input, output, session) {}

# Run the app/dashboard
shinyApp(ui, server)

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
library(shinydashboard)
library(httr)


### Define UI
ui <- dashboardPage(
  dashboardHeader(title = "2020 Mexico Census"),
  dashboardSidebar(
    useShinyjs(),
    selectInput("state_dropdown", "Select a state", choices = c(), selected = NULL),
    selectInput("municipality_dropdown", "Select a municipality", choices = c(), selected = NULL),
    selectInput("locality_dropdown", "Select a locality", choices = c(), selected = NULL)
  ),
  dashboardBody(
    fluidRow(
      box(
        title = tags$h3("Map"),
        status = "primary",
        plotOutput("map_plot")
      ),
      box(
        title = tags$h3("Population Pyramid"),
        status = "primary",
        plotOutput("population_pyramid")
      )
    ),
    fluidRow(
      box(
        title = tags$h3("Average number of children born alive"),
        status = "primary",
        textOutput("children_born_alive"),
        style = "font-size: 16px;"
      ),
      box(
        title = tags$h3("Total Population"),
        status = "primary",
        textOutput("population_card"),
        style = "font-size: 16px;"
      )
    ),
    fluidRow(
      box(
        title = tags$h3("Births in entity vs. births in another entity"),
        status = "primary",
        plotOutput("birth_origin")
      ),
      box(
        title = tags$h3("Men to Women Ratio"),
        status = "primary",
        plotOutput("men_women_ratio_pie")
      )
    )
  )
)


### Define server logic
server <- function(input, output, session) {
  
  # Import states names
  states <- reactive({
    file_path <- "https://raw.github.ubc.ca/MDS-2023-24/DSCI_532_individual-assignment_marcony1/master/data/processed/entity_names.csv?token=GHSAT0AAAAAAAAACLO4KRTK56KAAYK3ZMGQZRJHJBA"
    # file_path <- here("data", "processed", "entity_names.csv")
      state_data <- read_csv(file_path)
      state_data$NOM_ENT

  })
  
  # Import census data
  census_dataset <- reactive({
    open_dataset(here("data", "processed", "parquet_data_coords")) |>
      collect()
    # url <- "https://github.ubc.ca/MDS-2023-24/DSCI_532_individual-assignment_marcony1/raw/master/data/processed/parquet_data_coords"
    # open_dataset(url) |> collect()
  })
  
    # Import geographic information
    geojson_file <- reactive({
      # geojsonio::geojson_read(here("data", "processed", "mexico.geojson"), what = "sp")
      geojsonio::geojson_read("https://raw.github.ubc.ca/MDS-2023-24/DSCI_532_individual-assignment_marcony1/master/data/processed/mexico.geojson?token=GHSAT0AAAAAAAAACLO4PRPXDBZULOOUAMO4ZRJHTEA", what = "sp")
          })
  
  # Filtering census data by current state
  filtered_data <- reactive({
    req(input$state_dropdown)
    filter_df <- filter(census_dataset(), NOM_ENT == input$state_dropdown)
    return(filter_df)
  })
  
  # Load states to state dropdown
  observe({
    shinyjs::disable("state_dropdown")
    shinyjs::disable("municipality_dropdown")
    shinyjs::disable("locality_dropdown")
    updateSelectInput(session, "state_dropdown", choices = states())
    shinyjs::enable("state_dropdown")
    shinyjs::enable("municipality_dropdown")
    shinyjs::enable("locality_dropdown")
  })
  
  # Load Municipalities to municipalities dropdown
  observe({
    req(input$state_dropdown)
    shinyjs::disable("state_dropdown")
    shinyjs::disable("municipality_dropdown")
    shinyjs::disable("locality_dropdown")
    updateSelectInput(session, "municipality_dropdown", choices = unique(filtered_data()$NOM_MUN))
    shinyjs::enable("state_dropdown")
    shinyjs::enable("municipality_dropdown")
    shinyjs::enable("locality_dropdown")
  })
  
  # Update localities dropdown based on selected state and municipality
  observeEvent(c(input$state_dropdown, input$municipality_dropdown), {
    req(input$state_dropdown, input$municipality_dropdown)
    shinyjs::disable("state_dropdown")
    shinyjs::disable("municipality_dropdown")
    shinyjs::disable("locality_dropdown")
    filter_df <- filter(census_dataset(), NOM_ENT == input$state_dropdown, NOM_MUN == input$municipality_dropdown)
    updateSelectInput(session, "locality_dropdown", choices = unique(filter_df$NOM_LOC))
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
                              fill = "#00BFC4", color = "white") +
                            labs(title = "Population Distribution Map", x = "Longitude", y = "Latitude") +
                            theme_minimal() +
                            theme(axis.title = element_text(size = 12),
                                  axis.text = element_text(size = 10),
                                  legend.title = element_text(size = 12),
                                  legend.text = element_text(size = 10))

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
  
  # Render population pyramid
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
    
    if (any(is.na(population_data$Male)) || any(is.na(population_data$Female))) {
      message <- "Not Available"
      gg <- ggplot() + geom_text(aes(x = 0, y = 0, label = message), size = 10)
    } else {
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
        labs(x = "Age", y = "Population", fill = "Sex") +
        theme(
          text = element_text(size = 12),  
          axis.title = element_text(size = 14),  
          legend.title = element_text(size = 14),  
          legend.text = element_text(size = 12),  
          axis.text.x = element_text(size = 10),  
          axis.text.y = element_text(size = 10)   
        ) +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))  
      
    }
    
    print(gg)
  })
  
  # Render population card
  output$population_card <- renderText({
    req(input$state_dropdown, input$municipality_dropdown, input$locality_dropdown)
    filter_df <- filter(
      census_dataset(),
      NOM_ENT == input$state_dropdown,
      NOM_MUN == input$municipality_dropdown,
      NOM_LOC == input$locality_dropdown
    )
    
    total_population <- as.numeric(filter_df[1, "POBTOT"], na.rm = TRUE)
    
    if (is.na(total_population)) {
      "Not Available"
    } else {
      comma_format <- format(total_population, big.mark = ",")
      return(comma_format)
    }
  })
  
  # Render children born alive card
  output$children_born_alive <- renderText({
    req(input$state_dropdown, input$municipality_dropdown, input$locality_dropdown)
    filter_df <- filter(
      census_dataset(),
      NOM_ENT == input$state_dropdown,
      NOM_MUN == input$municipality_dropdown,
      NOM_LOC == input$locality_dropdown
    )
    
    total_population <- as.numeric(filter_df[1, "PROM_HNV"], na.rm = TRUE)
    
    if (is.na(total_population)) {
      "Not Available"
    } else {
      comma_format <- format(total_population, big.mark = ",")
      return(comma_format)
    }
  })
  
  # Render men to women ratio pie chart
  output$men_women_ratio_pie <- renderPlot({
    req(input$state_dropdown, input$municipality_dropdown, input$locality_dropdown)
    filter_df <- filter(
      census_dataset(),
      NOM_ENT == input$state_dropdown,
      NOM_MUN == input$municipality_dropdown,
      NOM_LOC == input$locality_dropdown
    )
    
    men_to_women_ratio <- as.numeric(filter_df[1, "REL_H_M"])
    women_to_men_ratio <- 10000 / men_to_women_ratio
    
    ratio_df <- tibble(
      Category = c("Men", "Women"),
      Ratio = c(men_to_women_ratio, women_to_men_ratio)
    )
    
    # Calculate percentages
    ratio_df$Percentage <- ratio_df$Ratio / sum(ratio_df$Ratio) * 100
    
    
    if (any(is.na(men_to_women_ratio)) || any(is.na(women_to_men_ratio))) {
      message <- "Not Available"
      gg <- ggplot() + geom_text(aes(x = 0, y = 0, label = message), size = 10)
    } else {
    # Plot the pie chart
      title <- paste("Men-to-Women Ratio:", round(men_to_women_ratio, 2))
      gg <- ggplot(ratio_df, aes(x = "", y = Ratio, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        theme_void() +
        theme(legend.position = "bottom") +
        scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
        geom_text(aes(label = paste0(round(Percentage), "%")), 
                  position = position_stack(vjust = 0.5),
                  size = 7, color = "white", fontface = "bold") +
        ggtitle(title) + 
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 14),  
          legend.text = element_text(size = 12)
        )
    
    }
    
    print(gg)
  })
  
  # Render birth origin grouped bar chart
  output$birth_origin <- renderPlot({
    req(input$state_dropdown, input$municipality_dropdown, input$locality_dropdown)
    
    filter_df <- filter(
      census_dataset(),
      NOM_ENT == input$state_dropdown,
      NOM_MUN == input$municipality_dropdown,
      NOM_LOC == input$locality_dropdown
    )
    
    birth_local <- as.numeric(filter_df[1, "PNACENT"])
    birth_another <- as.numeric(filter_df[1, "PNACOE"])
    
    if (is.na(birth_local) || is.na(birth_another)) {
      message <- "Data Not Available"
      gg <- ggplot() +
        geom_text(aes(x = 0, y = 0, label = message), size = 10)
    } else {
      ratio_df <- tibble(
        Category = c("Local", "Other"),
        Count = c(birth_local, birth_another)
      )
      
      ratio_df <- ratio_df |> 
        mutate(Percentage = (Count / sum(Count)) * 100)
      
      gg <- ggplot(ratio_df, aes(x = Category, y = Count, fill = Category)) +
        geom_bar(stat = "identity", width = 0.5) +
        theme_minimal() +
        labs(x = "Birth Origin", y = "Count", fill = "Category") +
        geom_text(aes(label = paste0(round(Percentage), "%")), 
                  position = position_stack(vjust = 0.5),
                  size = 7, color = "white", fontface = "bold") +
        theme(
          text = element_text(size = 12),  
          axis.title = element_text(size = 14),  
          legend.title = element_text(size = 14),  
          legend.text = element_text(size = 12),  
          axis.text.x = element_text(size = 10),  
          axis.text.y = element_text(size = 10),  
          axis.text = element_text(size = 10, color = "black"),  
          axis.line = element_line(color = "black"),  
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank(),  
          plot.title = element_text(size = 16, face = "bold"),  
          plot.subtitle = element_text(size = 14) 
        ) +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    }
    
    print(gg)
  })
}

# Run the application
shinyApp(ui, server)


library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(DT)

# API querying functions
get_vehicle_record <- function(vehicle_id) {
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/", vehicle_id)
  response <- GET(url, accept("application/json"))
  content <- fromJSON(content(response, "text"), flatten = TRUE)
  return(as_tibble(content))
}

get_emission_records <- function(vehicle_id) {
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/emissions/", vehicle_id)
  response <- GET(url, accept("application/json"))
  content <- fromJSON(content(response, "text"), flatten = TRUE)
  return(as_tibble(content))
}

get_fuel_prices <- function() {
  url <- "https://www.fueleconomy.gov/ws/rest/fuelprices"
  response <- GET(url, accept("application/json"))
  content <- fromJSON(content(response, "text"), flatten = TRUE)
  return(as_tibble(content))
}

get_shared_mpg_summary <- function(vehicle_id) {
  url <- paste0("https://www.fueleconomy.gov/ws/rest/ympg/shared/ympgVehicle/", vehicle_id)
  response <- GET(url, accept("application/json"))
  content <- fromJSON(content(response, "text"), flatten = TRUE)
  return(as_tibble(content))
}

get_user_mpg_records <- function(vehicle_id) {
  url <- paste0("https://www.fueleconomy.gov/ws/rest/ympg/shared/ympgDriverVehicle/", vehicle_id)
  response <- GET(url, accept("application/json"))
  content <- fromJSON(content(response, "text"), flatten = TRUE)
  return(as_tibble(content))
}

get_vehicle_menu_options <- function(year, make, model) {
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/menu/options?year=", year, "&make=", make, "&model=", model)
  response <- GET(url, accept("application/json"))
  content <- fromJSON(content(response, "text"), flatten = TRUE)
  return(as_tibble(content))
}

# Server
server <- function(input, output, session) {
  
  # Reactive values to store data
  data <- reactiveVal()
  
  # Download data
  observeEvent(input$download_data, {
    vehicle_id <- input$vehicle_id
    vehicle_data <- get_vehicle_record(vehicle_id)
    data(vehicle_data)
    output$data_table <- renderDT({
      datatable(vehicle_data)
    })
  })
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() { paste("vehicle_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  # Update plot and summary variable choices
  observe({
    if (!is.null(data())) {
      updateSelectInput(session, "plot_var", choices = names(data()))
      updateSelectInput(session, "summary_var", choices = names(data()))
    }
  })
  
  # Plot data
  observeEvent(input$plot_data, {
    req(input$plot_var, input$plot_type)
    plot_var <- input$plot_var
    plot_type <- input$plot_type
    
    output$plot <- renderPlot({
      ggplot(data(), aes_string(x = plot_var)) +
        {
          if (plot_type == "Histogram") geom_histogram() else
            if (plot_type == "Boxplot") geom_boxplot() else
              if (plot_type == "Scatterplot") geom_point(aes_string(y = input$summary_var))
        } +
        labs(title = paste(plot_type, "of", plot_var), x = plot_var, y = input$summary_var)
    })
  })
  
  # Contingency table
  output$contingency_table <- renderDT({
    req(input$plot_var, input$summary_var)
    contingency_table <- table(data()[[input$plot_var]], data()[[input$summary_var]])
    datatable(as.data.frame(contingency_table))
  })
  
  # Numerical summary
  output$summary <- renderPrint({
    req(input$plot_var, input$summary_var)
    summary_data <- data() %>%
      group_by_at(input$plot_var) %>%
      summarise(mean = mean(.data[[input$summary_var]], na.rm = TRUE),
                sd = sd(.data[[input$summary_var]], na.rm = TRUE))
    print(summary_data)
  })
}

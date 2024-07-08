library(shiny)
library(httr)
library(jsonlite)
library(dplyr)

# Function to retrieve and process vehicle record data
vehicle_record <- function(vehicle_id) {
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/", vehicle_id)
  api_info <- httr::GET(url)
  api_char <- base::rawToChar(api_info$content)
  parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
  vehicle_data <- as_tibble(parsed)
  selected_data <- vehicle_data %>%
    select(barrels08, barrelsA08, city08, cityA08, cylinders, engId, fuelCost08, fuelCostA08, fuelType1, fuelType2, highway08, highwayA08U, lv2, lv4, year, model, youSaveSpend) %>%
    arrange(year)
  return(selected_data)
}

# Function to retrieve emission records for a vehicle
emission_records <- function(vehicle_id) {
  if (typeof(vehicle_id) == "character") {
    stop("ERROR: Please provide a vehicle ID as a number, not a character string.")
  }
  if (!is.numeric(vehicle_id) || vehicle_id %% 1 != 0) {
    stop("ERROR: Please provide a valid vehicle ID as an integer.")
  }
  vehicle_data <- vehicle_record(vehicle_id)
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/emissions/", vehicle_id)
  api_info <- httr::GET(url)
  api_char <- base::rawToChar(api_info$content)
  parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
  emission_data <- as_tibble(parsed$emissionsInfo) %>% select(-efid)
  return(emission_data)
}

# Function to retrieve fuel prices by fuel type
get_fuel_prices <- function(fuel_type = NULL) {
  if (is.null(fuel_type)) {
    stop("ERROR: Please provide a fuel type (e.g., 'Regular', 'Midgrade', 'Premium', etc.).")
  }
  url <- "https://www.fueleconomy.gov/ws/rest/fuelprices"
  api_info <- httr::GET(url)
  api_char <- base::rawToChar(api_info$content)
  parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
  if (!(fuel_type %in% names(parsed))) {
    stop(paste("ERROR: Fuel type", fuel_type, "not found in fuel prices data. Available types are:", paste(names(parsed), collapse = ", ")))
  }
  fuel_prices <- as_tibble(parsed[[fuel_type]])
  return(fuel_prices)
}

# Function to retrieve vehicle options for a specific year, make, and model
get_vehicle_options <- function(year, make, model) {
  if (!is.numeric(year) || nchar(year) != 4) {
    stop("ERROR: Please provide a valid 4-digit year.")
  }
  if (!is.character(make)) {
    stop("ERROR: Please provide the make as a character string.")
  }
  if (!is.character(model)) {
    stop("ERROR: Please provide the model as a character string.")
  }
  make <- URLencode(make)
  model <- URLencode(model)
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/menu/options?year=", year, "&make=", make, "&model=", model)
  tryCatch({
    api_info <- httr::GET(url)
    api_char <- base::rawToChar(api_info$content)
    parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
    vehicle_options <- as_tibble(parsed$menuItem)
    colnames(vehicle_options) <- c("Engine info", "Vehicle record ID")
    return(vehicle_options)
  }, error = function(e) {
    message("Error: ", e$message)
  })
}

# Define server logic
function(input, output, session) {
  
  # Dynamically create UI elements based on selected function in Data Download tab
  output$year_input <- renderUI({
    selectInput("year", "Year:", choices = 1984:2024, selected = 2020)
  })
  
  output$make_input <- renderUI({
    textInput("make", "Make:")
  })
  
  output$model_input <- renderUI({
    textInput("model", "Model:")
  })
  
  # Handle data download based on user selection
  observeEvent(input$submit_download, {
    data <- switch(input$download_function,
                   "Vehicle Record" = {
                     req(input$year, input$make, input$model)
                     vehicle_record(input$year, input$make, input$model)
                   },
                   "Emission Records" = {
                     req(input$year, input$make, input$model)
                     emission_records(input$year, input$make, input$model)
                   },
                   "Fuel Prices" = {
                     req(input$fuel_type)
                     get_fuel_prices(input$fuel_type)
                   },
                   "Vehicle Options" = {
                     req(input$year, input$make, input$model)
                     get_vehicle_options(input$year, input$make, input$model)
                   })
    
    output$downloaded_data <- renderTable({
      head(data)
    })
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste("downloaded_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
  
  # Placeholder for data exploration functionality (to be implemented)
  observeEvent(input$explore_function, {
    # Placeholder for data exploration plots based on selected function
    output$exploration_plot <- renderPlot({
      plot(1:10, type = "l", col = "blue")
    })
  })
}

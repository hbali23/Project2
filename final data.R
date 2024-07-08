library(httr)
library(jsonlite)
library(tidyverse)

## This function retrieves and processes data for a gas vehicle from the FuelEconomy.gov API, selecting specific columns and arranging the data by year.
vehicle_record <- function(vehicle_id) {
  # Construct the URL
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/", vehicle_id)
  # Make the GET request
  api_info <- httr::GET(url)
  # Convert the raw content to character
  api_char <- base::rawToChar(api_info$content)
  # Parse the JSON content
  parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
  # Print the structure of the parsed data
  str(parsed, max.level = 1)
  # Convert the parsed data to a tibble
  vehicle_data <- as_tibble(parsed)
  # Select the desired columns and arrange by year
  selected_data <- vehicle_data %>%
    select(barrels08, barrelsA08, city08, cityA08, cylinders, engId, fuelCost08, fuelCostA08, fuelType1, fuelType2, highway08, highwayA08U, lv2, lv4, year, model, youSaveSpend) %>%
    arrange(year)
  return(selected_data)
}



## Function to get emission records by modifying the "vehicle_id" endpoint
emission_records <- function(vehicle_id) {
  # Check if the input is a character
  if (typeof(vehicle_id) == "character") {
    stop("ERROR: Please provide a vehicle ID as a number, not a character string.")
  }
  # Check if the input is a number
  if (!is.numeric(vehicle_id) || vehicle_id %% 1 != 0) {
    stop("ERROR: Please provide a valid vehicle ID as an integer.")
  }
  # Check if the vehicle ID exists in the vehicle records
  vehicle_data <- gas_vehicle_record(vehicle_id)
  if (nrow(vehicle_data) == 0) {
    stop("ERROR: The provided vehicle ID does not exist in the vehicle records.")
  }
  
  # Construct the URL for emission records
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/emissions/", vehicle_id)
  # Make the GET request
  api_info <- httr::GET(url)
  # Convert the raw content to character
  api_char <- base::rawToChar(api_info$content)
  # Parse the JSON content
  parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
  # Print the structure of the parsed data
  str(parsed, max.level = 1)
  # Convert the parsed data to a tibble and remove the 'efid' column
  emission_data <- as_tibble(parsed$emissionsInfo) %>% select(-efid)
  return(emission_data)
}


 
 
## Function to get fuel prices by fuel type
get_fuel_prices <- function(fuel_type = NULL) {
  # Check if fuel_type is specified
  if (is.null(fuel_type)) {
    stop("ERROR: Please provide a fuel type (e.g., 'Regular', 'Midgrade', 'Premium', etc.).")
  }
  # Construct the URL for fuel prices
  url <- "https://www.fueleconomy.gov/ws/rest/fuelprices"
  # Make the GET request
  api_info <- httr::GET(url)
  # Convert the raw content to character
  api_char <- base::rawToChar(api_info$content)
  # Parse the JSON content
  parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
  # Check if the specified fuel type exists in the parsed data
  if (!(fuel_type %in% names(parsed))) {
    stop(paste("ERROR: Fuel type", fuel_type, "not found in fuel prices data. Available types are:", paste(names(parsed), collapse = ", ")))
  }
  # Convert the parsed data to a tibble
  fuel_prices <- as_tibble(parsed[[fuel_type]])
  return(fuel_prices)
}



## This function returns a list of model option and the associated vehicle ID for a particular year, make and model
get_vehicle_options <- function(year, make, model) {
  # Validate year input
  if (!is.numeric(year) || nchar(year) != 4) {
    stop("ERROR: Please provide a valid 4-digit year.")
  }
  # Validate make input
  if (!is.character(make)) {
    stop("ERROR: Please provide the make as a character string.")
  }
  # Validate model input
  if (!is.character(model)) {
    stop("ERROR: Please provide the model as a character string.")
  }
  # URL encode make and model
  make <- URLencode(make)
  model <- URLencode(model)
  # Construct the URL for vehicle options
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/menu/options?year=", year, "&make=", make, "&model=", model)
  tryCatch({
    # Make the GET request
    api_info <- httr::GET(url)
    # Convert the raw content to character
    api_char <- base::rawToChar(api_info$content)
    # Parse the JSON content
    parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
    # Print the structure of the parsed data
    str(parsed, max.level = 1)
    # Convert the parsed data to a tibble
    vehicle_options <- as_tibble(parsed$menuItem)
    colnames(vehicle_options) <- c("Engine info", "Vehicle record ID")
    return(vehicle_options)
  }, error = function(e) {
    message("Error: ", e$message)
  })
}





##figure out why the download csv file is not workign first
##second create simple contingency tables
## metric below and plots

##metrics i want is comparison of yousavespend attribute over cars with fueltype1 vs fueltype2 using first function
## do you really save by having an electric car

##create contingency tables and work on dashboard itself now and to utilize the functions created
##try to make dashboard as dashy as possible and go over project description so that we do not have to 
##create fifth and sixth functions last 

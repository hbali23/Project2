library(httr)
library(jsonlite)
library(tidyverse)

#This function retrieves and processes data for a gas vehicle from the FuelEconomy.gov API, selecting specific columns and arranging the data by year.
gas_vehicle_record <- function(vehicle_id) {
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
    select(barrels08, barrelsA08, city08, cityA08, cylinders, engId, highway08, highwayA08U, lv2, lv4, year, model) %>%
    arrange(year)
  return(selected_data)
}



# Function to get emission records by modifying the "vehicle_id" endpoint
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


  
# Function to get fuel prices by fuel type
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





get_vehicle_options <- function(year, make, model) {
  # Construct the URL for vehicle options
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/menu/options?year=", year, "&make=", make, "&model=", model)
  # Make the GET request
  api_info <- httr::GET(url)
  # Convert the raw content to character
  api_char <- base::rawToChar(api_info$content)
  # Parse the JSON content
  parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
  # Print the structure of the parsed data
  str(parsed, max.level = 1)
  # Convert the parsed data to a tibble
  vehicle_options <- as_tibble(parsed$options)
  return(vehicle_options)
}



library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggplot2)


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
  # Remove the emissionsList element if it is NULL
  parsed <- parsed[!sapply(parsed, is.null)]
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
  parsed <- parsed[!sapply(parsed, is.null)]
  # Print the structure of the parsed data
  str(parsed, max.level = 1)
  parsed <- parsed[!sapply(parsed, is.null)]
  # Convert the parsed data to a tibble and remove the 'efid' column
  emission_data <- as_tibble(parsed$emissionsInfo) %>% select(-efid)
  return(emission_data)
}


 
 
## Function to get fuel prices by fuel type
get_fuel_prices <- function(fuel_type = NULL) {
  # Check if fuel_type is specified
  if (is.null(fuel_type)) {
    stop("ERROR: Please provide a fuel type (e.g., 'regular', 'midgrade', 'premium', etc.).")
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
  parsed <- parsed[!sapply(parsed, is.null)]
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
    parsed <- parsed[!sapply(parsed, is.null)]
    vehicle_options <- as_tibble(parsed$menuItem)
    colnames(vehicle_options) <- c("Engine info", "Vehicle record ID")
    return(vehicle_options)
  }, error = function(e) {
    message("Error: ", e$message)
  })
}



# Function to retrieve user MPG records for a specific vehicle ID
user_mpg_records <- function(vehicle_id) {
  if (typeof(vehicle_id) == "character") {
    stop("ERROR: Please provide a vehicle ID as a number, not a character string.")
  }
  if (!is.numeric(vehicle_id) || vehicle_id %% 1 != 0) {
    stop("ERROR: Please provide a valid vehicle ID as an integer.")
  }
  
  url <- paste0("https://www.fueleconomy.gov/ws/rest/ympg/shared/ympgDriverVehicle/", vehicle_id)
  api_info <- httr::GET(url)
  api_char <- base::rawToChar(api_info$content)
  parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
  parsed <- parsed[!sapply(parsed, is.null)]
  user_mpg_data <- as_tibble(parsed$yourMpgDriverVehicle)  
  return(user_mpg_data)
}



# Function to retrieve and process data for a gas vehicle from FuelEconomy.gov API
vehicle_record_segment <- function(vehicle_id) {
  # Construct the URL
  url <- paste0("https://www.fueleconomy.gov/ws/rest/vehicle/", vehicle_id)
  # Make the GET request
  api_info <- httr::GET(url)
  # Convert the raw content to character
  api_char <- base::rawToChar(api_info$content)
  # Parse the JSON content
  parsed <- jsonlite::fromJSON(api_char, flatten = TRUE)
  # Remove the emissionsList element if it is NULL
  parsed <- parsed[!sapply(parsed, is.null)]
  # Convert the parsed data to a tibble
  vehicle_data <- as_tibble(parsed)
  # Select the desired columns
  selected_data <- vehicle_data %>%
    select(comb08, fuelType1) %>%
    mutate(across(comb08, as.numeric)) # Convert comb08 to numeric
  # Return the selected data
  return(selected_data)
}

# Function to fetch vehicle data and compute average MPG
calculate_average_mpg <- function(vehicle_id) {
  # Fetch vehicle data using vehicle_record function
  vehicle_data <- vehicle_record(vehicle_id)
  # Convert highway08 and city08 to numeric (if not already)
  vehicle_data$highway08 <- as.numeric(vehicle_data$highway08)
  vehicle_data$city08 <- as.numeric(vehicle_data$city08)
  # Calculate average MPG
  average_mpg <- vehicle_data %>%
    summarise(
      Average_MPG = mean((highway08 + city08) / 2, na.rm = TRUE)
    )
  return(average_mpg)
}

















# The contingency table created here shows different types of miles per gallon (MPG) values associated with a specific vehicle record. It includes values for city driving (city MPG), highway driving (highway MPG), and a combined average (combined MPG), providing a concise summary of the vehicle's fuel efficiency across different driving conditions.
create_mpg_table <- function(parsed) {
  # Create a contingency table for MPG values
  mpg_table <- tibble(
    "MPG Type" = c("City MPG", "Highway MPG"),
    "MPG" = c(parsed$city08, parsed$highway08)
  )
  return(mpg_table)
}


# This contingency table provides a concise overview of basic vehicle details—such as ID, year, model, and make—based on the inputted vehicle ID, summarizing key descriptive attributes of the vehicle fetched from the FuelEconomy.gov API.
create_description_table <- function(vehicle_data) {
  # Extract ID, year, model, and make
  description_table <- tibble(
    "Attribute" = c("Year", "Model", "Fuel Type"),
    "Value" = c(vehicle_data$year, vehicle_data$model, vehicle_data$fuelType1)
  )
  
  return(description_table)
}




# Function to plot histogram with density plots of combined MPG, colored by fuel type
plot_combined_mpg_histogram <- function(vehicle_ids) {
  # Initialize an empty list to store data frames
  all_data <- list()
  # Loop through each vehicle ID and retrieve data
  for (vehicle_id in vehicle_ids) {
    # Retrieve data for the current vehicle ID
    vehicle_data <- vehicle_record_segment(vehicle_id)
    # Store the data frame in the list
    all_data[[as.character(vehicle_id)]] <- vehicle_data
  }
  # Combine all data frames into one
  combined_data <- bind_rows(all_data, .id = "vehicle_id")
  
  # Plotting histogram with density plots of combined MPG, colored by fuel type
  ggplot(data = combined_data, aes(x = comb08, fill = fuelType1)) +
    geom_histogram(binwidth = 2, alpha = 0.5, color = "black") +
    geom_density(alpha = 0.5) +
    labs(title = "Distribution of Combined MPG",
         x = "Combined MPG",
         y = "Density",
         fill = "Fuel Type") +
    scale_fill_manual(values = c("red", "blue", "green")) +  # Customize fill colors if needed
    theme_minimal()
}

# Example usage: Test the function with a list of vehicle IDs
vehicle_ids <- c(31873, 26425, 1000, 2000)  # Replace with your actual vehicle IDs
plot_combined_mpg_histogram(vehicle_ids)









#understanding api
# get information about businesses
api_info <- httr::GET("https://www.fueleconomy.gov/ws/rest/vehicle/menu/year") #this url has to follow resources manual on the website  
str(api_info, max.level = 1)

library(tidyverse)
api_char <- base::rawToChar(api_info$content)
api_JSON <- jsonlite::fromJSON(api_char, flatten=TRUE) 
parsed <- fromJSON(rawToChar(api_info$content))  #click on parsed and click on drop down for item that has data in (this one is named articles) and there you will see the names of the other variables but i think they all start with id and name
str(parsed)
article <- as_tibble(parsed$menuItem) #see articles is selected here and is now a dataframe

#removed source from first two columns id and name that had source$ attached to it
article <- article |> mutate(id=article$source$id,
                             name=article$source$name) |> select(id, name, everything(), -source)
article


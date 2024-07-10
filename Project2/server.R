# Load necessary libraries
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)  # For plotting

# Define server logic
server <- function(input, output, session) {
  
  # Function to retrieve and process vehicle record data
  vehicle_record <- function(year, make, model) {
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
  
  
  # Function to create description table for vehicle details
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
  
  
  
  # Example function to fetch and plot emission records for multiple vehicle IDs
  plot_emissions_by_vehicle <- function(vehicle_ids) {
    # Initialize an empty list to store data frames
    all_data <- list()
    
    # Loop through each vehicle ID and retrieve emissions data
    for (vehicle_id in vehicle_ids) {
      # Retrieve emissions data for the current vehicle ID
      emission_data <- emission_records(vehicle_id)
      # Add vehicle_id column to identify which data belongs to which vehicle
      emission_data <- mutate(emission_data, vehicle_id = as.character(vehicle_id))
      # Store the data frame in the list
      all_data[[as.character(vehicle_id)]] <- emission_data
    }
    
    # Combine all data frames into one
    combined_data <- bind_rows(all_data)
    
    # Plotting emissions by year and engine ID, faceted by vehicle ID
    ggplot(data = combined_data, aes(x = year, y = co2TailpipeGpm, color = factor(year))) +
      geom_line() +
      labs(title = "Emissions by Year and Engine ID",
           x = "Year",
           y = "CO2 Tailpipe GPM",
           color = "Year") +
      facet_wrap(~ vehicle_id, scales = "free") +  # Facet by vehicle_id
      theme_minimal()
  }
  
  
  # Dynamically create UI elements based on selected function in Data Download tab
  output$year_input <- renderUI({
    if (input$download_function %in% c("Vehicle Record", "Emission Records", "Vehicle Options")) {
      selectInput("year", "Year:", choices = 1984:2024, selected = 2020)
    }
  })
  
  output$make_input <- renderUI({
    if (input$download_function %in% c("Vehicle Record", "Emission Records", "Vehicle Options")) {
      textInput("make", "Make:")
    }
  })
  
  output$model_input <- renderUI({
    if (input$download_function %in% c("Vehicle Record", "Emission Records", "Vehicle Options")) {
      textInput("model", "Model:")
    }
  })
  
  # Function to handle data download for vehicle records
  observeEvent(input$submit_download_vehicle, {
    data <- switch(input$download_function_vehicle,
                   "Vehicle Record" = {
                     req(input$year, input$make, input$model)
                     vehicle_record(input$year, input$make, input$model)
                   },
                   "Emission Records" = {
                     # Handle emission records download here
                   },
                   "Vehicle Options" = {
                     # Handle vehicle options download here
                   },
                   "User MPG Records" = {
                     # Handle user MPG records download here
                   })
    output$downloaded_data_vehicle <- renderTable({
      data
    })
    output$download_csv_vehicle <- downloadHandler(
      filename = function() {
        paste0(input$download_function_vehicle, "_data.csv")
      },
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
  
  # Function to handle fuel price download for data_download2 tab
  observeEvent(input$submit_fuel_price2, {
    req(input$fuel_type2)
    fuel_price_data <- get_fuel_prices(input$fuel_type2)
    output$fuel_price_output2 <- renderText({
      paste("Current Price of", input$fuel_type2, ":", fuel_price_data$price)
    })
  })
  
  # Numerical Summaries UI elements
  output$categorical_var <- renderUI({
    req(input$summary_function)
    if (input$summary_function == "Vehicle Record") {
      selectInput("cat_var", "Categorical Variable:",
                  choices = c("fuelType1", "fuelType2", "model", "year"), selected = "fuelType1")
    } else if (input$summary_function == "Emission Records") {
      selectInput("cat_var", "Categorical Variable:",
                  choices = c("engId", "year"), selected = "engId")
    } else if (input$summary_function == "Fuel Prices") {
      NULL # No categorical variables
    } else if (input$summary_function == "Vehicle Options") {
      selectInput("cat_var", "Categorical Variable:",
                  choices = c("Engine info"), selected = "Engine info")
    } else if (input$summary_function == "User MPG Records") {
      NULL # No categorical variables
    }
  })
  
  output$quantitative_var <- renderUI({
    req(input$summary_function)
    if (input$summary_function == "Vehicle Record") {
      selectInput("quant_var", "Quantitative Variable:",
                  choices = c("barrels08", "barrelsA08", "city08", "cityA08", "cylinders", "fuelCost08", "fuelCostA08", "highway08", "highwayA08U", "youSaveSpend"), selected = "city08")
    } else if (input$summary_function == "Emission Records") {
      selectInput("quant_var", "Quantitative Variable:",
                  choices = c("co2TailpipeGpm", "ghgScore"), selected = "co2TailpipeGpm")
    } else if (input$summary_function == "Fuel Prices") {
      selectInput("quant_var", "Quantitative Variable:",
                  choices = c("price"), selected = "price")
    } else if (input$summary_function == "Vehicle Options") {
      NULL # No quantitative variables
    } else if (input$summary_function == "User MPG Records") {
      selectInput("quant_var", "Quantitative Variable:",
                  choices = c("avgMpg"), selected = "avgMpg")
    }
  })
  
  observeEvent(input$submit_summary, {
    req(input$summary_function, input$cat_var, input$quant_var)
    summary_data <- switch(input$summary_function,
                           "Vehicle Record" = {
                             vehicle_record(input$year, input$make, input$model)
                           },
                           "Emission Records" = {
                             emission_records(input$year, input$make, input$model)
                           },
                           "Fuel Prices" = {
                             get_fuel_prices(input$fuel_type)
                           },
                           "Vehicle Options" = {
                             vehicle_record(input$year, input$make, input$model)
                           },
                           "User MPG Records" = {
                             user_mpg_records(input$vehicle_id)
                           })
    # Render summary data table or other UI elements
    output$summary_data <- renderTable({
      summary_data
    })
  })
  
  # Additional Functionality: Generate and plot average MPG
  output$average_mpg <- renderTable({
    req(input$calculate_mpg_vehicle_id)
    calculate_average_mpg(input$calculate_mpg_vehicle_id)
  })
  
  # Additional Functionality: Generate description table
  output$description_table <- renderTable({
    req(input$vehicle_id_description)
    vehicle_data <- vehicle_record(input$vehicle_id_description)
    create_description_table(vehicle_data)
  })
  
  # Additional Functionality: Plot combined MPG histogram
  output$combined_mpg_plot <- renderPlot({
    req(input$plot_vehicle_ids)
    plot_combined_mpg_histogram(input$plot_vehicle_ids)
  })
}



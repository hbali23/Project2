library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)  # Load tibble package for tbl_df function


BASE_URL <- "www.fueleconomy.gov/ws/rest/"
info_url <- "fuelprices"
full_url <- base::paste0(BASE_URL, info_url)
api_call <- httr::GET(full_url)
#checking api response to see if api was able to find data and send it back to me successfully. it should = 200 meaning it's successful
api_call$status_code
api_char <- base::rawToChar(api_call$content)
api_JSON <- jsonlite::fromJSON(api_char, flatten=TRUE) #if finds dataframe, flatten=true will convert to dataframe

df<- api_JSON$regular

##continue from here to get dataframe then we can rewrite code for other parts to extract peieces we need and mimic his shiny app

print(colnames(get_info_df))



##FFOLLOW HW4 SOLUTIONS FROM HERE!!!
####### step1: find out col names for these and parse them using if else statements
####### steo2: figure out why dropdown for make isn't working (most likely cause of data)
####### step3: make final tab and analysis pretty things up

```


```{r}
library(httr)
library(jsonlite)
# get information about businesses
business_info <- httr::GET("https://newsapi.org/v2/top-headlines?country=us&category=business&apiKey=d0c08c5bbcd3476aa22de1b997561871")
str(business_info, max.level = 1)

library(tidyverse)
parsed <- fromJSON(rawToChar(business_info$content))  #click on parsed and click on drop down for item that has data in (this one is named articles) and there you will see the names of the other variables but i think they all start with id and name
str(parsed)
article <- as_tibble(parsed$articles) #see articles is selected here and is now a dataframe

#removed source from first two columns id and name that had source$ attached to it
article <- article |> mutate(id=article$source$id,
                             name=article$source$name) |> select(id, name, everything(), -source)
article



BASE_URL <- "www.fueleconomy.gov/ws/rest/"
full_url <- base::paste0(BASE_URL, info_url)
api_call <- httr::GET(full_url)
#checking api response to see if api was able to find data and send it back to me successfully. it should = 200 meaning it's successful
api_call$status_code
api_char <- base::rawToChar(api_call$content)
api_JSON <- jsonlite::fromJSON(api_char, flatten=TRUE) 
data <- as_tibble(api_JSON)

df<- api_JSON$regular


library(httr)
library(jsonlite)
library(tidyverse)

# Correct API URL with https:// and known endpoint
BASE_URL <- "https://www.fueleconomy.gov/ws/rest/"
info_url <- "fuelprices"
full_url <- paste0(BASE_URL, info_url)

# Make the API call
api_call <- httr::GET(full_url)

# Check the status code of the response
if (api_call$status_code == 200) {
  # Convert raw content to character
  api_char <- rawToChar(api_call$content)
  
  # Print the response to inspect it
  cat("API Response:\n", api_char, "\n\n")
  
  # Attempt to parse as JSON only if it's in JSON format
  try({
    api_JSON <- fromJSON(api_char, flatten = TRUE)
    print("Parsed JSON:")
    str(api_JSON, max.level = 1)
  }, silent = TRUE)
  
} else {
  cat("Failed to retrieve data. Status code:", api_call$status_code, "\n")
}












#OLD SERVER CODE
#dont forget to comment

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)  # Load tibble package for tbl_df function

BASE_URL <- "https://www.fueleconomy.gov/ws/rest"

####### step1: find out col names for these and parse them using if else statements
####### steo2: figure out why dropdown for make isn't working (most likely cause of data)
####### step3: make final tab and analysis pretty things up

get_makes_by_year <- function(year) {
  url <- paste0(BASE_URL, "/vehicle/menu/make?year=", year)
  response <- GET(url)
  data <- fromJSON(content(response, "text"))
  return(as_tibble(as.data.frame(data$menuItem)))  # Convert to tbl_df
}

get_models_by_make <- function(make) {
  url <- paste0(BASE_URL, "/vehicle/menu/model?make=", make)
  response <- GET(url)
  data <- fromJSON(content(response, "text"))
  return(as_tibble(as.data.frame(data$menuItem)))  # Convert to tbl_df
}

get_options_by_make_model_year <- function(make, model, year) {
  url <- paste0(BASE_URL, "/vehicle/menu/options?year=", year, "&make=", make, "&model=", model)
  response <- GET(url)
  data <- fromJSON(content(response, "text"))
  return(as_tibble(as.data.frame(data$menuItem)))  # Convert to tbl_df
}

get_vehicle_by_id <- function(vehicle_id) {
  url <- paste0(BASE_URL, "/vehicle/id/", vehicle_id)
  response <- GET(url)
  data <- fromJSON(content(response, "text"))
  return(as_tibble(as.data.frame(data)))  # Convert to tbl_df
}

get_vehicle_by_ymm <- function(year, make, model) {
  url <- paste0(BASE_URL, "/vehicle/", year, "/", make, "/", model)
  response <- GET(url)
  data <- fromJSON(content(response, "text"))
  return(as_tibble(as.data.frame(data)))  # Convert to tbl_df
}

get_fuel_economy_by_ymm <- function(year, make, model) {
  url <- paste0(BASE_URL, "/ympg/shared/ympgVehicle?", "year=", year, "&make=", make, "&model=", model)
  response <- GET(url)
  data <- fromJSON(content(response, "text"))
  return(as_tibble(as.data.frame(data)))  # Convert to tbl_df
}

shinyServer(function(input, output, session) {
  
  observeEvent(input$getMakes, {
    updateSelectInput(session, "make", choices = get_makes_by_year(input$year))
  })
  
  observeEvent(input$getModels, {
    updateSelectInput(session, "model", choices = get_models_by_make(input$make))
  })
  
  observeEvent(input$getOptions, {
    output$apiData <- renderTable({
      get_options_by_make_model_year(input$make, input$model, input$year)
    })
  })
  
  observeEvent(input$getVehicleById, {
    output$apiData <- renderTable({
      get_vehicle_by_id(input$vehicleId)
    })
  })
  
  observeEvent(input$getVehicleByYMM, {
    output$apiData <- renderTable({
      get_vehicle_by_ymm(input$year, input$make, input$model)
    })
  })
  
  observeEvent(input$getFuelEconomyByYMM, {
    output$apiData <- renderTable({
      get_fuel_economy_by_ymm(input$year, input$make, input$model)
    })
  })
  
  output$dataTable <- renderTable({
    data.frame(
      Year = input$year,
      Make = input$make,
      Model = input$model
    )
  })
  
  observe({
    updateSelectInput(session, "year", choices = as.character(2000:2024))
    updateSelectInput(session, "downloadYear", choices = as.character(2000:2024))
  })
  
  # Data Download Tab
  observeEvent(input$downloadGetMakes, {
    updateSelectInput(session, "downloadMake", choices = get_makes_by_year(input$downloadYear))
  })
  
  observeEvent(input$downloadGetModels, {
    updateSelectInput(session, "downloadModel", choices = get_models_by_make(input$downloadMake))
  })
  
  observeEvent(input$downloadGetData, {
    output$downloadDataTable <- renderTable({
      get_vehicle_by_ymm(input$downloadYear, input$downloadMake, input$downloadModel)
    })
    
    output$downloadColumns <- renderUI({
      data <- get_vehicle_by_ymm(input$downloadYear, input$downloadMake, input$downloadModel)
      checkboxGroupInput("selectedColumns", "Select Columns:", choices = names(data))
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("fuel_economy_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data <- get_vehicle_by_ymm(input$downloadYear, input$downloadMake, input$downloadModel)
      selectedData <- data[, input$selectedColumns, drop=FALSE]
      write.csv(selectedData, file)
    }
  )
})







#OLD UI CODE
#dont forget to comment

# Set working directory to the location of your Shiny app files
setwd("/Users/hananali/Desktop/Project2")

library(shiny)

shinyUI(fluidPage(
  titlePanel("Fuel Economy API Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = NULL),
      selectInput("make", "Select Make:", choices = NULL),
      selectInput("model", "Select Model:", choices = NULL),
      actionButton("getMakes", "Get Makes"),
      actionButton("getModels", "Get Models"),
      actionButton("getOptions", "Get Options"),
      actionButton("getVehicleById", "Get Vehicle by ID"),
      actionButton("getVehicleByYMM", "Get Vehicle by Year, Make, Model"),
      actionButton("getFuelEconomyByYMM", "Get Fuel Economy by Year, Make, Model"),
      textInput("vehicleId", "Enter Vehicle ID:"),
      tableOutput("dataTable")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("API Data",
                 h3("API Data"),
                 tableOutput("apiData")
        ),
        tabPanel("About",
                 h3("About the App"),
                 p("This app allows users to explore the Fuel Economy API, which provides data on vehicle fuel economy."),
                 p("The data is sourced from the US Department of Energy's Fuel Economy website."),
                 a("More information about the data", href="https://www.fueleconomy.gov/feg/ws/index.shtml"),
                 p("The app has several tabs to explore different data:"),
                 p("- API Data: Explore different types of data from the Fuel Economy API."),
                 p("- Data Download: Query the API, display the data, and download it as a CSV file."),
                 img(src = "Seal_of_the_United_States_Department_of_Energy.png", height = 100, width = 100)
        ),
        tabPanel("Data Download",
                 h3("Data Download"),
                 p("Use the controls to specify your query, display the data, and download it."),
                 selectInput("downloadYear", "Select Year:", choices = NULL),
                 selectInput("downloadMake", "Select Make:", choices = NULL),
                 selectInput("downloadModel", "Select Model:", choices = NULL),
                 actionButton("downloadGetMakes", "Get Makes"),
                 actionButton("downloadGetModels", "Get Models"),
                 actionButton("downloadGetData", "Get Data"),
                 tableOutput("downloadDataTable"),
                 uiOutput("downloadColumns"),
                 downloadButton("downloadData", "Download Data")
        )
      )
    )
  )
))




# Assuming vehicle_record function is already defined

# Fetch data for a specific vehicle ID
vehicle_id <- 12345  # Example vehicle ID
vehicle_data <- vehicle_record(vehicle_id)
# View the structure of the fetched data
str(vehicle_data)
# Example usage: after function is ran
vehicle_id <- 100  # Replace with a valid vehicle ID
vehicle_data <- vehicle_record(vehicle_id)
mpg_table <- create_mpg_table(vehicle_data)
print(mpg_table)






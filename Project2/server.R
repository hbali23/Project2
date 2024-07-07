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

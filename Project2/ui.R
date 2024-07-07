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

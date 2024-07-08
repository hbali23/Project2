library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Vehicle Data App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Data Download", tabName = "data_download", icon = icon("download")),
      menuItem("Data Exploration", tabName = "data_exploration", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              fluidRow(
                box(width = 12, title = "About This App", status = "primary", solidHeader = TRUE,
                    p("This app provides access to vehicle data and fuel economy information."),
                    p("Data Source: ", a(href = "https://www.fueleconomy.gov/", "FuelEconomy.gov")),
                    p("Purpose of each tab:"),
                    p("1. About: Information about the app."),
                    p("2. Data Download: Query the API and download data."),
                    p("3. Data Exploration: Visualize and analyze the data."),
                    img(src = "Seal_of_the_United_States_Department_of_Energy.png", height = 100, width = 100)
                )
              )),
      tabItem(tabName = "data_download",
              fluidRow(
                box(width = 12, title = "Data Download", status = "primary", solidHeader = TRUE,
                    textInput("vehicle_id", "Enter Vehicle ID", value = "31873"),
                    actionButton("download_data", "Download Data"),
                    DTOutput("data_table"),
                    downloadButton("download_csv", "Download CSV")
                )
              )),
      tabItem(tabName = "data_exploration",
              fluidRow(
                box(width = 12, title = "Data Exploration", status = "primary", solidHeader = TRUE,
                    selectInput("plot_var", "Choose a variable to plot", choices = NULL),
                    selectInput("summary_var", "Choose a variable for summary", choices = NULL),
                    selectInput("plot_type", "Choose plot type", choices = c("Histogram", "Boxplot", "Scatterplot")),
                    actionButton("plot_data", "Plot Data"),
                    plotOutput("plot"),
                    DTOutput("contingency_table"),
                    verbatimTextOutput("summary")
                )
              ))
    )
  )
)

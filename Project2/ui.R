library(shiny)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Vehicle Data Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data Download", tabName = "data_download"),
      menuItem("Data Exploration", tabName = "data_exploration")
    )
  ),
  dashboardBody(
    tabItems(
      # About tab content
      tabItem(tabName = "about",
              h2("About"),
              fluidRow(
                column(6,
                       tags$p("This app provides access to vehicle data from FuelEconomy.gov."),
                       tags$p("Data sources include vehicle records, emissions records, and fuel prices."),
                       tags$p("Each tab serves a different purpose in exploring and downloading this data.")
                ),
                column(6,
                       tags$img(src = "Seal_of_the_United_States_Department_of_Energy.png", height = 100, width = 100)
                )
              )
      ),
      
      # Data Download tab content
      tabItem(tabName = "data_download",
              h2("Data Download"),
              fluidRow(
                column(4,
                       selectInput("download_function",
                                   "Select Function:",
                                   choices = c("Vehicle Record", "Emission Records", "Fuel Prices", "Vehicle Options"),
                                   selected = "Vehicle Record"),
                       uiOutput("year_input"),
                       uiOutput("make_input"),
                       uiOutput("model_input"),
                       actionButton("submit_download", "Download Data")
                ),
                column(8,
                       tableOutput("downloaded_data"),
                       downloadButton("download_csv", "Download CSV")
                )
              )
      ),
      
      # Data Exploration tab content
      tabItem(tabName = "data_exploration",
              h2("Data Exploration"),
              fluidRow(
                column(4,
                       selectInput("explore_function",
                                   "Select Function:",
                                   choices = c("Vehicle Record", "Emission Records"),
                                   selected = "Vehicle Record")
                ),
                column(8,
                       plotOutput("exploration_plot")
                )
              )
      )
    )
  )
)

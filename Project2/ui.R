library(shiny)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Vehicle Data Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data Download", tabName = "data_download"),
      menuItem("Data Exploration", tabName = "data_exploration"),
      menuItem("Numerical Summaries", tabName = "numerical_summaries")
    )
  ),
  dashboardBody(
    tabItems(
      # About tab content
      tabItem(tabName = "about",
              h2("About"),
              fluidRow(
                column(6,
                       tags$p("This app integrates with the FuelEconomy.gov API to provide access to vehicle data. This data is from the US Department of Energy and has a source for fuel economy information. For more information about the data see", href = "https://fueleconomy.gov/feg/ws/"),
                       tags$p("The data sources include vehicle records, emissions records, fuel prices, and user MPG records. Users can explore these records through intuitive dropdown menus and input fields. It provides functionalities such as data download, subset selection, CSV export, and dynamic visualizations like histograms and contingency tables."),
                       tags$p("The About tab provides an overview of the app's purpose and source information. It also links users to more detailed information about the data from the official source."),
                       tags$p("The Data Download Tab enables users to specify and retrieve various types of vehicle-related data, such as vehicle records, emission records, fuel prices, and user MPG records. They can subset and download this data as CSV files for further analysis."),
                       tags$p("The Data Exploration Tab allows users to explore the selected data visually through plots and summaries. They can choose variables or combinations of variables to generate contingency tables and histograms, gaining insights into different aspects of vehicle data."),
                       tags$p("Overall, the app caters to both analytical and practical needs, offering insights into vehicle attributes and fuel efficiency metrics with interactive features for enhanced user experience and data exploration."),
                       tags$p("Examples of vehicle IDs that can be used to pull the data are 31873 and 26425. You may find a longer list on the website")
                ),
                column(6,
                       tags$img(src = "Seal_of_the_United_States_Department_of_Energy.png", height = 100, width = 100)
                )
              )
      ),
      
      # Data Download tab content (Assuming you have already implemented this part)
      tabItem(tabName = "data_download",
              h2("Data Download"),
              fluidRow(
                column(4,
                       selectInput("download_function",
                                   "Select Function:",
                                   choices = c("Vehicle Record", "Emission Records", "Fuel Prices", "Vehicle Options", "User MPG Records"),
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
      
      
      # Data Download2 tab content
      tabItem(tabName = "data_download2",
              h2("Data Download 2"),
              fluidRow(
                column(4,
                       selectInput("fuel_type2",
                                   "Select Fuel Type:",
                                   choices = c("Regular", "Midgrade", "Premium")),
                       actionButton("submit_fuel_price2", "Get Fuel Price")
                ),
                column(8,
                       verbatimTextOutput("fuel_price_output2")
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
      ),
      
      # Numerical Summaries tab content
      tabItem(tabName = "numerical_summaries",
              h2("Numerical Summaries"),
              fluidRow(
                column(4,
                       selectInput("summary_function",
                                   "Select Dataset:",
                                   choices = c("Vehicle Record", "Emission Records", "Fuel Prices", "Vehicle Options", "User MPG Records"),
                                   selected = "Vehicle Record"),
                       uiOutput("categorical_var"),
                       uiOutput("quantitative_var"),
                       actionButton("submit_summary", "Generate Summary")
                ),
                column(8,
                       tableOutput("summary_table")
                )
              ),
              fluidRow(
                column(4,
                       textInput("vehicle_id_summary", "Vehicle ID:")
                ),
                column(8,
                       tableOutput("average_mpg_table")
                )
              )
      )
    )
  )
)



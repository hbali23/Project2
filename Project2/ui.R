# Write intro

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "My Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Tab 1", tabName = "tab1", icon = icon("chart-bar")),
      menuItem("Tab 2", tabName = "tab2", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              h2("About This App"),
              p("This app demonstrates the usage of the Shiny framework with the shinydashboard package."),
              p("Data Source: ", a(href = "https://example.com", "Example Data Source")),
              p("Purpose of the Tabs:"),
              tags$ul(
                tags$li("About: Describes the app and its purpose."),
                tags$li("Tab 1: Displays a chart based on the data."),
                tags$li("Tab 2: Shows a table view of the data.")
              ),
              img(src = "logo.png", height = "200px")
      ),
      tabItem(tabName = "tab1",
              h2("Tab 1"),
              p("Content for Tab 1")
      ),
      tabItem(tabName = "tab2",
              h2("Tab 2"),
              p("Content for Tab 2")
      )
    )
  )
)


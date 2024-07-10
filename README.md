# Project 2

# Description
This R Shiny app integrates with the FuelEconomy.gov API to retrieve and present vehicle data. Users can explore different aspects of vehicle records, emissions, fuel prices, and user MPG records through intuitive dropdown menus and input fields. It provides functionalities such as data download, subset selection, CSV export, and dynamic visualizations like histograms and contingency tables. The app caters to both analytical and practical needs, offering insights into vehicle attributes and fuel efficiency metrics with interactive features for enhanced user experience and data exploration.

# Required R packages
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(DT)
library(tibble)

# Install required packages
install.packages(c("shiny", "shinydashboard", "httr", "jsonlite", "dplyr", "ggplot2", "DT", "tibble"))


# Run shiny app
shiny::runGitHub(repo = "Project2", username = "hbali23", subdir = "Project2")

# Project 2

# Description
Brief description of the app and its purpose.
This app allows users to explore the Fuel Economy API, which provides data on vehicle fuel economy.

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

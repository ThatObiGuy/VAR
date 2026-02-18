# Shared resources
# Runs once per R session before ui/server

# Libraries
library(shiny)
library(shinythemes) # Adding themeing package, quick way to make app more appealing with minimal effort
library(DBI)  # For DB interactions
library(RPostgres)  # For DB interactions
library(dbplyr) # To allow us to use the language of dplyr rather than SQL on our tables
library(dplyr) # any server side adjustments
library(DT) # DataTable used to allow users to select matches
library(ggplot2) # Familiar graphing tool
library(plotly) # Interactive plots for Teams view
library(scales)  # Color palettes and scaling utilities
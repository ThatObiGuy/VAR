# Shared resources
# Runs once per R session before ui/server

# Libraries
library(shiny)
library(shinythemes) # Adding themeing package, quick way to make app more appealing with minimal effort
library(readr) # reading in local csv's
library(dplyr) # manipulating data
library(DT) # DataTable used to allow users to select matches
library(ggplot2) # Familiar graphing tool
library(plotly) # Interactive plots for Teams view
library(scales)  # Color palettes and scaling utilities

# CSV data loader
source("R/utils_data.R")

# Load all CSVs under data/ once per R session
# Exposed as DATA: named list of tibbles (e.g., DATA$results1, DATA$odds1x2, DATA$entropy)
DATA <- try(load_csv_dir("data"), silent = TRUE)
if (inherits(DATA, "try-error")) {
  warning("Failed to load CSV data from 'data/'. The app may not function correctly without local data.")
  DATA <- list()
}

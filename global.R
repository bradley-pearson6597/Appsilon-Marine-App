#####################################################################################################
###################################### GLOBAL FILE ####################################################
#######################################################################################################

# Load libraries needed for project
# Need to check if installed
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(dplyr)
library(leaflet)
library(htmltools)
library(DT)
library(shinycssloaders)
library(data.table)
library(bit64)
# library(ggplot2)
# library(lubridate)

# Load modules into R Session
source(paste0(getwd(), "/modules.R"))

# Read ships data into the app & ensure data is of correct type
# ships.data <- read.csv("ships (small).csv")
# ships.data <- read.csv("ships.csv")
ships.data <- as.data.frame(data.table::fread("ships.csv")) # Allows for quicker & more efficient csv reading

# # Chane Datetime into correct format
# ships.data <- ships.data %>%
#   dplyr::mutate(DATETIME = strptime(DATETIME, format = "%Y-%m-%d %H:%M:%S")) %>%
#   dplyr::arrange(DATETIME) 

avlon = mean(ships.data$LON)
avlat = mean(ships.data$LAT)

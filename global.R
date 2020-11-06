#####################################################################################################
###################################### GLOBAL FILE ####################################################
#######################################################################################################

# Load libraries needed for project

library(shiny)
library(shiny.semantic)
library(dplyr)
library(leaflet)
library(geosphere)

# Read ships data into the app & ensre data is of correct type
ships.data <- read.csv("ships (small).csv") 

# ships.data.small <- ships.data %>% dplyr::slice(1:100000)
# write.csv(ships.data.small, "ships (small).csv")

ships.data <- ships.data %>%
  dplyr::group_by(SHIPNAME) %>%
  dplyr::mutate(DATETIME = strptime(DATETIME, format = "%Y-%m-%d %H:%M:%S"))

avlon = mean(ships.data$LON)
avlat = mean(ships.data$LAT)


# Longitude and latitude distance calculations
R = 6371000  # radius of Earth in meters
phi_1 = (ships.data3$LAT[1])*(pi / 180)
phi_2 = (ships.data3$LAT[2])*(pi / 180)

delta_phi = (ships.data3$LAT[1] - ships.data3$LAT[2]) * (pi/180)
delta_lambda = (ships.data3$LON[1] - ships.data3$LON[2]) * (pi/180)

a = sin(delta_phi / 2)^2 + cos(phi_1) * cos(phi_2) * sin(delta_lambda / 2)^2
c = 2 * atan2(sqrt(a), sqrt(1 - a))
meters = R * c
km = meters / 1000

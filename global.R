#####################################################################################################
###################################### GLOBAL FILE ####################################################
#######################################################################################################

# Load libraries needed for project

library(shiny)
library(shiny.semantic)
library(dplyr)
library(leaflet)
library(htmltools)
library(DT)
library(shinycssloaders)
# library(ggplot2)
# library(lubridate)

# Load modules into R Session
source(paste0(getwd(), "/modules.R"))

# Read ships data into the app & ensre data is of correct type
ships.data <- read.csv("ships (small).csv") 

# ships.data.small <- ships.data %>% dplyr::slice(1:100000)
# write.csv(ships.data.small, "ships (small).csv")

# Chane Datetime into correcrt format
ships.data <- ships.data %>%
  dplyr::mutate(DATETIME = strptime(DATETIME, format = "%Y-%m-%d %H:%M:%S")) %>%
  dplyr::arrange(DATETIME) 

avlon = mean(ships.data$LON)
avlat = mean(ships.data$LAT)



# ships.data2 <- ships.data %>%
#   dplyr::filter(SHIPNAME == "SOLSTRAUM") %>%
#   dplyr::mutate(LAGDATETIME = lag(DATETIME),
#                 TIMEDIFF = difftime(DATETIME, LAGDATETIME, units = "mins"),
#                 TIMEDIFF = round(TIMEDIFF, 2),
#                 LAGLAT = lag(LAT),
#                 LAGLON = lag(LON),
#                 EARTHDIST = 6371000,
#                 phi_1 = LAT*(pi / 180),
#                 phi_2 = LAGLAT*(pi / 180),
#                 delta_phi = (LAT - LAGLAT) * (pi/180),
#                 delta_lambda = (LON - LAGLON) * (pi/180),
#                 a = sin(delta_phi / 2)^2 + cos(phi_1) * cos(phi_2) * sin(delta_lambda / 2)^2,
#                 c = 2 * atan2(sqrt(a), sqrt(1 - a)),
#                 meters = EARTHDIST * c,
#                 meters = round(meters, 2)) 
# 
# 
# ships.data2$Date <- ymd_hms(ships.data2$DATETIME)
# 
# ggplot2::ggplot(data = ships.data2, aes(x = Date, y = meters)) + geom_line(color = "#bc7eaf") +
#   theme_classic() + labs(x = "Date", y = "Distance Travelled (Meters)")
# Look at adding ship sizes
# 
# ships.data3 = ships.data
# # Longitude and latitude distance calculations
# R = 6371000  # radius of Earth in meters
# phi_1 = (ships.data3$LAT[1])*(pi / 180)
# phi_2 = (ships.data3$LAT[2])*(pi / 180)
# 
# delta_phi = (ships.data3$LAT[1] - ships.data3$LAT[2]) * (pi/180)
# delta_lambda = (ships.data3$LON[1] - ships.data3$LON[2]) * (pi/180)
# 
# a = sin(delta_phi / 2)^2 + cos(phi_1) * cos(phi_2) * sin(delta_lambda / 2)^2
# c = 2 * atan2(sqrt(a), sqrt(1 - a))
# meters = R * c
# km = meters / 1000


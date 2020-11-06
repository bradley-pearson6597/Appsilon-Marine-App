#####################################################################################################
###################################### SERVER FILE ####################################################
#######################################################################################################

server <- function(input, output, session){
  
  # Filter ship data based on vessel_type
  vessel.name <- reactive({
    
    vs.names <- ships.data %>%
      dplyr::filter(ship_type %in% c(input[["vessel_type"]])) %>%
      dplyr::distinct(SHIPNAME)
    
    vs.names
  })
  

  
  # Render dropdown input when new vessel_type selected
  output$vesselname <- renderUI({
    
    vs.names <- vessel.name()
    
    dropdown_input("vessel_name", vs.names$SHIPNAME , type = "selection", value = "" )
  })
  
  
  vessel.longesttrip <- eventReactive(input[["vessel_name"]], {
    
    vs.name <- ifelse(is.null(input[["vessel_name"]]), "", input[["vessel_name"]])
    
    # Calculate distance based on longitude and latitude using Haversine Formula
    vs.longesttrip <- ships.data %>%
      dplyr::filter(SHIPNAME == vs.name) %>%
      # dplyr::filter(SHIPNAME == "KAROLI") %>%
      dplyr::mutate(TIMEDIFF = difftime(DATETIME, lag(DATETIME), units = "mins"),
                    TIMEDIFF = round(TIMEDIFF, 2),
                    LAGLAT = lag(LAT),
                    LAGLON = lag(LON),
                    EARTHDIST = 6371000,
                    phi_1 = LAT*(pi / 180),
                    phi_2 = LAGLAT*(pi / 180),
                    delta_phi = (LAT - LAGLAT) * (pi/180),
                    delta_lambda = (LON - LAGLON) * (pi/180),
                    a = sin(delta_phi / 2)^2 + cos(phi_1) * cos(phi_2) * sin(delta_lambda / 2)^2,
                    c = 2 * atan2(sqrt(a), sqrt(1 - a)),
                    meters = R * c,
                    meters = round(meters, 2)) %>%
      dplyr::arrange(desc(meters), desc(DATETIME)) %>%
      dplyr::slice(1)
    
    vs.longesttrip
    
  })
  
  output$vesseldist <- renderUI({
    
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.lt) == 0){
      vs.lt <- data.frame(meters = 0)
    }
    
    message_box(header = "Vessel Distance Travelled (Meters)", content = paste0(vs.lt$meters, " m"), class = "icon", icon_name = "map")
    
  })
  
  output$vesselspeed <- renderUI({
    
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.lt) == 0){
      vs.lt <- data.frame(SPEED = 0)
    }
    
    message_box(header = "Vessel Speed (Knots)", content = paste0(vs.lt$SPEED, " knots"), class = "icon", icon_name = "ship")
    
  })
  
  output$vesseldestination <- renderUI({
    
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.lt) == 0){
      vs.lt <- data.frame(DESTINATION = "UNKNOWN")
    }
    
    message_box(header = "Vessel Destination", content = paste0(vs.lt$DESTINATION), class = "icon", icon_name = "map marker alternate")
    
  })
  
  output$vesseltime <- renderUI({
    
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.lt) == 0){
      vs.lt <- data.frame(TIMEDIFF = 0)
    }
    
    message_box(header = "Vessel Time for Distance (Minutes)", content = paste0(vs.lt$TIMEDIFF, " mins"), class = "icon", icon_name = "stopwatch")
    
  })
  
  
  vessel.mapdata <- eventReactive(input[["vessel_name"]], {
    
    vs.lt <- vessel.longesttrip()

    if(nrow(vs.lt) > 0){
      vs.mapline <- data.frame(Trips = c("Trip1", "Trip2"),
                               lat = c(vs.lt$LAT, vs.lt$LAGLAT),
                               lon = c(vs.lt$LON, vs.lt$LAGLON))
    } else{
      vs.mapline <- data.frame(Trips = c("Trip1", "Trip2"),
                               lat = c(avlat, avlat),
                               lon = c(avlon, avlon))
    }

    
  })
  
  output$vesselmap <- renderLeaflet({
    
    vs.mapdata <- vessel.mapdata()
    vs.lt <- vessel.longesttrip()

    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE, maxZoom = 20)) %>%
      addPolylines(data = vs.mapdata, lng = ~lon, lat = ~lat, group = ~Trips, color = "grey", fillColor = "white") %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
                            }") %>%
      addCircleMarkers(data = vs.mapdata, lng = vs.lt$LON, vs.lt$LAT, radius = 2, color = "grey") %>%
      addCircleMarkers(data = vs.mapdata, lng = vs.lt$LAGLON, vs.lt$LAGLAT, radius = 2, color = "grey")
    
    
  })
  
}


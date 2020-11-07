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
  

  # Filter ship data and calculate distance between before and after coordinates
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
                    meters = EARTHDIST * c,
                    meters = round(meters, 2)) %>%
      dplyr::arrange(desc(meters), desc(DATETIME)) %>%
      dplyr::slice(1)
    
    vs.longesttrip
    
  })
  
  # Create message box on distance travelled
  output$vesseldist <- renderUI({
    
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.lt) == 0){
      vs.lt <- data.frame(meters = 0)
    }
    
    message_box(header = "Vessel Distance Travelled (Meters)", content = paste0(vs.lt$meters, " m"), class = "icon", icon_name = "map")
    
  })
  
  # Create message box on the speed of the vessel currently
  output$vesselspeed <- renderUI({
    
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.lt) == 0){
      vs.lt <- data.frame(SPEED = 0)
    }
    
    message_box(header = "Vessel Speed (Knots)", content = paste0(vs.lt$SPEED, " knots"), class = "icon", icon_name = "ship")
    
  })
  
  # Create a message box for the destination of the vessel
  output$vesseldestination <- renderUI({
    
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.lt) == 0){
      vs.lt <- data.frame(DESTINATION = "UNKNOWN")
    }
    
    message_box(header = "Vessel Destination", content = paste0(vs.lt$DESTINATION), class = "icon", icon_name = "map marker alternate")
    
  })
  
  # Create message box on the time taken by the vessel to cover the distance
  output$vesseltime <- renderUI({
    
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.lt) == 0){
      vs.lt <- data.frame(TIMEDIFF = 0)
    }
    
    message_box(header = "Vessel Time for Distance (Minutes)", content = paste0(vs.lt$TIMEDIFF, " mins"), class = "icon", icon_name = "stopwatch")
    
  })
  
  # Create map data of the points required
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
  
  # Create a leaflet map based on the information provided of current and
  # previous position of vessel
  output$vesselmap <- renderLeaflet({
    
    vs.mapdata <- vessel.mapdata()
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.lt) == 0){
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE, maxZoom = 20)) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
                            }") %>%
        setView(lng = avlon, lat = avlat, zoom = 10)
      
    }else{
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE, maxZoom = 20)) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
                            }") %>%
        addPolylines(data = vs.mapdata, lng = ~lon, lat = ~lat, group = ~Trips, color = "grey", fillColor = "white", popup = ~htmlEscape(paste0("Distance Travelled (Meters): ", vs.lt$meters,"m")), label = ~htmlEscape(paste0(vs.lt$SHIPNAME, "'s Journey"))) %>%
        addCircleMarkers(data = vs.mapdata, lng = vs.lt$LON, vs.lt$LAT, radius = 2, color = "#4b4b4c", opacity = 1, popup = ~htmlEscape(paste0("Longitude:", vs.lt$LON, " Latitude:", vs.lt$LAT))) %>%
        addCircleMarkers(data = vs.mapdata, lng = vs.lt$LAGLON, vs.lt$LAGLAT, radius = 2, color = "#4b4b4c", opacity = 1, popup = ~htmlEscape(paste0("Longitude:", vs.lt$LAGLON, " Latitude:", vs.lt$LAGLAT)))
      
      }
    
  })
  

  vessel.data <- reactive({
    
    vs.name <- ifelse(is.null(input[["vessel_name"]]), "", input[["vessel_name"]])
    
    vs.data <- ships.data %>%
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
                    meters = EARTHDIST * c,
                    meters = round(meters, 2)) 
    
    
    vs.data
  })
  
  output$vesseldata <- DT::renderDataTable({
    
    vs.data <- vessel.data()
    
    DT::datatable(vs.data, style = "default", options = list(autoWidth = TRUE,
                                                                scrollX = TRUE,
                                                                scrollY = TRUE,
                                                                dom = 'Bftsp'))
  })
  
  output$vesselmap2 <- renderLeaflet({
    
    vs.data <- vessel.data()
    vs.mapdata <- vessel.mapdata()
    vs.lt <- vessel.longesttrip()
    
    if(nrow(vs.data) == 0){
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE, maxZoom = 20)) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
                            }") %>%
        setView(lng = avlon, lat = avlat, zoom = 10)
      
    }else{
      
      total.disttravelled <- sum(vs.data$meters, na.rm = T) / 1000
      total.disttravelled <- round(total.disttravelled, 2)
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE, maxZoom = 20)) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
                            }") %>%
        addPolylines(data = vs.data, lng = ~LON, lat = ~LAT, color = "grey", fillColor = "white", popup = ~htmlEscape(paste0("Total Distance Travelled (Kilometers): ", total.disttravelled ,"km"))) %>%
        addPolylines(data = vs.mapdata, lng = ~lon, lat = ~lat, group = ~Trips, color = "green", fillColor = "white", popup = ~htmlEscape(paste0("Distance Travelled (Meters): ", vs.lt$meters,"m"))) 
        
    }
    
  })
  
  
}


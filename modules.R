#####################################################################################################
###################################### MODULES ####################################################
#######################################################################################################

dropdownUI <- function(id){
  tagList(
    flow_layout(p("Choose Vessel Type:", align = "center"),
                p("Choose Vessel Name:", align = "center")),
    flow_layout(
      dropdown_input(NS(id, "vessel_type"), unique(ships.data$ship_type), type = "selection multiple search", value = "", default_text = "Vessel Type"),
      uiOutput(NS(id, "vesselname"))
    )
  )
}

dropdownServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
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
        dropdown_input("vessel_name", vs.names$SHIPNAME , type = "selection search", value = "", default_text = "Vessel Name" )
      })
    }
  )
}

haversineUI <- function(id){
  tagList(
    verbatimTextOutput(NS(id, "haversineform"))
  )
}

haversineServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
    output$haversineform <- renderPrint({
      cat("R = 6371e3 # Radius of earth in metres",
          "phi_1 = Latitude1 * (pi / 180)",
          "phi_2 = Latitude2 * (pi / 180)",
          "delta_phi = (Latitude1 - Latitude2) * (pi / 180)",
          "delta_lambda = (Longitude1 - Longitude2) * (pi/180)",
          "a = sin(delta_phi / 2)^2 + cos(phi_1) * cos(phi_2) * sin(delta_lambda / 2)^2",
          "c = 2 * atan2(sqrt(a), sqrt(1 - a))",
          "meters = R * c",
          sep = "\n")
    })
    
    }
  )
}
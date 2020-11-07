#####################################################################################################
###################################### MODULES ####################################################
#######################################################################################################

dropdownUI <- function(id){
  tagList(
    flow_layout(p("Choose Vessel Type:"),
                p("Choose Vessel Name:")),
    flow_layout(
      dropdown_input(NS(id, "vessel_type"), unique(ships.data$ship_type), type = "selection multiple", value = ""),
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
        dropdown_input("vessel_name", vs.names$SHIPNAME , type = "selection", value = "" )
      })
    }
  )
}
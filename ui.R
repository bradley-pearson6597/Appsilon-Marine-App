#####################################################################################################
###################################### UI FILE ####################################################
#######################################################################################################

ui <- semanticPage(
  title = "Marine App",
  tags$head(tags$style(".leaflet-control-attribution {
                                      display:none;
                       }"
  )),
  
  flow_layout(cell_width = "40%",
              leafletOutput("vesselmap"),
              vertical_layout(
                uiOutput("vesseldist"),
                uiOutput("vesseltime"),
                uiOutput("vesselspeed"),
                uiOutput("vesseldestination")
                )
              ),
  flow_layout( 
    p("Choose Vessel Type:"),
    p("Choose Vessel Name:")
  ),
  flow_layout(
    dropdown_input("vessel_type", unique(ships.data$ship_type), type = "selection multiple", value = ""),
    uiOutput("vesselname")
  )
)
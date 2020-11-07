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
              tabset(
                tabs = list(
                  list(menu = "Map", content = list(shiny::HTML("<center><h3>Longest Distance Map</center></h2>"), 
                                                    leafletOutput("vesselmap"))),
                  list(menu = "Data", content = DT::dataTableOutput("vesseldata"))
                )
              ),
              vertical_layout(
                uiOutput("vesseldist"),
                uiOutput("vesseltime"),
                uiOutput("vesselspeed"),
                uiOutput("vesseldestination"),
                shiny::HTML("<center><h3>Route Map</center></h2>"),
                leafletOutput("vesselmap2", height = "200")
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
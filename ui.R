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
                # Create 2 separate tabs - 1 for Map & 1 for Data
                tabs = list(
                  list(menu = "Map", 
                       content = list(
                         shiny::h3("Longest Distance Map", align = "center"),
                         shinycssloaders::withSpinner(leafletOutput("vesselmap", height = "500"))
                         )
                       ),
                  list(menu = "Data", content = list(
                    downloadButton(outputId = "downloadcsv", label = "Save as CSV", icon = icon("save")),
                    DT::dataTableOutput("vesseldata")
                    )
                  )
                )
              ),
              tabset(
                # Create information tab
                tabs = list(
                  list(menu = "Information", content = list(
                    vertical_layout(
                      uiOutput("vesseldist"),
                      uiOutput("vesseltime"),
                      uiOutput("vesselspeed"),
                      uiOutput("vesseldestination"),
                      shiny::h3("Route Map", align = "center"),
                      shinycssloaders::withSpinner(leafletOutput("vesselmap2", height = "200"))
                      )
                  ))
                )
              )
              ),
  # Dropdown modules
  dropdownUI("")
)
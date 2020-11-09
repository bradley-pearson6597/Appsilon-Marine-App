#####################################################################################################
###################################### UI FILE ####################################################
#######################################################################################################

ui <- semanticPage(
  title = "Marine App",
  tags$head(tags$style(".leaflet-control-attribution {
                                      display:none;
                       }
                       #-haversineform {
                       overflow-x:scroll;
                       width = 50px;
                       }"
  )),
  h1("Vessel Dashboard", align = "center"),
  semantic.dashboard::box(
    title = "Filters",
    ribbon = FALSE,
    # color = "violet",
    dropdownUI("")
  ),
  shiny::HTML("<br>"),
  split_layout(cell_widths = c("50%","50%"),
               style = "background-color:white",
               cell_args = "padding-left: 5px",
              tabset(
                # Create 2 separate tabs - 1 for Map & 1 for Data
                tabs = list(
                  list(menu = "Map", 
                       content = list(
                         shiny::h3("Longest Distance Map", align = "center"),
                         shinycssloaders::withSpinner(leafletOutput("vesselmap", height = "500"))
                         )
                       ),
                  list(menu = "Calculation",
                       content = list(
                         h3("Haversine Formula - Longest Distance Calculated"),
                         haversineUI("")
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
                    )
                  ),
                  list(menu = "Vessel Data", content = list(
                    downloadButton(outputId = "downloadcsv", label = "Save as CSV", icon = icon("save")),
                    DT::dataTableOutput("vesseldata")
                  )
                  )
                )
              )
              ),
  shiny::hr()
)
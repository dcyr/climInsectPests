

shinyUI(fluidPage(#navbarPage(#"Forest Pests", id="nav",
                   
      # tabPanel("Interactive Map - Time Series Explorer",
        div(class="outer",
            tags$head(
                # Include our custom CSS
                includeCSS("styles.css"),
                includeScript("gomap.js")
            ),
            leafletOutput("map", width="100%", height="100%"),
            
            # Shiny versions prior to 0.11 should use class="modal" instead.
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = FALSE, top = 0, right = 0, bottom = 0,
                          width = "25%",
                          h4("Time Series Explorer"),
                          style = 'overflow-y: scroll; opacity: 0.75',
                          
                          selectInput("spp",#
                                      label = "Select a species",
                                      choices = as.character(spp),
                                      selected = as.character(spp)[2]),#,
                          
                          uiOutput("varUI"),
                          
                          
                          radioButtons("level", label = "Choose level of spatial aggregation to display",
                                       choices = c("Ecozones", "Ecoregions"),
                                       selected = "Ecoregions"),
                          
                          sliderInput("range", step = 5,
                                      label = "Temporal scope (years):",
                                      min = as.Date("1872-01-01"), max = as.Date("2008-12-31"),
                                      value = c(as.Date("1970-01-01"), max = as.Date("2008-12-31")),
                                      timeFormat = "%Y", dragRange = T),
                          
                          
                          checkboxInput("pValAdjust", label = "Adjust p-values for multiple comparisons", value = FALSE),
                          

                          plotOutput("tsPlot", height = 175),
                          
                          #h4("Variable description"),
                          htmlOutput("varDescrip")
                          
                          

                          
            )

       )
                   
))

require(shiny)
require(leaflet)

# Choices for drop-downs
vars <- c("Spruce Budworm - Growth Rate" = "GrowthRate",
          "Mountain Pine Beetle - Logan Probability" = "LoganProbability",
          "Mountain Pine Beetle - Safranyik Probability" = "SafranyikProbability",
          "Forest Tent Caterpillar - Peak Adult Emergeance" = "PeakAdultEmergence",
          "Forest Tent Caterpillar - Peak Hatch Day" = "PeakHatchDay",
          "Forest Tent Caterpillar - Peak Pupation" = "PeakPupation"
)



shinyUI(
    navbarPage("Forest Pests - Climate Suitability", id="nav",
    
        tabPanel("Interactive Map - Time Series Explorer",
                 div(class="outer",
                     tags$head(
                         # Include our custom CSS
                         includeCSS("styles.css"),
                         includeScript("gomap.js")
                         ),
                     leafletOutput("map", width="100%", height="100%"),
                     
                     # Shiny versions prior to 0.11 should use class="modal" instead.
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                   width = "28%", height = "auto",
                                    h2("Time Series Explorer"),
                                   # Copy the line below to make a set of radio buttons
                                   radioButtons("level", label = "Choose level of aggregation to display",
                                                choices = c("Ecozones", "Ecoregions"), 
                                                selected = "Ecozones"),
                                   selectInput("var",
                                               label = "Choose a variable to display",
                                               choices = names(vars),
                                               selected = names(vars)[1]),#,
                                   # sliderInput("range",
                                   #             label = "Range of interest:",
                                   #             min = 1900, max = 2000, value = c(1900, 2000)),
                                   sliderInput("range",
                                               label = "Range of interest:",
                                               min = as.Date("1900-01-01"), max = as.Date("2000-12-31"),
                                               value = c(as.Date("1900-01-01"), max = as.Date("2000-12-31")),
                                               timeFormat = "%Y", dragRange = T),
                                   plotOutput("tsPlot", height = 200)
                                   )
                     )
                 )
        
    )
)


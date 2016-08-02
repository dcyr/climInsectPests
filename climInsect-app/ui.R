require(shiny)
require(leaflet)

# Choices for drop-downs


shinyUI(
    navbarPage("Forest Pests - Climate Suitability", id="nav",
    
        tabPanel("Interactive map",
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
                                   width = 400, height = "auto",
                                    h2("Explorer"),
                                   # Copy the line below to make a set of radio buttons
                                   radioButtons("level", label = "Choose level of aggregation to display",
                                                choices = c("Ecozones", "Ecoregions"), 
                                                selected = "Ecozones"),
                                   selectInput("var",
                                               label = "Choose a variable to display",
                                               choices = c("LoganProbability", "GrowthRate"),
                                               selected = "GrowthRate"),#,
                                   sliderInput("range",
                                               label = "Range of interest:",
                                               min = 1900, max = 2000, value = c(1900, 2000)),
                                   plotOutput("tsPlot", height = 200)
                                   )
                     )
                 )
        
    )
)
#  #,
# 
#   tabPanel("Data explorer",
#     fluidRow(
#       column(3,
#         selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
#       ),
#       column(3,
#         conditionalPanel("input.states",
#           selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
#         )
#       ),
#       column(3,
#         conditionalPanel("input.states",
#           selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
#         )
#       )
#     ),
#     fluidRow(
#       column(1,
#         numericInput("minScore", "Min score", min=0, max=100, value=0)
#       ),
#       column(1,
#         numericInput("maxScore", "Max score", min=0, max=100, value=100)
#       )
#     ),
#     hr(),
#     DT::dataTableOutput("ziptable")
#   ),
# 
#   conditionalPanel("false", icon("crosshair"))
# ))

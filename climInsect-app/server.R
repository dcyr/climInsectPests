rm(list = ls())
#setwd("~/Travail/SCF/InsectClimatePotential/climInsect-app/")
source("helpers.R")


shinyServer(function(input, output) {#, session) {

  ## Interactive Map ###########################################
    
    ## zone polygons
    poly <- reactive({
        if(input$level == "Ecozones") {
            return(ecozones)
        }
        if(input$level == "Ecoregions") {
            return(ecoregions)  
        }
    })
    ## zone dataframe
    df <- reactive({
        if(input$level == "Ecozones") {
            return(dfEcozones)
        }
        if(input$level == "Ecoregions") {
            return(dfEcoregions)  
        }
    })
    popup <- reactive({
        if(input$level == "Ecozones") {
            return(paste0("<strong>Name: </strong>",
                          poly()$ZONE_NAME))
        }
        if(input$level == "Ecoregions") {
            return(paste0("<strong>Name: </strong>",
                          poly()$REGION_NAM))
        }
    })
    ## zone dataframe
    w <- reactive({ifelse(input$level == "Ecozones", 2,1)})
    defaultZ <- reactive({ifelse(input$level == "Ecozones",
                                 "Boreal Shield East",
                                 "Abitibi Plains")})
    
    # Create the map
  
    output$map <- renderLeaflet({
        ## plotting map (constant features)
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                ) %>%
            setView(lng = -80, lat = 52, zoom = 4)
    })
    
    observe({
 
        cols <- leaflet::colorFactor(pal, poly()$ZONE_NAME)
        # plotting zone polygons
        leafletProxy("map", data = poly()) %>%
            clearShapes() %>%
            addPolygons(stroke = TRUE, weight = 1,
                        fillColor = ~cols(ZONE_NAME), fillOpacity = 0.4,
                        color = "grey", opacity = 0.75,
                        popup = popup())
    })
   

    # # # # # time series plot
    observe({
        if(input$level == "Ecozones") {
            
            zoneLevels <- levels(poly()$ZONE_NAME)
            
        }
        if(input$level == "Ecoregions") {
            zoneLevels <- levels(poly()$REGION_NAM)
        }
        
        event <- input$map_shape_click

        if(is.null(event)) {
            zoneName <- defaultZ()
        } else {
            event <- over(SpatialPoints(matrix(c(event$lng, event$lat),
                                               ncol = 2, nrow = 1),
                                        proj4string=poly()@proj4string),
                          poly())
            if(input$level == "Ecozones") {
                zoneName <- zoneLevels[as.numeric(event$ZONE_NAME)]
            }
            if(input$level == "Ecoregions") {
                zoneName <- zoneLevels[as.numeric(event$REGION_NAM)]
            }
            print(zoneName) 
        }

        output$tsPlot <- renderPlot({
            plotTs(df(),
                   z = zoneName,
                   var = input$var,
                   minYear = input$range[1],
                   maxYear = input$range[2])
        })
    })

})

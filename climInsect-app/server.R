#setwd("~/Travail/SCF/InsectClimatePotential/climInsect-app/")
#source("helpers.R")


shinyServer(function(input, output) {
    
    ### UI conditional components
    output$varUI <- renderUI({
        conditionalPanel(condition = "input.spp != 'Spruce Budworm'", 
                         selectInput("var", label = "Select a variable",
                                     choices = vars[[input$spp]], selected = vars[[input$spp]][1])
        )
    })  

    ### Map base layer
    output$map <- renderLeaflet({
        ## plotting map (constant features)
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng = -80, lat = 52, zoom = 4)
    })
    
    ### zone polygons
    poly <- reactive({
        if(input$level == "Ecozones") {
            x <- ecozones
            names(x) <- "zone"
            return(x)

        }
        if(input$level == "Ecoregions") {
            x <- ecoregions
            names(x)[which(names(x) == "REGION_NAM")] <- "zone"
            return(x)
        }
    })
    
    popup <- reactive({
        return(paste0("<strong>Name: </strong>",
                      poly()$zone))
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
    
    # ts trends and associated colors
    ts <- reactive({
        if(!exists("input$var")){
            var <- vars[[input$spp]][1] 
        } else {
            var <- input$var
        }
        
        ts <- df()[df()$date >= input$range[1] &
                       df()$date <= input$range[2], ]
        
        ts <- data.frame(date = ts[, "date"],
                         zone = ts[,2],
                         variable = ts[,var])
        ts <- ts[complete.cases(ts),]
        return(ts)
    })
    

    kendallResults <-  reactive({
        if("variable" %in% colnames(ts())) {
            kendallResults <- by(ts()$variable, ts()$zone,
                                 function(x) Kendall::MannKendall(x))
            return(kendallResults)   
        }
    })


    ## may need to modify the following if actual time series are needed
    kTau <- reactive({
        kTau <- sapply(kendallResults(), function(x) as.numeric(x[1]))
        #reordering according to polygon levels
        kTau <- kTau[match(levels(poly()$zone), names(kTau))]
        n <- names(kTau)
        kTau <- as.numeric(kTau)
        names(kTau) <- n
        return(kTau)
    })
    
    kPval <- reactive({
        kPval <- sapply(kendallResults(), function(x) as.numeric(x[2]))
        #reordering according to polygon levels
        kPval <- kPval[match(levels(poly()$zone), names(kPval))]
        n <- names(kPval)
        kPval <- as.numeric(kPval)
        names(kPval) <- n
        if(input$pValAdjust) { # adjusting pValues for multiple comparisons
            kPval <- p.adjust(kPval, method = "bonferroni", n = length(kPval))
        }
        return(kPval)
    })
    
    
    zoneName <- reactive({
        defaultZ <- ifelse(input$level == "Ecozones",
                                     "Boreal Shield East",
                                     "Abitibi Plains")
        
        zoneLevels <- levels(poly()$zone)
        event <- input$map_shape_click
        if(is.null(event)) {
            zoneName <- defaultZ
        } else {
            event <- over(SpatialPoints(matrix(c(event$lng, event$lat),
                                               ncol = 2, nrow = 1),
                                        proj4string=poly()@proj4string),
                          poly())
            zoneName <- zoneLevels[as.numeric(event$zone)]
        }
        return(zoneName)
    })

    ### define color palettes, plot polygons
    observe({
        col <- c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")#c("red", "lightred", "grey", "lightblue","blue")
        labels <- c("increasing (significant; p-value < 0.05)", "positive trend (non-significant)",
                    "weak or no trend",
                    "negative (non-significant)", "decreasing (significant; p-value < 0.05)")
        
        pal <- rep(col[3], length(kPval()))
        signific <- which(kPval() < 0.05)
        ### those that are halfway signif are considered "weak" trends
        thresh <- min(abs(kTau()[signific]), na.rm = T)/2
        increasing <-  which(kTau() > thresh)
        decreasing <-  which(kTau() < -thresh)
        pal[increasing] <- col[2]
        pal[decreasing] <- col[4]
        pal[intersect(signific, increasing)] <- col[1]
        pal[intersect(signific, decreasing)] <- col[5]

        cols <- leaflet::colorFactor(pal, poly()$zone)
        
        # plotting zone polygons
        proxy <- leafletProxy("map", data = poly()) %>%
            clearShapes() %>%
            clearControls() %>%
            addPolygons(stroke = TRUE, weight = 1,
                        fillColor = ~cols(zone), fillOpacity = 0.4,
                        color = "grey", opacity = 1,
                        popup = popup()) #%>%
        
            proxy %>%
                addLegend("bottomleft", color = col, labels = labels,
                          title = "Legend",
                          opacity = 0.4)

    })
    

    # time series plot
    observe({
    
        ts <- ts()
        
        output$tsPlot <- renderPlot({
       
            ## redondancy with plotTs, allow to better control control the axis
            ts <- filter(ts, zone %in% zoneName())

            p <- plotTs(df = ts,
                        z = zoneName(),
                        var = input$var,
                        minYear = input$range[1],
                        maxYear = input$range[2])
            ylim <- ggplot_build(p)$panel$ranges[[1]]$y.range 
             p <- p +
                annotate("text", label = c(paste("Kendall's tau =", round(kTau()[zoneName()], 3)),
                                           paste("2-sided p-value =",
                                                 ifelse(kPval()[zoneName()]<0.0001,
                                                        "< 0.0001", round(kPval()[zoneName()], 3)))),
                         x = rep(input$range[2],2),
                         y = c(ylim[2], ylim[1]+(0.90*diff(ylim))),
                         hjust = 1) 
            p
        })

        
    })
    
    observe({
        varSpp <- vars[[input$spp]]
        output$varDescrip <- renderText(paste("<strong>", paste(input$spp, names(varSpp)[which(varSpp == input$var)], sep = " - "), "</strong><br>", 
                                              "Quote from", "<a href=", descrip[[input$var]]["url"],
                                              "target='_blank'>",
                                              descrip[[input$var]]["source"],
                                              "</a>", ":<br>",
                                              descrip[[input$var]]["descrip"]))
    })
    
})

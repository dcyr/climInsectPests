#library(maps)
#library(mapproj)

library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
require(leaflet)
require(ggplot2)
require(rgdal)
### loading shapefiles
ecozones <- get(load("./data/ecozonePoly.RData"))
ecoregions <- get(load("./data/ecoregionPoly.RData"))
### loading dataframes
dfEcozones <- read.csv("./data/dfEcozones.csv")
dfEcoregions <- read.csv("./data/dfEcoregions.csv")

vars <- c("Mountain Pine Beetle - Logan Probability" = "LoganProbability",
          "Spruce Budworm - Growth Rate" = "GrowthRate")


### reordering levels for cleaner display

ecozones$ZONE_NAME <- factor(ecozones$ZONE_NAME, levels = c("MixedWood Plain",
                                                            "Atlantic Maritime",
                                                            "Boreal Shield West",
                                                            "Boreal Shield East",
                                                            "Prairie",
                                                            "Boreal Plain",
                                                            "Montane Cordillera",
                                                            "Pacific Maritime",
                                                            "Boreal Cordillera",
                                                            "Taiga Shield",
                                                            "Taiga Plain",
                                                            "Taiga Cordillera",
                                                            "Hudson Plain",
                                                            "Southern Arctic",
                                                            "Northern Arctic",
                                                            "Arctic Cordillera"))
ecoregions$ZONE_NAME <- factor(ecoregions$ZONE_NAME, levels = c("MixedWood Plain",
                                                                "Atlantic Maritime",
                                                                "Boreal Shield West",
                                                                "Boreal Shield East",
                                                                "Prairie",
                                                                "Boreal Plain",
                                                                "Montane Cordillera",
                                                                "Pacific Maritime",
                                                                "Boreal Cordillera",
                                                                "Taiga Shield",
                                                                "Taiga Plain",
                                                                "Taiga Cordillera",
                                                                "Hudson Plain",
                                                                "Southern Arctic",
                                                                "Northern Arctic",
                                                                "Arctic Cordillera"))
### assigning colors
pal <- c("palegreen",
          "seagreen3",
          "tan2",
          "palegreen4",
          "navajowhite1",
          "darkseagreen2",
          "darkkhaki",
          "darkolivegreen4",
          "burlywood4",
          "bisque3",
          "burlywood3",
          "darkslategray4",
          "lemonchiffon4",
          "lightblue1",
          "lightcyan1",
          "lightblue3")



plotTs <- function(df = df, z, var, minYear, maxYear) {
    # raw time series
    ts <- df[df$year >= minYear &
                 df$year <= maxYear &
                 df[,2] == z, ]
    
    ts <- ts[, c("year", var)]
    ts <- ts[complete.cases(ts),]

    tsPlot <- ggplot(ts, aes_string("year", var)) +
        geom_line() +
        labs(y = names(vars)[which(vars == var)],
             title = "Add title here")
        
    return(tsPlot)
}


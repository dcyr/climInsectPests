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

dfEcozones[, "date"] <- as.Date(paste0(dfEcozones$year, "-07-01"))
dfEcoregions[, "date"] <-  as.Date(paste0(dfEcoregions$year, "-07-01"))

vars <- c("Spruce Budworm - Growth Rate" = "GrowthRate",
          "Mountain Pine Beetle - Logan Probability" = "LoganProbability",
          "Mountain Pine Beetle - Safranyik Probability" = "SafranyikProbability",
          "Forest Tent Caterpillar - Peak Adult Emergeance" = "PeakAdultEmergence",
          "Forest Tent Caterpillar - Peak Hatch Day" = "PeakHatchDay",
          "Forest Tent Caterpillar - Peak Pupation" = "PeakPupation"
)


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



plotTs <- function(df = df, z, var, minYear, maxYear,
                   level = 0.95, method = "lm") {
    require(Hmisc) # for stat_summary(fun.data = )
    # raw time series
    ts <- df[df$date >= minYear &
                 df$date <= maxYear &
                 df[,2] == z, ]
    ts <- ts[, c("date", var)]
    ts <- ts[complete.cases(ts),]
    ## plot lim
    ylim <-range(ts[,var])
    
    
    kendallResults <- Kendall::MannKendall(x = ts[,var])
    kTau <- round(as.numeric(kendallResults[1]), 3)
    kPval <- round(as.numeric(kendallResults[2]), 4)
    
    tsPlot <- ggplot(ts, aes_string("date", var)) +
        geom_line() +
        stat_smooth(method = method, level = level) +
        labs(y = vars[which(vars == var)],
             title = paste0(z, "\n", names(vars)[which(vars == var)])) +
        annotate("text", label = c(paste("Kendall's tau =", kTau),
                                   paste("2-sided pvalue =", kPval)),
                 x = rep(maxYear,2),
                 y = c(ylim[2], ylim[1]+(0.90*diff(ylim))),
                 hjust = 1)
        
    return(tsPlot)
}


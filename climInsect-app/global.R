require(shiny)
require(leaflet)
require(ggplot2)
require(rgdal)
require(dplyr)

# Choices for drop-downs
spp <- c("Mountain Pine Beetle", "Spruce Budworm","Forest Tent Caterpillar")
#names(spp) <- c("mpb", "sbw", "ftc")


vars <- list("Mountain Pine Beetle" = c("Adaptive seasonality" = "LoganProbability",
                       "Climate suitability" = "SafranyikProbability"),
             "Spruce Budworm" = c("Growth Rate" = "GrowthRate" ),
             "Forest Tent Caterpillar" = c("Peak Adult Emergeance" = "PeakAdultEmergence",
                       "Peak Hatch Day" = "PeakHatchDay",
                       "Peak Pupation" = "PeakPupation")
)


### loading shapefiles
ecozones <- get(load("./data/ecozonePolyFor.RData"))
ecoregions <- get(load("./data/ecoregionPolyFor.RData"))

ecozoneSubsample <- as.character(ecozones$ZONE_NAME)
ecoregionSubsample <- as.character(ecoregions$REGION_NAM)

## loading dataframes
dfEcozones <- read.csv("./data/dfEcozones.csv")
dfEcoregions <- read.csv("./data/dfEcoregions.csv")

### keeping only forested ecozones
dfEcozones <- dfEcozones %>%
    filter(EcozoneName %in% ecozoneSubsample)
dfEcoregions <- dfEcoregions %>%
    filter(EcoregionName %in% ecoregionSubsample)


dfEcozones[, "date"] <- as.Date(paste0(dfEcozones$year, "-07-01"))
dfEcoregions[, "date"] <-  as.Date(paste0(dfEcoregions$year, "-07-01"))


### assigning colors (all ecozones)
palDefault <- c("palegreen",
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
    require(dplyr)
    df <- as.data.frame(df)
    ## preparing ts to plot
    ts <- df %>%
        filter(zone %in% z &
                   date >= minYear &
                   date <= maxYear)

    tsPlot <- ggplot(ts, aes(x = date, y = variable)) +
        geom_line() +
        stat_smooth(method = method, level = level) +
        labs(x = "Year",
             y = var,
             title = paste0(z, "\n", var))

    return(tsPlot)
}

descrip <-list(SafranyikProbability = list(descrip = paste('"<i>The Safranyik et al. model of climate suitability is based on six key climatic variables',
                                                       'that jointly determine beetle attack success, development, and survival [...].',
                                                       'Heat accumulation during the growing season determines areas where the beetle can complete', 
                                                       'its life cycle in one year or less. The minimum winter temperature determines the winter', 
                                                       'survival rate. The mean maximum August temperature determines the level of flight activity',
                                                       'of attacking beetles. Three variables address the moisture balance that influences tree',
                                                       'vigour and thus beetle attack success and brood survival. The model produces classes of ',
                                                       'climate suitability that were calibrated using historical-outbreak records from British Columbia.',
                                                       'The basic biological assumption is that the climate-suitability classes [...] based', 
                                                       'on the relative frequency of joint occurrence ofthese six climatic variables are close',
                                                       'indirect measures of brood establishment and survival. Because this model was developed',
                                                       'with reference to lodgepole pine, there is some uncertainty about whether the water-deficit',
                                                       'conditions as expressed in the model represent a similar biological condition in jack pine',
                                                       'in the boreal forest. Furthermore, it should be noted that the Safranyik et al. model does',
                                                       'not consider completion of development in less than one year to have a negative impact',
                                                       'on population growth.</i>"<br>'),
                                           source = 'Safranyik <i>et al</i>. 2012',
                                           url = "http://journals.cambridge.org/abstract_S0008347X00001206"),
               LoganProbability = list(descrip = paste('"<i>The main premise in the Logan et al. model is that predominantly univoltine seasonality is',
                                                   'necessary for persistence of populations and development of epidemics. The model determines',
                                                   'adaptive seasonality based on the criteria of univoltism and a stable and viable oviposition date [...].',
                                                   'Mountain pine beetle has no diapause to functionally synchronize populations with critical',
                                                   'phenological events; its entire seasonal development is under direct temperature control [...].</i>"<br>'),
                                       source = 'Safranyik <i>et al</i>. 2012',
                                       url = "http://journals.cambridge.org/abstract_S0008347X00001206"),
               GrowthRate = list(descrip = paste('"<i>Temperature affects two main processes in spruce budworm’s life history: development and consumption', 
                                             'of energy reserves during diapause. Through its effect on development rates, temperature determines',
                                             'whether or not the insect can reach the cold-hardy diapausing stage before the onset of winter, which',
                                             'is of concern at the cold extreme of the insect’s range. At the warm extreme, temperature determines the',
                                             'rate at which the energy reserves of diapausing larvae are utilized, and this in turn determines their',
                                             'overwintering survival [...]."</i>'),
                                 source = 'Régnière <i>et al.</i> 2012.',
                                 url = "http://www.springerlink.com/index/10.1007/s10530-010-9918-1"),
               PeakAdultEmergence = list(descrip = paste('A description will be added soon'),
                                         source = "Cooke <i>et al</i>, in prep",
                                         url = ""),
               PeakHatchDay = list(descrip = paste('A description will be added soon'),
                                         source = "Cooke <i>et al</i>, in prep",
                                         url = ""),
               PeakPupation = list(descrip = paste('A description will be added soon'),
                                         source = "Cooke <i>et al</i>, in prep",
                                         url = "")
               )



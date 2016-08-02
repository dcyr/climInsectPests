rm(list = ls())

setwd("/media/dcyr/Windows7_OS/Travail/SCF/InsectClimatePotential")
processedOutputs <- "./processedOutputs"
outputDir <- paste(getwd(), Sys.Date(), sep="/")
dir.create(outputDir)

dfEcozones <- read.csv((paste(processedOutputs, "dfEcozones.csv", sep = "/")))

require(trend)
require(dplyr)

foo <- dfEcozones %>%
    filter(EcozoneName ==
               Boreal Shield East)
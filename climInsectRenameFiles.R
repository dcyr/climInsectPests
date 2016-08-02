rm(list=ls())
setwd("/media/dcyr/Windows7_OS/Travail/SCF/InsectClimatePotential/")
dataFolder <- paste(getwd(), "data", sep="/")
wd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wd)
setwd(wd)
rm(wd)

### renaming files (if necessary)
x <- list.files(dataFolder)
oldnames <- x
x <- gsub("(Export \\(Simulation_)|\\)", "", x)
x <- paste0(substr(x, 1, nchar(x)-9), "-", substr(x, nchar(x)-7, nchar(x)))
newnames <- x
file.rename(paste(dataFolder, oldnames, sep="/"), paste(dataFolder, newnames, sep="/"))

insectClimatePotential <- list()
for (i in seq_along(x)) {
    filename <- paste(dataFolder, x[i], sep="/")
    basename
    simInfo <- strsplit(basename(filename), "_")
    insect <- simInfo[[1]][1]
    climGrid <- simInfo[[1]][2]
    period <- gsub(".csv", "", simInfo[[1]][4])
    data <- read.csv2(filename)
    data <- data.frame(insect = insect,
                       climGrid = climGrid,
                       data)
    summary(data)
}
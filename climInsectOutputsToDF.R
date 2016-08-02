rm(list=ls())

setwd("/media/dcyr/Windows7_OS/Travail/SCF/InsectClimatePotential")
inputDir <- "./data"
outputDir <- paste(getwd(), Sys.Date(), sep="/")
dir.create(outputDir)

x <- list.files(inputDir)

fileInfo <- gsub(".csv", "", x)
fileInfo <- strsplit(fileInfo, "_")

pest <- as.character(lapply(fileInfo, function(x) x[1]))
climateGrid <- as.character(lapply(fileInfo, function(x) x[2]))
period <- as.character(lapply(fileInfo, function(x) x[4]))


outputTmp <- list()
gridID <- character()
lat <- long <- numeric()
for (i in seq_along(x)) {
    fileFullName <- paste(inputDir, x[i], sep="/")
    tmp <- read.csv2(fileFullName)
    tmp[,"pestCode"] <- pest[i]
    tmp[,"climGrid"] <- climateGrid[i]
    gridID <- append(gridID, as.character(tmp$Name))
    if ("Latitude" %in% colnames(tmp)) {
        lat <- append(lat, tmp$Latitude)
        long <-  append(long, tmp$Longitude)
    } else {
        lat <- append(lat, rep(NA, nrow(tmp)))
        long <-  append(long, rep(NA, nrow(tmp)))
    }
    outputTmp[[i]] <- tmp
}

# creating reference spatial grid

rasterID <- data.frame(value = gridID, Lat = lat, Long = long)
rasterID <- rasterID[complete.cases(rasterID),]

require(dplyr)
rasterID <- rasterID %>%
    distinct %>%
    arrange(value)

# for some reason, 'rasterFromXYZ doesn't accept a factor as values
rasterID[,"ID"] <- 1:nrow(rasterID)
#
require(raster)
r <- rasterFromXYZ(rasterID[,c("Long", "Lat", "ID")], digits = 5,
                    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
r <- ratify(r)
values(r) <- refRasterID <- rasterID[values(r), "value"]
#writeRaster(r, file = "refRaster.tif", overwrite = T)

### function that puts data.frame into raster stacks
###
dfToRasterStack <- function (df, refRaster, dfID, refRasterID, time.var,  variable.names){
    df <- as.data.frame(df)
    ## selecting useful colums
    df <- df[, which(colnames(df) %in% c(dfID, time.var, variable.names))]
    ## creating raster objects
    r <- refRaster
    tmpRaster <- raster(nrow = nrow(r), ncol = ncol(r))
    extent(tmpRaster) <- extent(r)
        ## looping through time steps
    results <- list()
    for (v in variable.names) { # v <- variable.names[1]
        if (time.var %in% (colnames(df))) {
            tmpVar <-  df[, c(dfID, time.var, v)]
            timesteps <- unique(as.data.frame(df)[,time.var])

            for (y in timesteps){# y <- timesteps[1]
                tmpRaster[] <- NA
                tmp <- tmpVar[tmpVar[, time.var] == y,]
                tmpRaster[]  <- tmp[match(refRasterID, tmp$Name), v]
                if(y ==  timesteps[1]) {
                    tmpStack <- tmpRaster
                } else {
                    tmpStack <- addLayer(tmpStack, tmpRaster)
                }
                layerID <- paste(v, y, sep=".")
                print(layerID)
            }
            names(tmpStack) <- paste(v, timesteps, sep=".")

        } else {
            tmpVar <-  df[, c(dfID, v)]
            tmpRaster[] <- NA
            tmpRaster <- tmpVar[match(refRasterID, tmpVar$Name), v]
            tmpStack  <- tmpRaster
            names(tmpStack) <- v
        }
        results[[v]] <- tmpStack

    }
    return(results)
}
#
# test <- dfToRasterStack(df = spDT,
#                         refRaster = refRaster,
#                         dfID = dfID,
#                         refRasterID = refRasterID,
#                         time.var = time.var,
#                         variable.names = variable.names)




# fetching values and storing them into raster stack
require(data.table) # for rbindlist
outputs <- list()
for (sp in unique(pest)) { #sp <- pest[1]

#    require(data.table)
    index <- which(pest == sp)
    tmp <- list()
    for (i in seq_along(index)) {
        tmp[[i]] <- outputTmp[[index[i]]]
    }
    spDT <- rbindlist(tmp, fill=TRUE)
    colnames(spDT) <- gsub("\\.", "", colnames(spDT))

    ### spatializing data.frame
    dfID <- "Name"
    time.var = "Year"
    if (sp == "FTC") {
        variable.names <- c("PeakHatchDay", "PeakPupation", "PeakAdultEmergence")
    }
    if (sp == "MPB") {
        variable.names = c("LoganProbability", "SafranyikProbability", "ColdToleranceSurvival")
    }
    if (sp == "SBW") {
        variable.names = "GrowthRate"
    }
    if (sp == "GM") {
        variable.names = "Viability"
    }

    pestStackList <- dfToRasterStack(df = spDT,
                            refRaster = r,
                            dfID = dfID,
                            refRasterID = refRasterID,
                            time.var = time.var,
                            variable.names = variable.names)
    ### saving data.frame
    write.csv(spDT, file = paste0(outputDir, "/", sp, ".csv"), row.names = FALSE)
    ### saving rasterStack
    stackName <- paste(sp, gsub("\\.", "", names(pestStackList)), sep="_")
    for (i in seq_along(stackName)) {
        assign(stackName[i], pestStackList[[i]])
        tmp <- get(stackName[i])
        save(tmp, file = paste0(outputDir, "/", stackName[i],".RData"))
    }
}

rm(list=ls())
setwd("~/Travail/SCF/InsectClimatePotential")

require(rgdal)
require(maptools)
require(plyr)
require(dplyr)
require(ggplot2)
require(raster)
require(sp)
require(rgeos)
require(colorRamps)
require(rgdal)

project <- TRUE
#processedOutputs <- "./processedOutputs"
outputDir <- paste(getwd(), Sys.Date(), sep="/")
dir.create(outputDir)

sigFolder <- paste(outputDir, "gis", sep="/")
dir.create(sigFolder)

# outputFiles <- list.files(processedOutputs)
# stackFiles <- outputFiles[grep(".RData", outputFiles)]
# ##
# ##
# stackInfo <- strsplit(stackFiles, "_")
# spp <- unlist(lapply(stackInfo, function(x) x[1]))
# stackNames <- unlist(lapply(stackInfo, function(x) x[2]))
# stackNames <- gsub(".RData", "", stackNames)


# ############################
# ############################
# ####
# ####  Preparing shapefiles and rasters for downstream analyses
# ####
# ############################
# ############################

##### North-America shapefiles
can1 <- getData('GADM', country="CAN", level=1) ## level 1: provinces
usa1 <- getData('GADM', country="USA", level=1) ## level 1: provinces
usa1 <- usa1[usa1$NAME_1 != "Hawaii",]
## simplified shapefile for faster processing + plotting
can <- gSimplify(can1, tol = 0.1, topologyPreserve=T)
usa <- gSimplify(usa1, tol = 0.1, topologyPreserve=T)
## reference raster Can + Continental US
refRaster <- raster("./gis/refRaster.tif")

####################
##### loading ecoregions and ecozones
ecoregions <- readOGR(dsn = "./gis/Ecoregions", layer = "ecoregions")
ecozones <- readOGR(dsn = "./gis/Ecozones", layer = "ecozones")



# ecodistricts <- readOGR(dsn = "./gis/Ecodistricts", layer = "ecodistricts")
# ecoprovinces <-  readOGR(dsn = "./gis/Ecoprovinces", layer = "ecoprovinces")
## extracting ecozone names
ecozoneNames <- distinct(as.data.frame(ecozones[, c("ECOZONE", "ZONE_NAME")]))
### matching ecoregion table
ecozoneNames <- as.character(ecozoneNames[match(ecoregions$ECOZONE, ecozoneNames$ECOZONE), "ZONE_NAME"])
### renaming / correcting errors
ecozoneNames <- ifelse(ecozoneNames == "Boreal PLain", "Boreal Plain", ecozoneNames)
ecozoneNames <-  ifelse(ecozoneNames == "Boreal Shield",
                        ifelse(ecoregions$ECOREGION > 95, "Boreal Shield East", "Boreal Shield West"),
                        ecozoneNames)

### dissolving polygons from ecoregions (to separate "Boreal Shield" East and West)
ecoregions$ZONE_NAME <- ecozoneNames
foo <- as.data.frame(ecoregions)
ecozones <- gUnaryUnion(ecoregions, id = ecoregions@data$ZONE_NAME)


#foo <- SpatialPolygonsDataFrame(ecozones, data.frame("ZONE_NAME" = row.names(ecozones)))
# If you want to recreate an object with a data frame
# make sure row names match
foo <- row.names(ecozones)
row.names(ecozones) <- as.character(1:length(ecozones))
foo <- as.data.frame(foo)
colnames(foo) <- "ZONE_NAME"  
ecozones <- SpatialPolygonsDataFrame(ecozones, foo)

### reordering levels for cleaner display
ecozoneLevels <-c("MixedWood Plain",
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
                  "Arctic Cordillera")


ecozones$ZONE_NAME <- factor(ecozones$ZONE_NAME, levels = ecozoneLevels)
ecoregions$ZONE_NAME <- factor(ecoregions$ZONE_NAME, levels = ecozoneLevels)


# write new polygons to disk
save(ecozones, file = "ecozonePoly.RData")
save(ecoregions, file = "ecoregionPoly.RData")


# extract forested ecozones
ecozoneSubsample <- c("MixedWood Plain",
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
                      "Hudson Plain")

ecoregions <- ecoregions[ecoregions$ZONE_NAME %in% ecozoneSubsample,]
ecozones <- ecozones[ecozones$ZONE_NAME %in% ecozoneSubsample,]


### extract attribute data frame (disappears during following steps)
ZONE_NAME <- ecozones$ZONE_NAME
ZONE_NAME <- as.data.frame(ZONE_NAME)

### simplifying polygons for faster plotting
ecozones <- gSimplify(ecozones, tol = 0.005, topologyPreserve=T)
row.names(ZONE_NAME) <- names(ecozones)
ecozones <- SpatialPolygonsDataFrame(ecozones, ZONE_NAME)
foo <- as.data.frame(ecoregions)
ecoregions <- gSimplify(ecoregions, tol = 0.005, topologyPreserve=T)
ecoregions <- SpatialPolygonsDataFrame(ecoregions, foo)

# write new polygons to disk
save(ecozones, file = "ecozonePolyFor.RData")
save(ecoregions, file = "ecoregionPolyFor.RData")

## getwd()
if(project) {
    CanUsCRS <- CRS("+init=epsg:2163") ### US National Atlas Equal Area Lambert Azimuthal Equal Area (Spherical)
    can <- spTransform(can, CanUsCRS)
    usa <- spTransform(usa, CanUsCRS)
    ecoregions <- spTransform(ecoregions, CanUsCRS)
}

can_us <- rbind(can, usa, makeUniqueIDs = TRUE)

### rasterize can_us
can_usRas <- raster(ncol=500, nrow=500) ### 500 x 500 works well for animations and small figures.
extent(can_usRas) <- extent(can_us)
can_usRas <- rasterize(can_us, can_usRas)
ecoregionRas <- rasterize(ecoregions, can_usRas, field = c("ECOREGION", "REGION_NAM", "ZONE_NAME"))



# foo <- ecoregionRas
# ecoregionRas <- foo
rat1 <- levels(ecoregionRas)[[1]]
ecoregionRas <- ratify(ecoregionRas)
rat2 <- levels(ecoregionRas)[[1]]


rat <- data.frame(ID = 1:nrow(rat2),
                  ECOREGION =  rat2$ID,
                  rat1[match(rat2$ID, rat1$ECOREGION), c("REGION_NAM", "ZONE_NAME")])
rownames(rat) <- 1:nrow(rat)

levels(ecoregionRas) <- rat

## replacing raster values
# values(ecoregionRas) <- match(values(ecoregionRas), rat2$ID)
# rat <- data.frame(ID = 1:nrow(rat2), rat1)
# rat <- rat1
# rat <- distinct(rat)
# rat <- rat[order(rat$ID),]
# rownames(rat) <- rat$ID
# names(rat)[names(rat) == "ECOREGION"] <- "ID"
# ecoregionRas <- ratify(ecoregionRas)
# levels(ecoregionRas) <- rat

val <- values(ecoregionRas)
values(ecoregionRas) <- rat[match(val, rat$ECOREGION),"ID"]


### transforming shapefile to fortified shapefiles
can_usF <- fortify(can_us, region = "Polygons")
canF <- fortify(can, region = "Polygons")
usaF <- fortify(usa, region = "Polygons")
ecozonesF <- fortify(ecoregions, region = "ZONE_NAME")
ecoregionsF <- fortify(ecoregions, region = "REGION_NAM")

##################
##################
### saving object to allow skipping the previous section
save(can_usF, file = paste(sigFolder, "can_usF.RData", sep = "/"))
save(canF, file = paste(sigFolder, "canF.RData", sep = "/"))
save(usaF, file = paste(sigFolder, "usaF.RData", sep = "/"))
save(can_usRas, file = paste(sigFolder, "can_usRas.RData", sep = "/"))
save(ecoregions, file = paste(sigFolder, "ecoregions.RData", sep = "/"))
save(ecozonesF, file = paste(sigFolder, "ecozonesF.RData", sep = "/"))
save(ecoregionsF, file = paste(sigFolder, "ecoregionsF.RData", sep = "/"))
save(ecoregionRas, file = paste(sigFolder, "ecoregionRas.RData", sep = "/"))
############################
############################

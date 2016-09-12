# Static variables for station panel

rm(list=ls())

library(raster)
library(maptools)
library(rgdal)
library(sp)
library(readstata13)
library(sp)
library(rgeos)

# SOurce of Costal line and water bodies/River data
# https://www.soest.hawaii.edu/pwessel/gshhg/
# https://www.soest.hawaii.edu/pwessel/gshhg/README.TXT

data_path <- "E:/brazil_lst/data"
out_path <- 'E:/brazil_lst/data'
setwd(data_path)

#read in random sample
pts <- read.dta13("lst_station_points.dta")

ransample <- pts # static variables will be added here
#make pts spatial points
pts$lon <- pts$modislon
pts$lat <- pts$modislat

coordinates(pts)=~lon+lat
proj4string(pts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Write this shapefile

br <- getData('GADM', country='Brazil', level=0)
lc <- raster("modis1kmlc_2002.tif")
bb <- c(-84.06186, -28.3912, -60.3116, 16.51572) # Entire South America (to get the full coast line in all directions)


gcoast <- readShapeLines("ne_10m_coastline.shp")
crs(gcoast) <- crs(lc) 

gcoast <- crop(gcoast,bb)
plot(gcoast)

m <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# gcoast MW projection
gcoastMW<- spTransform(gcoast, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
plot(gcoastMW)

# points in MW projection
ptsMW <- spTransform(pts, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
plot(ptsMW, add=TRUE)


cdist <- rgeos::gDistance(ptsMW, gcoastMW, byid=TRUE)
cdist <- cdist/1000
cdist1 <- apply(cdist,2,min)
ransample$cdist <- round(cdist1, digits=2)

# Major River: https://www.soest.hawaii.edu/pwessel/gshhg/
# Distance from Major Rivers
rshp <-  readShapeLines("WDBII_river_h_L04.shp")
crs(rshp) <- crs(lc) 
rshp <- crop(rshp,bb)
plot(rshp)

m <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# wshp MW projection
rshpMW<- spTransform(rshp, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
plot(rshpMW)

# points in MW projection
ptsMW <- spTransform(pts, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
plot(ptsMW, add=TRUE)

rdist <- rgeos::gDistance(ptsMW, rshpMW, byid=TRUE)
rdist <- rdist/1000
rdist1 <- apply(rdist,2,min)
ransample$wdist <- round(rdist1, digits=2)

# Elevatiuon, Slope, and Aspect 

elevation <- raster ("SRTM_1km.tif")
elevation <- crop(elevation,bb)
elevation[is.na(elevation)] <- 0

slopeAspect <- terrain(elevation, opt=c('slope', 'aspect'), unit='degrees')
slope <- raster(slopeAspect,1);
aspect <- raster(slopeAspect,2);
setwd(data_path)

state_ras <- raster("stateIDraster.tif")
biome_ras <- raster("biomeIDraster.tif")

ransample$ele <- extract(elevation,pts, method='simple')
ransample$slope <-extract(slope,pts, method='simple')
ransample$aspect <- extract(aspect,pts, method='simple')

ransample$state_id <- extract(state_ras,pts, method='simple')
ransample$biome_id <- extract(biome_ras,pts, method='simple')

ransample$state_id1 <- extract(state_ras,pts, buffer = 5000,fun=max)
ransample$biome_id1 <- extract(biome_ras,pts, buffer = 5000,fun=max)
ransample$state_id[is.na(ransample$state_id)] <- ransample$state_id1[is.na(ransample$state_id)]
ransample$biome_id[is.na(ransample$biome_id)] <- ransample$biome_id1[is.na(ransample$biome_id)]

ransample <- ransample[,-which(names(ransample) %in% c("state_id1","biome_id1"))]

save.dta13(ransample,"lst_station_points_withstaticvar.dta")




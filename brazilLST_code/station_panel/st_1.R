#Get all GSOD stations in Brazil; find all stations with lst pixels with at least 50% Hansen GFC for four years between 2002-2013
#Download gsod station data and template to extract all relevant lst values.

rm(list = ls())
library(dplyr)
library(data.table)
library(raster)
library(maptools)
library(stringr)
library(XML)
library(raster)
library(sp)
library(maptools)
library(rgeos)
library(rgdal)
library(GSODR)
library(rnoaa)
library(stringr)
library(readstata13)
options(noaakey = "AQQeOudEPwnmhjQRhnUPUtojRNNnVJPR")

# If necessary change this path and set path to the folder below
raster_path <- "E:/brazil_lst/data/forest_universe"
out_path <- 'E:/brazil_lst/data'

setwd(out_path)

# Get GSOD stations and subset Brazil

stations_br <- stations
stations_br <- subset(stations_br, CTRY == 'BR')

#Subset out early years

stations_br <- subset(stations_br, END >= 20020101)
stations_br <- subset(stations_br, !is.na(LON))
stations_br <- subset(stations_br, !is.na(LAT))

#Make spatial

stations_br$x <- stations_br$LON
stations_br$y <- stations_br$LAT

stations_brshp <- stations_br
coordinates(stations_brshp)=~x+y
proj4string(stations_brshp)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Keep only stations in Brazil

br <- getData('GADM', country='Brazil', level=0)

br_ext <- extent(br)
br_ras <- raster(br)

stations_brshp <- stations_brshp[br, ]

br_stn <- stations_brshp

#Create 2 km buffer around stations

allstMW<- spTransform(br_stn, CRS("+proj=moll +lon_0=0 +x_0=0 
                                  +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # reproject station location to Mollweide
plot(allstMW)

pbuf2km <- gBuffer(allstMW, width=2000, byid=TRUE)
plot(pbuf2km)

# forest samples - this is a binary map of all lst pixels with at least 50% Hansen GFC cover for at least 4 years

in_rast <- raster(paste(raster_path,'/pfb1_forest_universe.tif', sep ='')) 
in_pts <- rasterToPoints(in_rast)
in_pts <- data.frame(in_pts)
names(in_pts) <- c("x","y","run_length")
in_pts <- subset(in_pts,  run_length >= 4)
in_pts$lon <- in_pts$x
in_pts$lat <- in_pts$y
ll <- length(in_pts$lat)
in_pts$id <- seq(1, ll, 1)

coordinates(in_pts)=~x+y
proj4string(in_pts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

in_ptsMW<- spTransform(in_pts, CRS("+proj=moll +lon_0=0 +x_0=0 
                                   +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # reproject station location to Mollweide


# Clip only points within 2km station buffer

lst_pts <- in_ptsMW[pbuf2km, ]
lst_df <- data.frame(lst_pts)

#Get station ID for points within 2km buffer - this provides a station ID to join lst timeseries and lst downloads

lst_pts_out <- over(lst_pts, pbuf2km)
lst_pts_out$modislat <- lst_df$lat
lst_pts_out$modislon <- lst_df$lon

#get gsod data - match to downloaded lst data using either USAF or STNID

station_list <- subset(lst_pts_out, select=c(STNID))
station_list <- station_list[!duplicated(station_list), ]
station_list <- data.frame(station_list)

# MODIS LST download - use lat and lon to extract relevant lst values
lst_pts_out$pixelid <- 1:dim(lst_pts_out)[1]
setwd(out_path)
save.dta13(lst_pts_out, 'lst_station_points.dta')

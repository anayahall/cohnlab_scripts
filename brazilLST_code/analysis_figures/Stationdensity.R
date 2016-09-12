# Station density - Brazil

rm(list = ls())
library(dplyr)
library(data.table)
library(raster)
library(maptools)
library(stringr)
library(XML)

# If necessary change this path and set path to the folder below
main_path <- "E:/brazil_lst/data"
code_path <- "E:/brazil_lst/data"

## GHCN station inventory data
setwd(main_path)
# if the inventory file is not in the dta folder download it
if (file.exists('ghcnd-inventory.txt') ==0){
  download.file("http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", 
                paste(main_path,'/ghcnd-inventory.txt', sep=""))
}

ghcn <- data.frame(read.table("ghcnd-inventory.txt"))
names(ghcn) <- c("StID","Lat","Lon","Dat","Start","End")

# Filter TmaxOnly,
ghcn <- subset(ghcn, End >= 2002)
ghcnTmax <- ghcn[grep("^TMAX", ghcn$Dat), ]

ghcnTmaxshp <- ghcnTmax 
ghcnTmaxshp$x <- ghcnTmaxshp$Lon
ghcnTmaxshp$y <- ghcnTmaxshp$Lat
coordinates(ghcnTmaxshp)=~x+y
proj4string(ghcnTmaxshp)=CRS("+proj=longlat +datum=WGS84")


# Make a grid ~ 1000km buffer around Brazil and find stations within it
setwd(main_path)
br <- getData('GADM', country='Brazil', level=0)

br_ext <- extent(br)
br_ras <- raster(br)

source(paste(code_path,"/makeNewGridWithUID.R", sep="")) # with a bounding box and grid size this function will make unique values

ugrid <- makeNewGridWithUID(br_ext, 0.03,15) # 0.03 = grid size we want; 15 deg = threshold / go 15 deg beyond extent of raster to catch stations outside Brazil
plot(ugrid)
plot(br,add=TRUE)
plot(ghcnTmaxshp,add=TRUE)

ghcnTmaxshp <- raster::intersect(ghcnTmaxshp,ugrid) # gets stations that intersect with bounding area
plot(ugrid)
plot(br,add=TRUE)
plot(ghcnTmaxshp,add=TRUE)

ghcnTmax <- data.frame(ghcnTmaxshp) # add intersecton to dataframe
ghcnTmax <- ghcnTmax[,-which(names(ghcnTmax) %in% c("optional"))] #drop extra variables that dataframe creates

# compute # of operational years within 2002-2013
ghcnTmax$Start[ghcnTmax$Start < 2002] <- 2002# set max start year to 2002
ghcnTmax$End[ghcnTmax$End > 2013] <- 2013# set max start year to 2002
ghcnTmax$yop <- ghcnTmax$End - ghcnTmax$Start+1
ghcn <- ghcnTmax
rm(ghcnTmax)

# convert all station into numeric - done to match GHCN and GSOD stations - this is because GHCN has its own prefix
library(stringr)
ghcn$StID <- gsub("[^[:ascii:]]|[[:punct:]]|[[:space:]]", "", as.character(ghcn$StID), perl=TRUE)
ghcn$StID

## split string at non-digits
s <- strsplit(as.character(ghcn$StID), "[^[:digit:]]")

## convert strings to numeric ("" become NA)
s1 <- as.numeric(unlist(s))

## remove NA and duplicates
s1 <- unique(s1[!is.na(s1)])
s1 <- s1[s1 >0]
ghcn$StID <- s1

ghcn$ghcn <- 1
ghcn$gsod <-0


# GSOD
library(weatheR)
gsod <- allStations() # Assigns our subsetted list of ISD stations to variable 'stations'
head(gsod)

# convert BEGIN END into year
gsod$BEGIN <- as.numeric(substr(as.character(gsod$BEGIN), 1,4))
gsod$END <- as.numeric(substr(as.character(gsod$END), 1,4))

gsod <- subset(gsod,LON < 180 & LAT < 90)
gsod <- subset(gsod,LON > -180 & LAT > -90)
gsod <- subset(gsod, END >= 2002)

gsodshp <- gsod 
gsodshp$x <- gsodshp$LON
gsodshp$y <- gsodshp$LAT
coordinates(gsodshp)=~x+y
proj4string(gsodshp)=CRS("+proj=longlat +datum=WGS84")


# Make a grid ~ 1000km buffer around Brazil and find stations within it
setwd(main_path)
br <- getData('GADM', country='Brazil', level=0)

br_ext <- extent(br)
br_ras <- raster(br)


source(paste(code_path,"/makeNewGridWithUID.R", sep=""))

ugrid <- makeNewGridWithUID(br_ext, 0.03,15)
plot(ugrid)
plot(br,add=TRUE)
plot(gsodshp,add=TRUE)

gsodshp <- raster::intersect(gsodshp,ugrid)
plot(ugrid)
plot(br,add=TRUE)
plot(gsodshp,add=TRUE)

gsod <- data.frame(gsodshp)
gsod <- gsod[,-which(names(gsod) %in% c("optional"))]

# compute # of operational years within 2002-2013
gsod$BEGIN[gsod$BEGIN < 2002] <- 2002# set max start year to 2002
gsod$END[gsod$END > 2013] <- 2013# set max start year to 2002
gsod$yop <- gsod$END- gsod$BEGIN+1

gsod$ghcn <- 0
gsod$gsod <-1


## Now Combine both station
ghcn1 <- data.frame((cbind(ghcn$StID,ghcn$Lat,ghcn$Lon,ghcn$Start,ghcn$End,ghcn$yop,ghcn$ghcn,ghcn$gsod)))
names(ghcn1) <- c("stid","lat","lon","startyear","endyear","yop","ghcn","gsod")

gsod1 <- data.frame((cbind(gsod$USAF,gsod$LAT,gsod$LON,gsod$BEGIN,gsod$END,gsod$yop,gsod$ghcn,gsod$gsod)))
names(gsod1) <- c("stid","lat","lon","startyear","endyear","yop","ghcn","gsod")
allst <- rbind(ghcn1,gsod1)

# find duplicate station and duplicate lat and lon
a<- duplicated(allst$stid)


allst <- allst[!duplicated(allst$stid),]
allst <- allst[!duplicated(allst$stid),]


# Catching cases when GHCN / GSOD starts before / finishes later than the other at duplicate stations
allstsmean <- aggregate (allst,by = list(allst$lat,allst$lon), FUN="mean")
allstsmax <- aggregate (allst,by = list(allst$lat,allst$lon), FUN="max")
allstsmin<- aggregate (allst,by = list(allst$lat,allst$lon), FUN="min")

allstsmean$startyear <- allstsmin$startyear
allstsmean$endyear <- allstsmax$endyear
allstsmean$yop <- allstsmean$endyear-allstsmean$startyear+1

allst <- allstsmean
rm(allstsmax,allstsmean,allstsmin)

allst <- allst[,- which(names(allst) %in% c("Group.1","Group.2"))]
allst$startyear <- round(allst$startyear,digits=0)
allst$endyear <- round(allst$endyear,0)
allst$yop <- round(allst$yop,0)

## Save the CSV of stations and Shapefile
allstshp <- allst
allstshp$lonWGS <- allstshp$lon
allstshp$latWGS <- allstshp$lat
coordinates(allstshp)=~lon+lat
proj4string(allstshp)=CRS("+proj=longlat +datum=WGS84")


## shapefile of stations within Brazil

##########################################################################################
##########################################################################################
# STation Density
m <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
r_matMW <- projectRaster(ugrid, crs=m, method='ngb')

# Write THis Raster
# writeRaster(r_matMW, "sts_MW_matWGSVal.tiff", overwrite=T)
# 
plot(r_matMW)

allstMW<- spTransform(allstshp, CRS("+proj=moll +lon_0=0 +x_0=0 
                                    +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # reproject station location to Mollweide
plot(allstMW, add= TRUE)

stMW <- data.frame(allstMW)
stMW <- stMW[,-which(names(stMW) %in% c("optional"))]

stMW$col <- colFromX(r_matMW, stMW$lon)
stMW$row <- rowFromY(r_matMW, stMW$lat)
stMW$count <- 1

nrows=nrow(r_matMW)
ncols = ncol(r_matMW)

setwd(main_path)
source (paste(code_path,"/getFocalVal.R", sep=""))  # neighborhood functon
# random sample
setwd(main_path)

# Brazil shp
br <- getData('GADM', country='Brazil', level=0)
br_ras <- raster(br)
res(br_ras) <- 0.25
values(br_ras) <- 1:ncell(br_ras)
br_ras <- crop(br_ras, br)

pts <- rasterToPoints(br_ras)
pts <- as.data.frame(pts)

names(pts) <- c("x","y","id")

datshp <- pts
datshp$lon <- datshp$x
datshp$lat <- datshp$y
coordinates(datshp) <-~ x+y
proj4string(datshp) <- CRS("+init=epsg:4326") # set it to lat-long

# project random sample shp file into Mollewide projection
m <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
datpshpPr<- spTransform(datshp, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

plot(r_matMW)
plot(datpshpPr,add=TRUE)

# make a data frame
rsamMW <- data.frame(datpshpPr)
rsamMW <- rsamMW[,-which(names(rsamMW) %in% c("optional","optional.1"))]

# get row and col for the random samples
rsamMW$col <- colFromX(r_matMW, rsamMW$x)
rsamMW$row <- rowFromY(r_matMW, rsamMW$y)

# just making sure it works- map points based on row and col ID
rcount <- dim(rsamMW)[1]
nrows <- nrow(r_matMW)
ncols <- ncol(r_matMW)

neilist <- c(1000)
lennei <- length(neilist)


r_mat <- as.matrix(r_matMW)

pdenall <- data.frame()

nrows=nrow(r_matMW)
ncols = ncol(r_matMW)

for (year in 2013) {
  t1 <- Sys.time()
  print(year)
  # create a randominfo panel
  rsamMWYear <- rsamMW[,c("id","lon","lat","x","y","col","row")]
  rsamMWYear$year <- year
  
  ## Crate a new cmat(count matrix) based on year 
  stMWyear <- subset(stMW, endyear >= year) # active stations in that year
  stMWyear$col <- colFromX(r_matMW, stMWyear$lon)
  stMWyear$row <- rowFromY(r_matMW, stMWyear$lat)
  stMWyear$count <- 1
  
  stcount <- dim(stMWyear)[1] # number of stations
  
  # count the number of unique row and col. count represents the number of stations within a grid.
  stagg <- aggregate(stMWyear,by=list(stMWyear$row,stMWyear$col), FUN="length")
  stavg <- aggregate(stMWyear,by=list(stMWyear$row,stMWyear$col), FUN="mean")
  stavg$count <- stagg$count
  stMWyear <- stavg
  rm(stagg,stavg)
  
  # create an empty matrix and fill with number of stations based on location of stations within the matrix
  cmat <- matrix(0,nrow=nrows,ncol=ncols)
  for (rani in 1: stcount){
    rani
    rowid = stMWyear$row[rani]
    colid= stMWyear$col[rani]
    cmat[c(rowid),c(colid)] <- stMWyear$count[rani]
    #cmat[seq(rowid-1,rowid+1,1),seq(colid-1,colid+1,1)] <- 1
  }
  sum(cmat)
  i <- 2
  for (i in 1:lennei){
    
    nei <- neilist[i]
    
    # i = desired neighborhood in km
    print(nei)
    t <- Sys.time()
    Distm <- nei * 1000; # distance in km
    
    gf <- focalWeight(r_matMW, Distm, "circle")
    gf [gf >0] <-1 # to get sum

    dat1 <- matrix(0,nrow=rcount,ncol=1)
    for (rani in 1: rcount ){

      denval <- getFocalVal (rani,rsamMWYear,gf,cmat)
      dat1[rani,1] <- denval
      
    } 
    
    rsamMWYear$newcol <- dat1
    names(rsamMWYear)[names(rsamMWYear) == "newcol"] <- paste("pden",nei,"km",sep="")
    print(Sys.time()-t)
  }
  
  pdenall <- rbind(pdenall,rsamMWYear)
  
  # save the yearly CSV
  print(Sys.time()-t1)
}

pdenall$x <- pdenall$lon
pdenall$y <- pdenall$lat

pdenall <- pdenall[,-which(names(pdenall) %in% c("lon", "lat"))]

# Write Final station density Panel Data
# Write out station shp
setwd(main_path)
allstats_SA <- as.data.frame(allstshp) 
write.csv(allstats_SA,"stationpoints.csv", row.names=FALSE)

setwd(main_path)
write.csv(pdenall,"stationDensity025.csv", row.names=FALSE)




























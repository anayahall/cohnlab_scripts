# Merge start and end years for runs of GFC

# SOurce of Costal line and water bodies/River data
# https://www.soest.hawaii.edu/pwessel/gshhg/
# https://www.soest.hawaii.edu/pwessel/gshhg/README.TXT

rm(list=ls())

library(maptools)
library(raster)
library(readstata13)
library(rgdal)

#set this option so R does not print out in scientific notation
options(scipen = 999)


# Paths in Cluster
code_path <- '/cluster/shared/jangus01/brazil_lst/code_path'
out_path <- '/cluster/tufts/duncan/'
data_path <- '/cluster/tufts/bhattarai02/nfbras'

## Seprate End years and derive end year CSVs
setwd(code_path)
dat <- read.dta13(paste(code_path,"/gfc_universe_pfb1.dta",sep=""))
minend <- min(unique(dat$end))
maxend <- max(unique(dat$end))

halo <- c(2000, 4000, 10000)
fnames <- c("fa1_") #neighborhood forest cover
pname <- c("pfa1_") #pixel forest cover

allend <- data.frame()

for (year in minend:maxend){
  print(year)
  subdat <- subset(dat,end==year)
  
  datcopy <- subdat
  coordinates(subdat)=~x+y
  proj4string(subdat)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  
  
  for (fname in fnames){
    print(fname)
    dat1 <- datcopy
    for (h in halo){
      print(h)
      t <- Sys.time()
      
      setwd(data_path)
      ras <- raster(paste(fname,year,"_",toString(h),".tif", sep=""))
      pts <- extract(ras, subdat, method ='simple')
      pts <- round(pts, digits=2)
      dat1$newcol <- pts
      names(dat1)[names(dat1) == 'newcol'] <- paste("n",fname,h,"end", sep="")
      print(Sys.time()-t) 
      
    }
    
  }
  
    setwd(data_path)
    ras <- raster(paste(pname,year,".tif", sep=""))
    pts <- extract(ras, subdat, method ='simple')
    pts <- round(pts, digits=2)
    dat1$newcol <- pts
    names(dat1)[names(dat1) == 'newcol'] <- paste("nfa1_1end", sep="") # this end pixel forest cover value
    print(Sys.time()-t)
    
  print("Merging end years")
  allend <- rbind(allend,dat1)  
}


#START = 2002 for all GFC
styear <- 2002
setwd(code_path)
allstart <- dat
coordinates(dat)=~x+y
proj4string(dat)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  

for (fname in fnames){
  print(fname)
  
  for (h in halo){
    print(h)
    t <- Sys.time()
    
    setwd(data_path)
    ras <- raster(paste(fname,styear,'_',h,'.tif', sep=""))
    pts <- extract(ras, dat, method ='simple')
    pts <- round(pts, digits=2)
    #print(Sys.time()-t)
    allstart$newcol <- pts
    names(allstart)[names(allstart) == 'newcol'] <- paste("n",fname,h,"st", sep="")
    print(Sys.time()-t) 
    
  }
}  

# 2002 start pixel location

  setwd(data_path)
  ras <- raster(paste(pname,styear,".tif", sep=""))
  pts <- extract(ras, dat, method ='simple')
  pts <- round(pts, digits=2)
  allstart$newcol <- pts
  names(allstart)[names(allstart) == 'newcol'] <- paste("nfa1_1st", sep="") # this start pixel forest cover value
  print(Sys.time()-t)
    
str(allstart)
str(allend)

allstart  <-allstart[order(allstart$u_id),]
allend  <- allend[order(allend$u_id),]

diff_uid <- sum(abs(allstart$u_id-allend$u_id)) # THIS MUST BE 0

if (diff_uid ==0) {
  print ('u_id peferectly ordered')
  
}

if (diff_uid !=0) {
  print ('u_id peferectly ordered')
  
}

print(" Merging Start and End years...............")
alls_end <- cbind(allstart, allend[,7:dim(allend)[2]])

alls_shp <- alls_end[,1:4]
alls_shp$lon <- alls_shp$x
alls_shp$lat <- alls_shp$y

coordinates(alls_shp)=~lon+lat
proj4string(alls_shp)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  


alls_shpMW <- spTransform(alls_shp, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 
                                        +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# Water Layer
# Distance from Major Rivers
rshp <-  readShapeLines(paste(code_path,"/WDBII_river_h_L04.shp", sep=""))

bb <- c(-84.06186, -28.3912, -60.3116, 16.51572) # Entire South America (to get the full coast line in all directions)
crs(rshp) <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # confirmed
rshp <- crop(rshp,bb)

m <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# wshp MW projection
rshpMW<- spTransform(rshp, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# Get 1 km buffer from coastline
library(rgeos)
r1km <- gBuffer(rshpMW, width=1000, byid=TRUE )

# create 1 km grid of r1km
lcMW <- raster(paste(code_path,"/modis1kmlcMW_2002.tif", sep=""))
r1kmras <- rasterize(r1km,lcMW)

pts <- extract(r1kmras,alls_shpMW, method='simple')
alls_end$n1km_river <- pts
alls_end$n1km_river[!is.na(alls_end$n1km_river)] <- 1
alls_end$n1km_river[is.na(alls_end$n1km_river)] <- 0

#drop all points within 1 km of rivers
print('dropping points near rivers')
alls_end <- subset(alls_end, n1km_river != 1)

removeTmpFiles(h=1)

print("writing dta....")
setwd(out_path)
save.dta13(alls_end, paste("gfc_universe_nfa1.dta", sep=""))
print("write out done")

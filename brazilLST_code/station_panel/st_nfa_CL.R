# extract NFA for station data 

rm(list=ls())

library(maptools)
library(raster)
library(readstata13)

#set this option so R does not print out in scientific notation
options(scipen = 999)

# Paths in Cluster
code_path <- '/cluster/shared/jangus01/brazil_lst/code_path'
out_path <- '/cluster/tufts/duncan/' 
best_path <- '/cluster/tufts/bhattarai02/best'
ppt_path <- '/cluster/tufts/bhattarai02/persiann/cdr/persiann'
nfc_path <- '/cluster/tufts/bhattarai02/nfbras' 

setwd(code_path)
stpts <- read.dta13(paste("lst_station_points.dta",sep=""))
stpts <- stpts[, c("modislon", "modislat", "pixelid")]
infocol <- dim(stpts)[2] 
stpts1 <- stpts 

#make stpts spatial points
coordinates(stpts)=~modislon+modislat
proj4string(stpts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

years <- c(2002:2011)

fcpanel <- data.frame()

for (iyear in years){
  print(paste("Processing year ", iyear, "..............", sep=""))
  dat1 <- stpts1
  dat1$year <- iyear
  dat1 <- dat1[,-which(names(dat1) %in% c("modislon", "modislat"))] 
  
  halo <- c(2000, 4000, 10000, 25000, 50000, 100000)
  fnames <- c("fa1") 
  pfnames <- c("pfa1")

  t <- Sys.time()
  
  for (fname in fnames){

    for (h in halo){
      
      setwd(nfc_path)
      ras <- raster(paste(fname,"_",iyear,'_',h,'.tif', sep=""))
      pts <- extract(ras, stpts, method ='simple')
      pts <- round(pts, digits=2)
      dat1$newcol <- pts
      names(dat1)[names(dat1) == 'newcol'] <- paste("n",fname,"_",h/1000,"km", sep="") # appending n, n = neighborhood
      
    }

  }
  
  # Now extract pixel stats
  
  for (fname in pfnames){
    
    setwd(nfc_path)
    
    ras <- raster(paste(fname,"_",iyear,".tif", sep=""))
    pts <- extract(ras, stpts, method ='simple')
    pts <- round(pts, digits=2)
    dat1$newcol <- pts
    names(dat1)[names(dat1) == 'newcol'] <- "nfa1_1" 
    
  }
  
  print(Sys.time()-t) 
  print("Appening Yearly panels ...........................")
  print(iyear)
  fcpanel <- rbind(fcpanel,dat1)
  
}

fcpanel$year <- fcpanel$year + 1

removeTmpFiles(h=1)
print(paste("writing fc panel  ", iyear, sep=""))
setwd(out_path)
save.dta13(fcpanel, paste("panel_fc_st.dta", sep=""))
print('writing nfa')



#Extract NFA for random sample near ag

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
lst_path  <- '/cluster/tufts/bhattarai02/BrazilLST/LST_Aqua/'

setwd(code_path)

stpts <- read.dta13(paste("pfb1_25deg_sample_with_ag.dta",sep=""))
stpts <- stpts[, c("u_id25", "x", "y")]

infocol <- dim(stpts)[2] # after this column LST data is added
stpts1 <- stpts # make a copy of random sample for creating yearly panels

#make stpts spatial points
coordinates(stpts)=~x+y
proj4string(stpts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Change This
years <- c(2002, 2011)

fcpanel <- data.frame()
dat1 <- stpts1
dat1 <- as.data.frame(dat1)

for (iyear in years){
  
  print(paste("Processing year ", iyear, "..............", sep=""))
  
  
  halo <- c(2000, 4000, 10000, 100000)
  fnames <- c("fa1") 
  pfnames <- c("pfa1")
  
  t <- Sys.time()
  for (fname in fnames){
    
    for (h in halo){
      
      setwd(nfc_path)
      ras <- raster(paste(fname,"_",iyear,'_',h,'.tif', sep=""))
      pts <- extract(ras, stpts, method ='simple')
      pts <- round(pts, digits=2)
      #print(Sys.time()-t)
      dat1$newcol <- pts
      names(dat1)[names(dat1) == 'newcol'] <- paste("n",fname,"_",h/1000,"km_",iyear, sep="") # appending n, n = neighborhood
      
    }
    
  }
  
  # Now extract pixel stats
  for (fname in pfnames){
    
    setwd(nfc_path)
    ras <- raster(paste(fname,"_",iyear,".tif", sep=""))
    pts <- extract(ras, stpts, method ='simple')
    pts <- round(pts, digits=2)
    dat1$newcol <- pts
    names(dat1)[names(dat1) == 'newcol'] <- paste(fname,'_',iyear, sep='') 
    
  }
  
  print(Sys.time()-t) 
  
}

print('writing......')

setwd(out_path)
save.dta13(dat1, paste("panel_25km_nfa.dta", sep=''))



#Extract LST values for random sample near ag
rm(list = ls())

library(raster)
library(maptools)
library(reshape)
library(readstata13)

removeTmpFiles(h=1)

# change this for other years
year_run <- c(2012)

# Paths in Cluster
code_path <- '/cluster/shared/jangus01/brazil_lst/code_path'
out_path <- '/cluster/tufts/duncan/' 
best_path <- '/cluster/tufts/bhattarai02/best'
ppt_path <- '/cluster/tufts/bhattarai02/persiann/cdr/persiann'
nfc_path <- '/cluster/tufts/bhattarai02/nfbras' 
lst_path  <- '/cluster/tufts/bhattarai02/BrazilLST/LST_Aqua/'

#function that create year, month, doy, day, week, gsyear panel
source(paste(code_path,"/Get_ymdoys.R",sep=""))
alldoys <- Get_ymdoys(2000,2014,gs_startMonth=8,gs_endMonth=7)
alldoys <- subset(alldoys, gsyear >= 2003) #DOY for growing season

setwd(code_path)
#read in random sample ID
stpts <- read.dta13(paste("pfb1_25deg_sample_with_ag.dta",sep=""))

# start Extracting LST values

for (igsyear in year_run) {
  
  setwd(lst_path )
  
  print(igsyear)
  
  #subset to points
  gs_id <- stpts[, c("u_id25", "x", "y")]
  
  gs_id_qc <- gs_id
  
  st_points <- gs_id
  # create spatial points data frame for LST extraction
  st_points$x <- gs_id$x
  st_points$y <- gs_id$y
  st_points$lat <- gs_id$x
  st_points$lon <- gs_id$y
  coordinates(st_points)=~x+y
  proj4string(st_points)=CRS("+init=epsg:4326")
  
  print("spatial_done")
  
  
  ad <- subset(alldoys, year == igsyear)
  yeardoy <- ad$yeardoy
  
  for (i in yeardoy) {
    print(i)
    
    t <- Sys.time()
    ## MYD daytime LST and QC
    lstDayfname <- paste(lst_path,"/MYD11A1.A", i, ".LST_Day_1km.tif", sep="")
    qcDayfname <- paste(lst_path, "/MYD11A1.A", i, ".QC_Day.tif", sep="")
    
    # check if the file exists in the folder or not
    if (file.exists(lstDayfname)){
      lst <- raster(lstDayfname) 
      gs_id$newcol <- extract(lst, st_points, method='simple', na.rm=FALSE) 
      gs_id$newcol <- gs_id$newcol* 0.02 - 273.15
      names(gs_id)[names(gs_id) == 'newcol'] <- paste(i) 
      
    }
    
    if (file.exists(qcDayfname)){
      qc <- raster(qcDayfname) 
      gs_id_qc$newcol <- extract(qc, st_points, method='simple', na.rm=FALSE) 
      names(gs_id_qc)[names(gs_id_qc) == 'newcol'] <- paste(i) 
      
      removeTmpFiles(h=1)
      
    }
    
    print(Sys.time()-t)
    
  }
  
  #reshape wide to long  
  gs_id <- melt(gs_id, id=c("u_id25", "x", "y"))
  names(gs_id)[names(gs_id) == "variable"] <- 'yeardoy'
  names(gs_id)[names(gs_id) == "value"] <- 'lst'
  
  
  print("lst_done")
  
  #MODIS QC info
  #reshape wide to long 
  gs_id_qc <- melt(gs_id_qc, id=c("u_id25", "x", "y"))
  names(gs_id_qc)[names(gs_id_qc) == "variable"] <- 'yeardoy'
  names(gs_id_qc)[names(gs_id_qc) == "value"] <- 'qc'
  
  gs_id_qc$qcbin1 <- 0  # Highest Quality
  gs_id_qc$qcbin1[gs_id_qc$qc == 0] <- 1
  
  gs_id_qc$qcbin2 <- 0  # less than 1 K error
  gs_id_qc[which(gs_id_qc$qc %in% c(1,5,17,21)), c("qcbin2")] <- 1
  
  gs_id_qc$qcbin3 <- 0  #  1- 3 K error
  gs_id_qc[which(gs_id_qc$qc %in% c(65,69,81,85,129,133,145,149)), c("qcbin3")] <- 1
  
  print("qc_done")
  
  gs_id <- merge(gs_id, gs_id_qc, by=c("u_id25", "yeardoy"))
  
  print("merge good")
  
  gs_id <- gs_id[, c("u_id25", "yeardoy", "lst", "qc","qcbin1", "qcbin2", "qcbin3")]
  
  setwd(out_path)
  save.dta13(gs_id, paste("panel_25km_lst.dta",sep=""))
  
}

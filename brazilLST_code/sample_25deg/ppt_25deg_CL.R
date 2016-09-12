# Extract PERSIAAN precipitation data for random sample near ag
rm(list=ls())

library(ncdf4)
library(raster)
library(maptools)
library(reshape)
library(readstata13)

year_run <- c(2012)

# Paths in Cluster
code_path <- '/cluster/shared/jangus01/brazil_lst/code_path'
out_path <- '/cluster/tufts/duncan/' 
best_path <- '/cluster/tufts/bhattarai02/best'
ppt_path <- '/cluster/tufts/bhattarai02/persiann/cdr/persiann'
nfc_path <- '/cluster/tufts/bhattarai02/nfbras' 
lst_path  <- '/cluster/tufts/bhattarai02/BrazilLST/LST_Aqua/'


#function that create year, month, doy, day, week, gsyear panel
setwd(code_path)
source(paste("Get_ymdoys.R",sep=""))
alldoys <- Get_ymdoys(2000,2014,gs_startMonth=8,gs_endMonth=7)
alldoys <- subset(alldoys, gsyear >= 2002 & gsyear <= 2013)
ad <- alldoys[,c("year","doy","yeardoy","gsyear", "month")]


#read in random sample
stpts <- read.dta13(paste("pfb1_25deg_sample_with_ag.dta",sep=""))
stpts <- stpts[, c("x", "y", "u_id25")]

ranDF <- stpts # make a copy of random sample for creating yearly panels
coordinates(stpts)=~x+y
proj4string(stpts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

setwd(ppt_path)
pptpanel <- data.frame()

for (iyear in year_run) {
  print(iyear)
  t1 <- Sys.time()
  ydoys <- subset(alldoys, year == iyear)
  ranyear <- data.frame()
  doyl <- dim(ydoys)[1]
  doynum <- 1
  pptyear <- ranDF
  
  for (doynum in 1:doyl) {
    #ft <- Sys.time()
    
    print(doynum)
    
    year <- ydoys$year[doynum]
    month <- ydoys$month[doynum]
    day <- ydoys$day[doynum]
    yeardoy <- ydoys$yeardoy[doynum]
    subdir <- paste(ppt_path,"/files/",year,sep="")
    
    #setwd(subdir) - PERSIANN data downloads into subdirectories
    if (file.exists(subdir) ==1){setwd(subdir)}
    if (!file.exists(subdir) ==1){print(paste("Folder", year, "does not exist: Please check data folder", sep=""))}
    
    # YearMonthDay String
    mstr <- toString(month)
    if (month <10) {mstr <- paste("0",month,sep="")}
    daystr <- toString(day)
    if (day <10) {daystr <- paste("0",day,sep="")}
    ymd_str = paste(year,mstr,daystr,sep="")
    
    # list the precipitation file- there should be only one file
    ncfilename <- list.files(pattern=paste("^.*",ymd_str,".*.nc$", sep=""))
    if (!file.exists(ncfilename)){print(paste("Nc file ", ymd_str, "does not exist: Please check data folder",
                                              sep=""))}
    
    if (file.exists(ncfilename)){
      ncin <- nc_open(ncfilename)
      # Read ppt and matrix and then make adjustment to align properly with world coordinates 
      
      p <- ncvar_get(ncin, "precipitation") 
      p <- cbind(p[,721:dim(p)[2]],p[,1:720]) # this is always work for PERSIANN
      p <- raster(p)
      extent(p) <- c(-180,180,-60,60)
      
      crs(p) <- CRS("+init=epsg:4326")
      
      pptyear$ppt <- extract(p,stpts,'simple')
      pptyear$ppt <- round(pptyear$ppt,2)
      names(pptyear)[names(pptyear) == 'ppt'] <- paste(yeardoy) 
      
      #close netcdf 4 
      nc_close(ncin)
    }
    
  }
  
  # reshape- long format
  pptyear <- melt(pptyear, id=c("u_id25", "x", "y"))
  names(pptyear)[names(pptyear) == "variable"] <- 'yeardoy'
  names(pptyear)[names(pptyear) == "value"] <- 'ppt'
  
  pptyear <- pptyear[,c("u_id25","yeardoy","ppt")]
  pptyear$ppt [pptyear$ppt == -9999] <- NA # set missing values to NA
  
  print(Sys.time() -t1)
}

print("Writing PPT Panel ..................")

setwd(out_path)
save.dta13(pptyear, paste("panel_25km_ppt.dta", sep=""))




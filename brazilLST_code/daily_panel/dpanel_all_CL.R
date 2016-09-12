## Merge all data to create a daily panel data
rm(list =ls())

library(stringr)
library(raster)
library(maptools)
library(reshape)
library(ncdf4)
library(readstata13)

removeTmpFiles(h=1)


# Paths in Cluster
code_path <- '/cluster/shared/jangus01/brazil_lst/code_path/'
out_path <- '/cluster/tufts/duncan/' 
best_path <- '/cluster/tufts/bhattarai02/best'
ppt_path <- '/cluster/tufts/bhattarai02/persiann/cdr/persiann'
nfc_path <- '/cluster/tufts/bhattarai02/nfbras' 

setwd(code_path)

# Large bounding box for Brazil
br <- c(-84.06186, -28.3912, -60.3116, 16.51572) 

# Get dates
source(paste("Get_ymdoys.R",sep=""))
alldoys <- Get_ymdoys(2001,2013,gs_startMonth=1,gs_endMonth=12)
alldoys <- subset(alldoys, select=c(gsyear,year,month,doy,yeardoy,week_id)) 
alldoys <- subset(alldoys, year >= 2002 & year <= 2012) 
ad <- alldoys[,c("gsyear","year","doy","yeardoy", "month","week_id")]
names(ad)[names(ad) == "month"] <- "monthid"
names(ad)[names(ad) == "week_id"] <- "weekid"

# random sample
# load sample data
ransample <- read.dta13(paste("mhv_4_nw_gfc.dta",sep=""))
u_idsid <- read.dta13(paste("gfc_universe_s_id.dta",sep=""))
u_idsid <- u_idsid[,c("u_id", "s_id")]
ransample <- merge(ransample, u_idsid, by=c("u_id"))
rm(u_idsid)

#LST data
# daytime LST in degree C
setwd(out_path)
lstpanel <- read.dta13(paste("panel_lst_mhv_4_nw.dta", sep="")) 

#BEST
print(' Creating BEST Panel ...............')
setwd(best_path)
t <- Sys.time()
l <- brick("Complete_TMAX_Daily_LatLong1_2000.nc", varname='temperature')
c <- brick("Complete_TMAX_Daily_LatLong1_2000.nc", varname='climatology')
ll <- brick("Complete_TMAX_Daily_LatLong1_2010.nc", varname='temperature')
cc <- brick("Complete_TMAX_Daily_LatLong1_2010.nc", varname='climatology')

#crop global raster to Brazil extent
l <- crop(l, br)
c <- crop(c, br)
ll <- crop(ll, br)
cc <- crop(cc, br)

lll <-stack(l, ll)

#leap year climatology doy 365 = 366
decl <- c[[365]]
cl <- stack(c, decl)

#create 2013 climatology = only months January to November
c2013 <- cc[[1:334]]

#stack of climatologies
c_clim <- stack(cl, c, c, c, cl, c, c, c, cl, c, cc, cc, cl, c2013)

#add climatology to anomaly
lll <- overlay(lll, c_clim, fun=sum)

#Get growing season day IDs
setwd(code_path)
source(paste("Get_ymdoys.R", sep=""))
alldoys <- Get_ymdoys(2000,2014,gs_startMonth=1,gs_endMonth=12)
alldoys <- subset(alldoys, year >= 2000 & year < 2014)
alldoys$bestind <- seq(1, nrow(alldoys), 1)
ad <- alldoys[,c("year","doy","yeardoy","gsyear", "bestind")]

#read in random sample
stpts <- read.dta13(paste("mhv_4_nw_gfc.dta",sep=""))
stpts <- stpts[, c("x", "y", "u_id")]
infocol <- dim(stpts)[2] 
stpts1 <- stpts # make a copy of random sample for creating yearly panels

#make stpts spatial points
coordinates(stpts)=~x+y
proj4string(stpts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

bestpanel <- data.frame()

# for each growing season year extract BEST values
for (i in 2003:2012) {
  
  print(i)
  
  z <- subset(alldoys, year == i)
  zmin <- min(z$bestind)
  zmax <- max(z$bestind)
  
  best <- lll[[zmin:zmax]]
  
  b_out <- extract(best, stpts, method='simple')
  
  r_ind <- stpts1
  
  r_out <- cbind(r_ind, b_out)
  
  for (zz in 1:nrow(z)) {
    z_id <- zz+infocol
    names(r_out)[z_id] <- z$yeardoy[zz]
  }
  
  r_out[,c((infocol+1):dim(r_out)[2])] <- round(r_out[,c((infocol+1):dim(r_out)[2])] ,digits=2)
  
  
  # reshape- long format
  r_out <- melt(r_out, id=c("u_id", "x", "y"))
  names(r_out)[names(r_out) == "variable"] <- "yeardoy"
  names(r_out)[names(r_out) == "value"] <- "tmaxb"
  
  #print("Appening Yearly panels ...........................")
  bestpanel <- rbind(bestpanel,r_out)
  
}

print(' Best Panel dome ...............')
print(Sys.time()-t)

# Merge BEST and LST Panel
print('Merging BEST and LST Panel')
lstbest <- merge(lstpanel,bestpanel, by =c("u_id","yeardoy"))
rm(lstpanel,bestpanel)

str(lstbest)

#PPT data

print('Creating precipitation Panel............')

#function that create year, month, doy, day, week, gsyear panel
setwd(code_path)
source(paste("Get_ymdoys.R",sep=""))
alldoys <- Get_ymdoys(2000,2014,gs_startMonth=1,gs_endMonth=12)
alldoys <- subset(alldoys, year >= 2003 & year <= 2012)
ad <- alldoys[,c("year","doy","yeardoy","gsyear")]

#read in random sample
stpts <- read.dta13(paste("mhv_4_nw_gfc.dta",sep=""))
stpts <- stpts[, c("x", "y", "u_id")]

ranDF <- stpts # make a copy of random sample for creating yearly panels
coordinates(stpts)=~x+y
proj4string(stpts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

setwd(ppt_path)
pptpanel <- data.frame()

for (iyear in 2003:2012) {
  t1 <- Sys.time()
  ydoys <- subset(alldoys, year == iyear)
  ranyear <- data.frame()
  doyl <- dim(ydoys)[1]
  doynum <- 1
  pptyear <- ranDF
  
  for (doynum in 1:doyl) {
    #     print(doynum)
    year <- ydoys$year[doynum]
    month <- ydoys$month[doynum]
    day <- ydoys$day[doynum]
    yeardoy <- ydoys$yeardoy[doynum]
    subdir <- paste(ppt_path,"/files/",year,sep="")
    
    #setwd(subdir) - PERSIANN data downloads into subdirectories
    if (file.exists(subdir) ==1){setwd(subdir)}
    if (!file.exists(subdir) ==1){print(paste("Folder", year, "does not exist: Please check data folder", paste=""))}
    
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
  pptyear <- melt(pptyear, id=c("u_id", "x", "y"))
  names(pptyear)[names(pptyear) == "variable"] <- 'yeardoy'
  names(pptyear)[names(pptyear) == "value"] <- 'ppt'
  
  
  pptyear <- pptyear[,c("u_id","yeardoy","ppt")]
  pptyear$ppt [pptyear$ppt == -9999] <- NA # set missing values to NA
  pptpanel <- rbind(pptpanel,pptyear)
  
}

print("Merging pptpanel ..................")
dpanel <- merge(lstbest,pptpanel, by =c("u_id","yeardoy"))
names(dpanel)
str(dpanel)
print(Sys.time()-t)
rm(lstbest,pptpanel)

#Merge to add year, month, doy etc
dpanel <- merge(dpanel,ad,by=c("yeardoy"))

#Make nfc and pfa panels
print('Making Yearly nfc and pfc panels..............')

setwd(code_path)
stpts <- read.dta13(paste("mhv_4_nw_gfc.dta",sep=""))
stpts <- stpts[, c("x", "y", "u_id")]
infocol <- dim(stpts)[2] 
stpts1 <- stpts 

#make stpts spatial points
coordinates(stpts)=~x+y
proj4string(stpts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

fcpanel <- data.frame()

for (iyear in 2002:2011){
  print(paste("Processing year ", iyear, "..............", sep=""))
  dat1 <- stpts1
  dat1$year <- iyear
  dat1 <- dat1[,-which(names(dat1) %in% c("x","y"))] 
  
  halo <- c(2000, 4000, 10000)
  fnames <- c("fa1") # n for neighborhood will be added later
  pfnames <- c("pfa1")
  
  t <- Sys.time()
  for (fname in fnames){
    print(fname)
    for (h in halo){
      print(h)
      
      setwd(nfc_path)
      ras <- raster(paste(fname,"_",iyear,'_',h,'.tif', sep=""))
      pts <- extract(ras, stpts, method ='simple')
      pts <- round(pts, digits=2)
      #print(Sys.time()-t)
      dat1$newcol <- pts
      names(dat1)[names(dat1) == 'newcol'] <- paste("n",fname,"_",h/1000,"km", sep="") # appending n, n = neighborhood
      
    }

  }
  
  # Now extract pixel stats
  for (fname in pfnames){
    #print(fname)
    
    setwd(nfc_path)
    ras <- raster(paste(fname,"_",iyear,".tif", sep=""))
    pts <- extract(ras, stpts, method ='simple')
    pts <- round(pts, digits=2)
    #print(Sys.time()-t)
    dat1$newcol <- pts
    names(dat1)[names(dat1) == 'newcol'] <- 'nfa1_1' 
  }
  
  print(Sys.time()-t) 
  print("Appening Yearly panels ...........................")
  print(iyear)
  fcpanel <- rbind(fcpanel,dat1)
  
}

fcpanel$year <- fcpanel$year + 1
str(fcpanel)

# Add yearly and daily panel (daily panel has monthly and weekly_id)
main_panel <- merge(dpanel,fcpanel,by=c("u_id","year"))
str(main_panel)

rm(dpanel,fcpanel)

names(main_panel)

# merge s_id - id for 0.25 deg grid cells
main_panel<- merge(main_panel,ransample, by =c("u_id"))


## Compute extra Terrestial radiation (Allen et al. 1998, FAO guidelines)
lat_rad <- main_panel$y*pi/180
dr = 1+0.033*cos(main_panel$doy*2*pi/365) # Earth-Sun distance
dec = 0.4093 * sin ((2*pi/365)*main_panel$doy - 1.39) # solar Dec
ws = acos(-1*tan(lat_rad) *tan(dec))# Note: Lat in radians
R = 24/pi *  4.92* dr *((ws *sin(lat_rad))*sin(dec)+cos(lat_rad)*(cos(dec)*sin(ws))) 
main_panel$rmjm2 <- round(R,digits = 2)

str(main_panel)
main_panel <- as.data.frame(lapply(main_panel,as.numeric))
setwd(out_path)
save.dta13(main_panel,paste("dailypanelmhv_4_nw_gfc.csv",sep=""))
print('finished writing')

## Merge all station data to create a daily panel data
rm(list =ls())

library(reshape)
library(stringr)
library(raster)
library(readstata13)

#set this option so R does not print out in scientific notation
options(scipen = 999)

removeTmpFiles(h=1)

# Paths in Cluster
code_path <- '/cluster/shared/jangus01/brazil_lst/code_path'
out_path <- '/cluster/tufts/duncan/' 
best_path <- '/cluster/tufts/bhattarai02/best'
ppt_path <- '/cluster/tufts/bhattarai02/persiann/cdr/persiann'
nfc_path <- '/cluster/tufts/bhattarai02/nfbras' 

source(paste(code_path,"/Get_ymdoys.R",sep=""))
alldoys <- Get_ymdoys(2000,2014,gs_startMonth=1,gs_endMonth=12)
alldoys <- subset(alldoys, select=c(gsyear,year,month,doy,day,yeardoy,week_id)) # ignore gsyear
alldoys <- subset(alldoys, year >= 2003 & year <= 2012) 
ad <- alldoys[,c("gsyear","year","doy","day","yeardoy", "month", "week_id")]
names(ad)[names(ad) == "month"] <- "monthid"
names(ad)[names(ad) == "week_id"] <- "weekid"

# random sample
# load sample data
setwd(code_path)
statvar <- read.dta13(paste("lst_station_points_withstaticvar.dta",sep=""))

# daily BEST Tmax in degree C
setwd(out_path)
bestpanel <- read.dta13(paste("panel_bt_st.dta", sep=""))
names(bestpanel)
bestpanel$yeardoy <- as.numeric(as.character(bestpanel$yeardoy))
str(bestpanel)

# daytime LST in degree C
lstpanel <- read.dta13(paste("panel_lst_st.dta", sep="")) 
lstpanel$yeardoy <- as.numeric(as.character(lstpanel$yeardoy))
str(lstpanel)

t <- Sys.time()
lstbest <- merge(bestpanel,lstpanel, by =c("pixelid","yeardoy"), all=TRUE) # to preseve BEST Tmax, Tmin for edd
print(Sys.time()-t)
str(lstbest)

rm(lstpanel,bestpanel)

pptpanel <- read.dta13(paste("panel_ppt_st.dta", sep=""))
pptpanel$yeardoy <- as.numeric(as.character(pptpanel$yeardoy))
str(pptpanel)

t <- Sys.time()
dpanel <- merge(lstbest,pptpanel, by =c("pixelid","yeardoy"))
print(Sys.time()-t)
str(dpanel)

rm(lstbest,pptpanel)

## Merge with ad to add year, month, week_id, doy
dpanel <- merge(dpanel,ad,by=c("yeardoy"))
str(dpanel)

# Merge NFC
ypanel <- read.dta13(paste("panel_fc_st.dta", sep=""))
ypanel$year <- as.numeric(as.character(ypanel$year))
str(ypanel)

# this is already y-1
t <- Sys.time()
main_panel <- merge(dpanel,ypanel,by=c("pixelid","year"))
print(Sys.time()-t)
str(main_panel)

#merge with static variables
main_panel<- merge(main_panel,statvar, by =c("pixelid"))
str(main_panel)

# Compute extra Terrestial radiation (Allen et al. 1998, FAO guidelines)
lat_rad <- main_panel$LAT*pi/180
dr = 1+0.033*cos(main_panel$doy*2*pi/365) # Earth-Sun distance
dec = 0.4093 * sin ((2*pi/365)*main_panel$doy - 1.39) # solar Dec
ws = acos(-1*tan(lat_rad) *tan(dec))# Note: Lat in radians
R = 24/pi *  4.92* dr *((ws *sin(lat_rad))*sin(dec)+cos(lat_rad)*(cos(dec)*sin(ws))) 
main_panel$rmjm2 <- round(R,digits = 2)


main_panel$USAF <- as.numeric(as.character(main_panel$USAF))
str(main_panel)

# Get station Tmax Data
setwd(code_path)
st_tmax <- read.csv(paste("st_tmax.csv", sep=""), header = TRUE)

st_tmax <- as.data.frame(lapply(st_tmax,as.numeric))
str(main_panel)

# merge with station lst panel data
main_panel$STN <- main_panel$USAF
str(main_panel)

# merge station temp with main panel
main_panel<- merge(main_panel,st_tmax,by=c("STN", "yeardoy"))
str(main_panel)
main_panel <- main_panel[,-which(names(main_panel) %in% c("ELEV.M","ELEV.M.SRTM.90m","CALL",
                                                          "STATE","CTRY", "X", "x","y", "optional"))]
str(main_panel)


main_panel <- as.data.frame(lapply(main_panel,as.numeric))
setwd(out_path)
save.dta13(main_panel,paste("stpanel.dta",sep=""))



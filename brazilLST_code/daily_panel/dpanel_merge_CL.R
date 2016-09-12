## Merge to create a daily panel data
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
ad <- alldoys[,c("gsyear","year","doy", "day","yeardoy", "month", "week_id")]
names(ad)[names(ad) == "month"] <- "monthid"
names(ad)[names(ad) == "week_id"] <- "weekid"

#LST data
# daytime LST in degree C
setwd(out_path)
lstpanel <- read.dta13(paste("panel_lst_mhv_4_nw.dta", sep="")) 
lstpanel$yeardoy <- as.numeric(as.character(lstpanel$yeardoy))

#lonlat
lonlat <- read.dta13('mhv_4_nw_gfc.dta')
lstpanel <- merge(lstpanel, lonlat, by=c("u_id"))
str(lstpanel)

#merge year, month etc
lstpanel <- merge(lstpanel, ad, by=c("yeardoy"))
str(lstpanel)

bestpanel <- read.dta13("dpanel_best.dta")
bestpanel$yeardoy <- as.numeric(as.character(bestpanel$yeardoy))

#merge best - lst
bestlst <- merge(lstpanel, bestpanel, by=c("u_id", "yeardoy"))
str(bestlst)

#merge ppt
pptpanel <- read.dta13("dpanel_ppt.dta")
dpanel <- merge(bestlst, pptpanel, by=c("u_id", "yeardoy"))
pptpanel$yeardoy <- as.numeric(as.character(pptpanel$yeardoy))
str(dpanel)

#merge nfa 
fpanel <- read.dta13("dpanel_nfa.dta")
fpanel$yeardoy <- as.numeric(as.character(fpanel$year))
dpanel <- merge(dpanel, fpanel, by=c("u_id", "year"))
str(dpanel)

## Compute extra Terrestial radiation (Allen et al. 1998, FAO guidelines)
lat_rad <- dpanel$y*pi/180
dr = 1+0.033*cos(dpanel$doy*2*pi/365) # Earth-Sun distance
dec = 0.4093 * sin ((2*pi/365)*dpanel$doy - 1.39) # solar Dec
ws = acos(-1*tan(lat_rad) *tan(dec))# Note: Lat in radians
R = 24/pi *  4.92* dr *((ws *sin(lat_rad))*sin(dec)+cos(lat_rad)*(cos(dec)*sin(ws))) 
dpanel$rmjm2 <- round(R,digits = 2)
str(dpanel)

#write panel out 
print("writing out panel")
dpanel <- as.data.frame(lapply(dpanel,as.numeric))
save.dta13(dpanel, "dailypanel_mhv_4_nw.dta")



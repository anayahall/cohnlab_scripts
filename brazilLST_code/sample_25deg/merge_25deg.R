## Merge all data to merge data near ag
rm(list =ls())

library(reshape)
library(dplyr)
library(stringr)
library(raster)
library(readstata13)

#set this option so R does not print out in scientific notation
options(scipen = 999)

removeTmpFiles(h=1)

data_path <- 'E:/brazil_lst/data/'
out_path <- 'E:/brazil_lst/data/'
setwd(data_path)

source(paste("Get_ymdoys.R",sep=""))
alldoys <- Get_ymdoys(2000,2014,gs_startMonth=8,gs_endMonth=7)
alldoys <- subset(alldoys, select=c(gsyear,year,month,doy,day,yeardoy,week_id)) 
alldoys <- subset(alldoys, gsyear >= 2002 & gsyear <= 2013) 
ad <- alldoys[,c("gsyear","year","doy","day","yeardoy", "month","week_id")]
names(ad)[names(ad) == "month"] <- "monthid"
names(ad)[names(ad) == "week_id"] <- "weekid"

# static variables 
ransample <- read.dta13(paste("pfb1_25deg_ag_withstaticvar.dta",sep=""))

# DAILY PANEL DATA 
# daily BEST Tmax in degree C
bestpanel <- read.dta13(paste("panel_25km_BEST.dta", sep=""))
bestpanel$yeardoy <- as.numeric(as.character(bestpanel$yeardoy))
bestpanel  <-bestpanel[order(bestpanel$u_id25, bestpanel$yeardoy),]
bestpanel[1:(dim(bestpanel)[1]-1),c("tminb2")] <- bestpanel[2:dim(bestpanel)[1],c("tminb")]

# daytime LST in degree C
lstpanel <- read.dta13(paste("panel_25km_lst.dta", sep="")) 
lstpanel$yeardoy <- as.numeric(as.character(lstpanel$yeardoy))

t <- Sys.time()
lstbest <- merge(bestpanel,lstpanel, by =c("u_id25","yeardoy"), all = TRUE) # to preseve BEST Tmax, Tmin for edd
print(Sys.time()-t)

pptpanel <- read.dta13(paste("panel_25km_ppt.dta", sep=""))
pptpanel$yeardoy <- as.numeric(as.character(pptpanel$yeardoy))

t <- Sys.time()
dpanel <- merge(lstbest,pptpanel, by =c("u_id25","yeardoy"), all = TRUE)
print(Sys.time()-t)

rm(lstbest,pptpanel)

## Merge with ad to add year, month, week_id, doy
dpanel <- merge(dpanel,ad,by=c("yeardoy"))

ypanel <- read.dta13(paste("panel_25km_nfa.dta", sep=""))

t <- Sys.time()
main_panel <- merge(dpanel,ypanel,by=c("u_id25"), all = TRUE)
print(Sys.time()-t)

main_panel<- merge(main_panel,ransample, by =c("u_id25"), all = TRUE)

main_panel <- subset(main_panel, select=-c(x.x, y.x))

names(main_panel)[names(main_panel) == 'x.y'] <- 'x' 
names(main_panel)[names(main_panel) == 'y.y'] <- 'y'

## Compute extra Terrestial radiation (Allen et al. 1998, FAO guidelines)
lat_rad <- main_panel$y*pi/180
dr = 1+0.033*cos(main_panel$doy*2*pi/365) # Earth-Sun distance
dec = 0.4093 * sin ((2*pi/365)*main_panel$doy - 1.39) # solar Dec
ws = acos(-1*tan(lat_rad) *tan(dec))# Note: Lat in radians
R = 24/pi *  4.92* dr *((ws *sin(lat_rad))*sin(dec)+cos(lat_rad)*(cos(dec)*sin(ws))) 
main_panel$rmjm2 <- round(R,digits = 2)


setwd(out_path)
main_panel <- as.data.frame(lapply(main_panel,as.numeric))
save.dta13(main_panel,paste("panel025_ag.dta",sep=""))









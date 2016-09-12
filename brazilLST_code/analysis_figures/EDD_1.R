rm(list =ls())
library(reshape)
library(dplyr)
library(stringr)
library(raster)
library(foreign)
library(readstata13)
data_path <- "E:/brazil_lst/data"
code_path <- "E:/brazil_lst/data"
setwd(data_path)
source(paste(code_path,"/edd_v.R", sep =""))
tthresh <- 35 # Temperature threshold used
year <- 2012

# load data - CHANGE FOR ALL YEARS!!! 
allpanel <-  read.dta13("edd_new25.dta")


# but the BEST dataset includes 20013213 Tmin too, so the last yeardoy of gsyear 2013 gets correct next day tmin value
# 2013213 will always be the last yeardoy for each strata_id which actually get the wrong nextday tmin value (i.e. next strata_id)
# however, we don't use it in the analysis since it not within gsyear 2013 
allpanel <- subset(allpanel, yeardoy < 2013213)

# Run the Vectorized EDD code

allpanel$edd_lst <- edd_v(allpanel$tmaxlst,allpanel$tminb, allpanel$tminb2,thresh=tthresh)
print('done')
allpanel$edd_lstadj <- edd_v(allpanel$lstadj, allpanel$tminb, allpanel$tminb2,thresh=tthresh)
print('done')

# consider rmjm2 only on available days- will later be used to compute rscale factor
allpanel$rmjm2A <- allpanel$rmjm2
allpanel$rmjm2[is.na(allpanel$tmaxlst)] <- NA

# to count avail days
allpanel$numdays <- NA
allpanel$numdays[!is.na(allpanel$tmaxlst)] <- 1

# get yearly sum extraterrestail radiation 

etrsum <- raster("yearlySumETr366.tif")

ipts1 <- subset(allpanel, select = c(u_id25, x, y))
ipts1 <-aggregate(ipts1, by=list(ipts1$u_id25), 
                  FUN=mean, na.rm=TRUE)


ipts <- ipts1
#names(agpts)
coordinates(ipts)=~x+y
proj4string(ipts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

ipts1$r_ysum <- extract(etrsum, ipts, method='simple')

stateras <- raster('stateIDraster.tif')
ipts1$state_id <- extract(stateras, ipts, method='simple')

ipts1 <- ipts1[,c("u_id25","r_ysum", "state_id")]
# Merge with panel data
allpanel <- merge(allpanel,ipts1,by=c("u_id25"))

# Aggregate to get sum and mean
yearp <-aggregate(allpanel, by=list(allpanel$u_id25), FUN=mean, na.rm=TRUE)
yearsum  <-aggregate(allpanel, by=list(allpanel$u_id25), FUN=sum, na.rm=TRUE)

# Get sums for desired variables
yearp$numdays <- yearsum$numdays # sum
yearp$edd_lst <- yearsum$edd_lst # sum
yearp$edd_lstadj<- yearsum$edd_lstadj # sum
yearp$rmjm2A <- yearsum$rmjm2A # sum
yearp$r_ysum <- yearsum$r_ysum

# R scale factor to account for missing days. chances that EDD values are larger when more days are available
yearp$r_scale <- yearp$r_ysum/yearp$rmjm2A

eddyearly <- yearp[, which(colnames(yearp) %in% c("u_id25","y", 
                                                  "x","edd_lst","edd_lstadj","numdays","r_scale", "state_id"))]

save.dta13(eddyearly , paste("edd_25_daily",".dta",sep=""))

## Compute Statewise EDD

eddyearly$edd_lstsc <- eddyearly$edd_lst*eddyearly$r_scale
eddyearly$edd_lstadjsc <- eddyearly$edd_lstadj*eddyearly$r_scale


# collapse by state

eddout <- subset(eddyearly, select=c(state_id, edd_lstsc, edd_lstadjsc))

eddout1 <-aggregate(eddout, by=list(eddout$state_id), FUN=mean, na.rm=TRUE)

save.dta13(eddout1, 'edd_state.dta')

rm(list = ls())
library(raster)
library(maptools)
library(reshape)
library(readstata13)
removeTmpFiles(h=1)
code_path <- "/cluster/shared/jangus01/brazil_lst/code_path"
data_path  <- "/cluster/tufts/bhattarai02/BrazilLST/LST_Aqua/"
out_path <- "/cluster/tufts/duncan/"
setwd(code_path)
#function that create year, month, doy, day, week_id, year panel
source("Get_ymdoys.R")
alldoys <- Get_ymdoys(2000,2014,gs_startMonth=1,gs_endMonth=12)
alldoys <- subset(alldoys, year >= 2003) #DOY for year

# load sample data
ransample <- read.dta13(paste("lst_station_points.dta",sep=""))

# start Extracting LST values
for (igsyear in 2012) {
setwd(data_path)
print(igsyear)
gs_id <- ransample[, c("modislat", "modislon", "pixelid")]
gs_id_qc <- gs_id
# create spatial points data frame for LST extraction
ran_points <- gs_id
coordinates(ran_points)=~modislon+modislat
proj4string(ran_points)=CRS("+init=epsg:4326")
#subset year
ad <- subset(alldoys, year == igsyear)
yeardoy <- ad$yeardoy
for (i in yeardoy) {
print(i)
t <- Sys.time()
## MYD daytime LST and QC
lstDayfname <- paste(data_path,"/MYD11A1.A", i, ".LST_Day_1km.tif", sep="")
qcDayfname <- paste(data_path, "/MYD11A1.A", i, ".QC_Day.tif", sep="")
# check if the file exists in the folder or not
if (file.exists(lstDayfname)){
lst <- raster(lstDayfname) 
gs_id$newcol <- extract(lst, ran_points, method="simple", na.rm=FALSE)
gs_id$newcol <- gs_id$newcol* 0.02 - 273.15
names(gs_id)[names(gs_id) == "newcol"] <- paste(i)
}
if (file.exists(qcDayfname)){
qc <- raster(qcDayfname)
gs_id_qc$newcol <- extract(qc, ran_points, method="simple", na.rm=FALSE) 
names(gs_id_qc)[names(gs_id_qc) == "newcol"] <- paste(i)
removeTmpFiles(h=1) # clear memory
}
print(Sys.time()-t)
}
#reshape wide to long
gs_id <- melt(gs_id, id=c("pixelid", "modislon", "modislat"))
names(gs_id)[names(gs_id) == "variable"] <- "yeardoy"
names(gs_id)[names(gs_id) == "value"] <- "lst"
#MODIS QC info
#reshape wide to long 
gs_id_qc <- melt(gs_id_qc, id=c("pixelid"))
names(gs_id_qc)[names(gs_id_qc) == "variable"] <- "yeardoy"
names(gs_id_qc)[names(gs_id_qc) == "value"] <- "qc"
gs_id_qc$qcbin1 <- 0  # Highest Quality
gs_id_qc$qcbin1[gs_id_qc$qc == 0] <- 1
gs_id_qc$qcbin2 <- 0  # less than 1 K error
gs_id_qc[which(gs_id_qc$qc %in% c(1,5,17,21)), c("qcbin2")] <- 1
gs_id_qc$qcbin3 <- 0  #  1- 3 K error
gs_id_qc[which(gs_id_qc$qc %in% c(65,69,81,85,129,133,145,149)), c("qcbin3")] <- 1
#drop observations with > 3 K error
gs_id <- merge(gs_id, gs_id_qc, by=c("pixelid", "yeardoy"))
#colnames(gs_id)
gs_id <- gs_id[, c("pixelid","yeardoy","lst","qc","qcbin1", "qcbin2", "qcbin3")]
setwd(out_path)
save.dta13(gs_id, paste("lst_st_",igsyear,".dta",sep=""))
}

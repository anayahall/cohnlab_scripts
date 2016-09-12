# Identify eligible GFC pixels based on pfb1 method

# gdal needs to be installed

rm(list = ls())

# SET FILE PATHS

data_path <- 'E:/brazil_lst/data/forest_universe'
out_path <- 'E:/brazil_lst/data'
setwd(data_path)

# CALL LIBRARIES
library(raster)
library(maptools)
library(sampling)
library(rgdal)
library(readstata13)
library(foreign)

# CREATE STRING OF INPUT FILENAMES
fname <- c('pfb1_forest_universe')

br <- getData('GADM', country='Brazil', level=0)
brstates <- getData('GADM', country='Brazil', level=1)
br_ext <- extent(br)
br_ras <- raster(br)

# raster of br states
state_ras <- rasterize(brstates,br_ras,field="ID_1")

#Crop to Brazil extent - use gdal

a <- paste('cd /d e:/brazil_lst/data/forest_universe', sep="")
system(a, show.output.on.console=T)
c <- paste('gdalwarp -cutline BRA_adm0.shp -crop_to_cutline -dstalpha ',fname, '.tif ',fname,'_br.tif', sep="")
system(c, show.output.on.console = TRUE)

ras <- raster('pfb1_forest_universe_br.tif')
all_p <- data.frame(rasterToPoints(ras))
names(all_p) <- c("x","y","run_length")
all_p$u_id <- 1:dim(all_p)[1]
all_p <- subset(all_p, run_length >= 4)

all_p$start <- 2002
all_p$end <- all_p$start + all_p$run_length - 1

# now reset end in 2012 to 2011 to get forest cover in t-1
all_p$end [all_p$end >= 2012] <- 2011

setwd(out_path)
save.dta13(all_p, 'gfc_universe_pfb1.dta')



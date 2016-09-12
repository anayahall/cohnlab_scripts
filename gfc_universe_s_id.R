### GFC sample generation near ag ###

# gdal needs to be installed

rm(list = ls())

# SET FILE PATHS
data_path <- "E:/brazil_lst/data/forest_universe"
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

### This makes s_id 

all_p <- data.frame(rasterToPoints(ras))
names(all_p) <- c("x","y","run_length")
all_p$u_id <- 1:dim(all_p)[1]
all_p <- subset(all_p, run_length >= 4)
inlocs <- all_p
coordinates(inlocs)=~x+y
proj4string(inlocs)=CRS("+init=epsg:4326")

# Brazil shp
br <- getData('GADM', country='Brazil', level=0)
br_ras <- raster(br)
res(br_ras) <- 0.25
values(br_ras) <- 1:ncell(br_ras)
br_ras <- crop(br_ras, br)

all_p$s_id <- extract(br_ras, inlocs, method='simple')

setwd(out_path)
save.dta13(all_p, 'gfc_universe_s_id.dta')
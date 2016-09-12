#Station Density Map

rm(list = ls())
detach(package:ggplot2, unload=TRUE)
library(lattice)
library(latticeExtra)
library(raster)
library(maptools)
library(rgdal)
library(rasterVis)
library(grid)
library(LSD)
library(MASS)
library(devtools)
library(RColorBrewer)

removeTmpFiles()

data_path <- "E:/brazil_lst/data"
setwd(data_path)

br <- getData('GADM', country='Brazil', level=0)
brstates <- getData('GADM', country='Brazil', level=1)
i <- read.csv("stationDensity025.csv", header=T)
ipts <- read.csv('stationpoints.csv', header=T)

lon <- i$x
lat <- i$y

xy <- cbind(lon, lat)

coordinates(i)=~x+y

proj4string(i)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# make points spatial
coordinates(ipts)=~lon+lat
proj4string(ipts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Get Brazil pts
ipts_br <- ipts[br, ]

#Generate Brazil Raster

r = raster(i)
res(r) <- 0.25
projection(r) = CRS("+init=epsg:4326")

#station density
out1 <- rasterize(xy, r, i$pden1000km, background = NA) 

out1 <- mask(out1, br)

colt1 <- colorRampPalette(c("yellow", "red"))(256)
myTheme <- BTCTheme()
myTheme$panel.background$col = 'gray93' 

p1 <- levelplot(out1, layers=1, col.regions=colt1, margin=F, scales = list(draw = TRUE),
                at=seq(30,175, length=50), par.settings = myTheme) + layer(sp.points(ipts_br, col='black')) + layer(sp.polygons(br, lwd=0.8, col='black'))


tiff(filename=paste("Fig_station_density.tiff",sep=""), 
     units="in", 
     width=4, 
     height=4, 
     pointsize=4, 
     res=300)

plot.new()
par(mar=c(0.1, 0.1, 0.1, 0.1), oma = c(0.1, 0.1, 0.1, 0.1))
print(p1, split = c(1, 1, 1, 1), more = F)

dev.off()

# #### 3 way plot
# 
# library(ggplot2)
# library(readstata13)
# data_path <- "Z:/LSTProjecttest/master_data/data"
# setwd(data_path)
# #set file paths
# infile <- read.dta13('Fig_1_input_data.dta')
# 
# 
# #Create three-way visualization following: https://www.r-bloggers.com/using-2d-contour-plots-within-ggplot2-to-visualize-relationships-between-three-variables/
# 
# infile$agcut <- cut(infile$nag_100km, 6)
# 
# breaks <- levels(unique(infile$agcut))
# 
# 
# p2 <- ggplot() +
#   geom_tile(data = infile, aes(qpbsumfa100, qstden1000, nag_100km, fill = agcut)) +
#   geom_contour(color = 'white', alpha = 0.5) +
#   theme_bw() +
#   xlab('NFA loss (100 km)') +
#   ylab('Station Density (1000 km)') +
#   scale_fill_manual(values = c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
#                     name = 'Ag. Density', breaks = breaks, labels = breaks)
# 
# 
# 
# tiff(filename=paste("Fig_1b.tiff",sep=""), 
#      units="in", 
#      width=4, 
#      height=4, 
#      pointsize=4, 
#      res=300)
# 
# plot.new()
# par(mar=c(0.1, 0.1, 0.1, 0.1), oma = c(0.1, 0.1, 0.1, 0.1))
# print(p2, split = c(1, 1, 1, 1), more = F)
# 
# dev.off()
# detach(package:ggplot2, unload=TRUE)
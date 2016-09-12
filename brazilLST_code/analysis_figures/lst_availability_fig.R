#Daily Panel LST availability

rm(list = ls())
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
i <- read.dta13(paste(data_path,"/lst_availability.dta", sep=""))

lon <- i$lon
lat <- i$lat

xy <- cbind(lon, lat)

coordinates(i)=~lon+lat

proj4string(i)=CRS("+init=epsg:4326")

#Generate Brazil Raster

r = raster(i)
res(r) <- 0.25
projection(r) = CRS("+init=epsg:4326")

#Make rasters
#LST availability
out1 <- rasterize(xy, r, i$lstsum, background = NA) 

#QC bin 3
out2 <- rasterize(xy, r, i$qcbin3sum, background = NA)

colt1 <- colorRampPalette(c("yellow", "red"))(256)
myTheme <- BTCTheme()
myTheme$panel.background$col = 'gray93' 


p1 <- levelplot(out1, layers=1, col.regions=colt1, margin=F, scales = list(draw = TRUE),
                at=seq(0,300, length=50), par.settings = myTheme) + layer(sp.polygons(brstates, lwd=0.8, col='black'))

p2 <- levelplot(out2, layers=1, col.regions=colt1, margin=F, scales = list(draw = TRUE),
                at=seq(0,300, length=50), par.settings = myTheme) + layer(sp.polygons(brstates, lwd=0.8, col='black'))


tiff(filename=paste("Fig_lstavail.tiff",sep=""), 
     units="in", 
     width=8, 
     height=4, 
     pointsize=4, 
     res=300)

plot.new()
par(mar=c(0.1, 0.1, 0.1, 0.1), oma = c(0.1, 0.1, 0.1, 0.1))
print(p1, split = c(1, 1, 2, 1), more = TRUE)
print(p2, split = c(2, 1, 2, 1), more = FALSE)

grid.text('a', x=0.02, y=0.98, rot=0, 
          gp = gpar(col=1,font=2,cex=3))
grid.text('b', x=0.5, y=0.98, rot=0, 
          gp = gpar(col=1,font=2,cex=3)) 

dev.off()

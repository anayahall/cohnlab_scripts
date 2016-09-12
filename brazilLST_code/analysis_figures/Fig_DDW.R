# Generate maps of DDW and temp. value of remnant forest as of 2011

rm(list=ls())

#warming maps made by DDW.do
setwd('E:/brazil_lst/data') 


library(grid)
library(raster)
library(rgdal)
library(colorRamps)
library(gplots)
library(rasterVis)
library(grid)
library(maptools)
library(sp)
library(readstata13)


i <- read.dta13('ddw_fig_data.dta')


lon <- i$x
lat <- i$y

xy <- cbind(lon, lat)

coordinates(i)=~x+y

proj4string(i)=CRS("+init=epsg:4326")

#Generate Brazil Raster

r = raster(i)
res(r) <- 0.25
projection(r) = CRS("+init=epsg:4326")

myproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
shp <- getData('GADM', country='Brazil', level=1)


# LST cumulative forest warm
out1 <- rasterize(xy, r, i$ltcsum, background = NA) # max DDW detected by LST  

# BEST cumulative forest warm
out2 <- rasterize(xy, r, i$btcsum, background = NA) # max DDW detected by BEST

# LST BEST diff
out3 <- rasterize(xy, r, i$cdiff, background = NA) # DDW not detected by BEST but detected by LST

# LST cumulative forest warm
out4 <- rasterize(xy, r, i$lfcsum, background = NA)  

# BEST cumulative forest warm
out5 <- rasterize(xy, r, i$bfcsum, background = NA) 

# LST BEST diff
out6 <- rasterize(xy, r, i$fdiff, background = NA) 


myTheme <- BTCTheme()
myTheme$panel.background$col = 'gray93' 

# color palattes

colt1 <- colorRampPalette(c("yellow", "red", "red2", "red4"))(256)
colt2 <- colorRampPalette(c("blue", "white"))(256)

p1 <- levelplot(out1, layers=1, col.regions=colt1, margin=F, scales = list(draw = TRUE),at=seq(0,4.5, length=50), par.settings = myTheme) + layer(sp.polygons(shp, lwd=0.8, col='black'))

p2 <- levelplot(out2, layers=1, col.regions=colt1, margin=F, scales = list(draw = TRUE),at=seq(0,4.5, length=50), par.settings = myTheme) + layer(sp.polygons(shp, lwd=0.8, col='black'))

p3 <- levelplot(out3, layers=1, col.regions=colt1, margin=F, scales = list(draw = TRUE),at=seq(0,4.5, length=50), par.settings = myTheme) + layer(sp.polygons(shp, lwd=0.8, col='black'))

p4 <- levelplot(out4, layers=1, col.regions=colt2, margin=F, scales = list(draw = TRUE),at=seq(-8,0, length=15), par.settings = myTheme) + layer(sp.polygons(shp, lwd=0.8, col='black'))

p5 <- levelplot(out5, layers=1, col.regions=colt2, margin=F, scales = list(draw = TRUE),at=seq(-8,0, length=15), par.settings = myTheme) + layer(sp.polygons(shp, lwd=0.8, col='black'))

p6 <- levelplot(out6, layers=1, col.regions=colt2, margin=F, scales = list(draw = TRUE),at=seq(-8,0, length=15), par.settings = myTheme) + layer(sp.polygons(shp, lwd=0.8, col='black'))

dev.off()

#Write out DDW figs

tiff(filename="Fig_DDW.tiff",
     units="in", 
     width=13, 
     height=4, 
     pointsize=4, 
     res=300)

plot.new()
par(mar=c(0.1, 0.1, 0.1, 0.1), oma = c(0.1, 0.1, 0.1, 0.1))
print(p1, split = c(1, 1, 3, 1), more = TRUE)
print(p2, split = c(2, 1, 3, 1), more = TRUE)
print(p3, split = c(3, 1, 3, 1), more = FALSE)

grid.text('a', x=0.02, y=0.98, rot=0, 
          gp = gpar(col=1,font=2,cex=3))
grid.text('b', x=0.33, y=0.98, rot=0, 
          gp = gpar(col=1,font=2,cex=3)) 
grid.text('c', x=0.66, y=0.98, rot=0, 
          gp = gpar(col=1,font=2,cex=3)) 

dev.off()

#Write out temp value in forest lest

tiff(filename="Fig_FVL.tiff",
     units="in", 
     width=13, 
     height=4, 
     pointsize=4, 
     res=300)

plot.new()
par(mar=c(0.1, 0.1, 0.1, 0.1), oma = c(0.1, 0.1, 0.1, 0.1))
print(p4, split = c(1, 1, 3, 1), more = TRUE)
print(p5, split = c(2, 1, 3, 1), more = TRUE)
print(p6, split = c(3, 1, 3, 1), more = FALSE)

grid.text('a', x=0.02, y=0.98, rot=0, 
          gp = gpar(col=1,font=2,cex=3))
grid.text('b', x=0.33, y=0.98, rot=0, 
          gp = gpar(col=1,font=2,cex=3)) 
grid.text('c', x=0.66, y=0.98, rot=0, 
          gp = gpar(col=1,font=2,cex=3)) 

dev.off()

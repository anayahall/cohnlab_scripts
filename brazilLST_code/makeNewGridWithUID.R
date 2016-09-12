makeNewGridWithUID <- function (bb, gsize1,buffer){
# Make a newGrid with unique ID
# wotks with WGS and quare pixels only
#gsize1 = 0.05

# round to 2, this might need to be changed, depending upon the grid size
Xmin1 <- round(bb[1]-buffer,2)
Xmax1 <- round(bb[2]+buffer,2)
Ymax1 <- round(bb[4]+buffer,2)
Ymin1 <- round(bb[3]-buffer,2)

# change the bb to fit the grid
newbb <- c(Xmin1,Xmax1,Ymin1,Ymax1)

lonR1 <- seq(Xmin1,Xmax1,by = gsize1)
#latR1 <- seq(Ymin1,Ymax1, by = gsize1)
latR1 <- seq(Ymax1,Ymin1, by = -gsize1) # Lat is decreasing

nrows1 <- length(latR1)
ncols1 <- length(lonR1)

longrids1 <- t(as.matrix(replicate(nrows1,lonR1)))
latgrids1 <- as.matrix(replicate(ncols1,latR1))

##%% NOTE: The lat and lon grids refers to the corner of each, to get the coordiniates of the center pixel add half of the pixel size (only applicable for square pixels)
##longrids1kmCenter = longrids+gsize/2; % monotonously increasing, so use +
##latgrids1kmCenter = latgrids-gsize/2; % monotonously decreasing, so use -

longrids1 <- longrids1  + gsize1/2
latgrids1 <- latgrids1  - gsize1/2


#image(longrids)
longrids1<- raster(longrids1)
extent(longrids1) <- newbb
projection(longrids1) <- CRS("+proj=longlat +datum=WGS84")
#plot(longrids1)

#image(latgrids)
latgrids1<- raster(latgrids1)
extent(latgrids1) <- newbb
projection(latgrids1) <- CRS("+proj=longlat +datum=WGS84")
#plot(latgrids1)
# 
# nrows <- ncol(latgrids1)
# ncols <- ncol(latgrids1)
# New Mat Val (unique) corresponding to the MW grid
matval <- seq(1,nrows1*ncols1,1)
mat <- matrix(seq(1,nrows1*ncols1,1),nrows1,ncols1)
r_mat <- raster(mat)
extent(r_mat) <- newbb
projection(r_mat) <- CRS("+proj=longlat +datum=WGS84")
#plot(r_mat)

return(r_mat)


}
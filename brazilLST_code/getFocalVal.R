# This Function is written to expediate the focal negighhorhood process for random samples
## INPUTS
# Inupt dataframe with row(x) and col (y) (where row and col id are extracted based on 
# x and y location within the raster used in focal stats). 
# x and y also corresponds to locations in Mat (raster converetd into matrix) 
# gf is the focal matrix
# rani is the row number for random sample 
# dat = random sample dataset with col and row names
# Mat  = matrix that focal stats performed on

library(raster)

getFocalVal <- function (rani,dat,gf,Mat) {
  #Mat <- as.matrix(Mat)
  nrows <- nrow(Mat)
  ncols <- ncol(Mat)
  r <- dim(gf)[1]
  c <- dim(gf)[2]
  
  rowi = dat$row[rani] # getting row and column for central pixel
  coli= dat$col[rani]
  
  rowsel <- seq(rowi-floor(r/2),rowi+floor(r/2),1) #floor = round down
  #rowsel
  colsel <- seq(coli-floor(c/2),coli+floor(c/2),1)
  #colsel
  
  
  #Outside matrix
  rowsel[rowsel > nrows] <- 0
  colsel[colsel > ncols] <- 0
  
  rowcolsel <- matrix(1,nrow=r,ncol=c)
  
  # TO deal with the boundary pixels. Make NaNs and Ignore- But it's better to increase bounding box beforehand
  if (min(rowsel) <=0|min(colsel) <=0){
    rowcolsel <- matrix(NA,nrow=r,ncol=c)
    
    rowsel1 <- rowsel
    if (min(rowsel1) <=0) {
      
      rowsel1[rowsel1 <= 0] <- NA
      rowsel1[is.na(rowsel1)] <- min(rowsel1, na.rm=TRUE)
      #rowsel1
    }
    
    colsel1 <- colsel
    if (min(colsel1) <=0) {
      
      colsel1[colsel1 <= 0] <- NA
      colsel1[is.na(colsel1)] <- min(colsel1, na.rm=TRUE)
    }
    rowsel <- rowsel1
    colsel <- colsel1
    
  }
  
  apart = Mat[c(rowsel),c(colsel)]
  
  apart <- apart * rowcolsel
  p <- sum(apart * gf, na.rm=TRUE) #multiply by focal neighborhood
  if (sum(is.na(apart))==ncell(apart)){
    p=NA
  }
  
  return(p)
  
}

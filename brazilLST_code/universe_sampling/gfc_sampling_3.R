# Random sample based on maximum halo variation

# Locations near rivers omitted in gfc_sampling_2_CL.R

# http://stackoverflow.com/questions/7508229/how-to-create-a-column-with-a-quartile-rank

rm(list = ls())

setwd('E:/brazil_lst/data')

# CALL LIBRARIES
library(raster)
library(maptools)
library(sampling)
library(readstata13)

#set this option so R does not print out in scientific notation
options(scipen = 999)

# use these for computation of halos - avoid computing halos for nfa1_1* as this is pixel location
halo <- c(2000, 4000, 10000)
halo2 <- c(2000, 4000)

# once halos have been computed use this vector for MHV sampling
halo3 <- c(1, 2000, 4000, 10000)

# length of one side of MODIS LST footprint in GEE
pixdim <- 926.6254330554165

infile <- read.dta13('gfc_universe_nfa1.dta')

# set run lengths of 12 to = 11 as we are working with lags of forest cover. End year is 2011 not 2012
infile$run_length [infile$run_length == 12] <- 11

infile <- subset(infile, select=c(x, y, u_id, run_length, nfa1_1st, nfa1_1end, nfa1_2000st, nfa1_2000end, 
                                  nfa1_4000st, nfa1_4000end, nfa1_10000st, nfa1_10000end))

#convert nfa to proportions

for (ha in halo3){
  
  eval(parse(text=paste('infile$nfa1_',ha,'st <- infile$nfa1_',ha,'st*0.01', sep = '')))
  eval(parse(text=paste('infile$nfa1_',ha,'end <- infile$nfa1_',ha,'end*0.01', sep = '')))
  
}

# drop locations with nfa1 below threshold - these are due to reprojection

eval(parse(text=paste('infile$nfa1_1st [infile$nfa1_1st < 0.5] <- NA', sep = '')))
eval(parse(text=paste('infile$nfa1_1end [infile$nfa1_1end < 0.5] <- NA', sep = '')))

for (ha in halo) {
  
  eval(parse(text=paste('infile$nfa1_',ha,'st [infile$nfa1_',ha,'st == 0] <- NA', sep = '')))
}

infile <- na.omit(infile)

# convert halos to area - we convert pixel location to area seperately as it is a square pixel

eval(parse(text=paste('infile$nfa1_1st <- infile$nfa1_1st*pixdim*pixdim', sep = '')))
eval(parse(text=paste('infile$nfa1_1end <- infile$nfa1_1end*pixdim*pixdim', sep = '')))

for (ha in halo){
   
  eval(parse(text=paste('infile$nfa1_',ha,'st <- infile$nfa1_',ha,'st*',ha,'*',ha,'*pi', sep = '')))
  eval(parse(text=paste('infile$nfa1_',ha,'end <- infile$nfa1_',ha,'end*',ha,'*',ha,'*pi', sep = '')))
}

# compute halos

#do not compute halo for pixel location
infile$h1_st <- infile$nfa1_1st
infile$h1_end <- infile$nfa1_1end

idx = 1

for (ha in halo) {
  
  if (ha == 2000) {
    
    eval(parse(text=paste('infile$h',ha,'_st <- infile$nfa1_',ha,'st', sep = '')))
    eval(parse(text=paste('infile$h',ha,'_end <- infile$nfa1_',ha,'end', sep = '')))
    
  } else {
    
    ha2 <- halo2[idx]
    eval(parse(text=paste('infile$h',ha,'_st <- infile$nfa1_',ha,'st - infile$nfa1_',ha2,'st', sep = '')))
    eval(parse(text=paste('infile$h',ha,'_end <- infile$nfa1_',ha,'end - infile$nfa1_',ha2,'end', sep = '')))
    idx <- idx+1
  }
  
}


# drop negative halos

for (ha in halo3) {
 
  eval(parse(text=paste('infile$h',ha,'_st [infile$h',ha,'_st <= 0] <- NA', sep = ""))) 
  eval(parse(text=paste('infile$h',ha,'_end [infile$h',ha,'_end <= 0] <- NA', sep = ""))) 
  
}

infile <- na.omit(infile)

# change in halos

for (ha in halo3) {
  
  eval(parse(text=paste('infile$change_h_',ha,' <-  (infile$h',ha,'_st - infile$h',ha,'_end)/(infile$h',ha,'_st + infile$h',ha,'_end)*0.5', sep = ""))) 
  
  eval(parse(text=paste('lun <- length(unique(infile$change_h_',ha,'))', sep ='')))
  
  print(lun)
  if (lun == 1){
    
    eval(parse(text=paste('infile$catchange_',ha,'<- 5', sep = '')))
    
  } else {
  
  
  eval(parse(text=paste('infile$catchange_',ha,' <- as.integer(cut(infile$change_h_',ha,', unique(quantile(infile$change_h_',ha,', probs=0:5/5, na.rm=TRUE)), include.lowest=TRUE))', sep='')))
  }
} 

#initialize variables for dissimlarity index

 infile$nr_rank <- 0
 infile$dr_rank <- 5
 infile$rank_weight <- 0
 infile$dsim <- 0

for (ha in halo3) {
  for (za in halo3) {
    
    eval(parse(text=paste('infile$nr_rank <- abs(infile$catchange_',ha,' - infile$catchange_',za,')', sep='')))
    infile$rank_weight <- infile$nr_rank / infile$dr_rank
    infile$dsim <- infile$dsim + infile$rank_weight
     
  }
}

infile$ok_4_w <- NA

gfc_run <- unique(infile$run_length)

for (zz in gfc_run) {
  
  eval(parse(text=paste('infile$dsim_',zz,' <- infile$dsim', sep='')))
  eval(parse(text=paste('infile$dsim_',zz,' [infile$run_length != ',zz,'] <- NA', sep='')))
  eval(parse(text=paste('infile$catdsim_',zz,' <- as.integer(cut(infile$dsim_',zz,', unique(quantile(infile$dsim_',zz,', probs=0:4/4, na.rm=TRUE)), include.lowest=TRUE))', sep='')))
  eval(parse(text=paste('cat_max <- max(infile$catdsim_',zz,', na.rm=TRUE)', sep='')))
  eval(parse(text=paste('infile$catdsim_',zz,' [infile$catdsim_',zz,' != cat_max] <- 0', sep='')))
  eval(parse(text=paste('infile$catdsim_',zz,' [infile$catdsim_',zz,' != 0] <- 1', sep='')))
  eval(parse(text=paste('infile$ok_4_w [infile$catdsim_',zz,' == 1] <- 1', sep='')))   

}

infile <- subset(infile, ok_4_w == 1)

# Make sure sample is balanced on gfc run lengths

ransample <- data.frame()

#set seed for replication
set.seed(1)

for (i in gfc_run){
  
  seldata <- subset(infile, run_length==i)
  
  if (dim(seldata)[1] < 889) {
    
    r1 <- seldata
  } else {
  
  r1 <- r1 <- seldata[sample(1:nrow(seldata), size=889),]
  
  }
  
  ransample <- rbind(ransample,r1)

}
 
ransample <- subset(ransample, select=c(u_id, x, y, run_length))
save.dta13(ransample, 'mhv_4_nw_gfc.dta')



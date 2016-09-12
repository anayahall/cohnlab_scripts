#extract BEST for random sample near ag

rm(list = ls())
library(raster)
library(maptools)
library(reshape)
library(ncdf4)
library(readstata13)


# Paths in Cluster
code_path <- '/cluster/shared/jangus01/brazil_lst/code_path'
out_path <- '/cluster/tufts/duncan/' 
best_path <- '/cluster/tufts/bhattarai02/best'
ppt_path <- '/cluster/tufts/bhattarai02/persiann/cdr/persiann'
nfc_path <- '/cluster/tufts/bhattarai02/nfbras' 
lst_path  <- '/cluster/tufts/bhattarai02/BrazilLST/LST_Aqua/'


#Get growing season day IDs
setwd(code_path)

source(paste("Get_ymdoys.R", sep=""))
alldoys <- Get_ymdoys(2000,2014,gs_startMonth=8,gs_endMonth=7)
alldoys <- subset(alldoys, year >= 2000 & year < 2014)
alldoys$bestind <- seq(1, nrow(alldoys), 1)
ad <- alldoys[,c("year","doy","yeardoy","gsyear", "bestind")]

years <- c(2012)


#read in random sample
stpts <- read.dta13(paste("pfb1_25deg_sample_with_ag.dta",sep=""))
stpts <- stpts[, c("u_id25", "x", "y")]

infocol <- dim(stpts)[2] # after this column LST data is added

stpts1 <- stpts # make a copy of random sample for creating yearly panels
coordinates(stpts)=~x+y
proj4string(stpts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

br <- c(-84.06186, -28.3912, -60.3116, 16.51572) # Entire South America (to get the full coast line in all directions)


#plot(lc)
#Stack all BEST
setwd(best_path)
lmin <- brick("Complete_TMIN_Daily_LatLong1_2000.nc", varname='temperature')
cmin <- brick("Complete_TMIN_Daily_LatLong1_2000.nc", varname='climatology')
llmin <- brick("Complete_TMIN_Daily_LatLong1_2010.nc", varname='temperature')
ccmin <- brick("Complete_TMIN_Daily_LatLong1_2010.nc", varname='climatology')

lmax <- brick("Complete_TMAX_Daily_LatLong1_2000.nc", varname='temperature')
cmax <- brick("Complete_TMAX_Daily_LatLong1_2000.nc", varname='climatology')
llmax <- brick("Complete_TMAX_Daily_LatLong1_2010.nc", varname='temperature')
ccmax <- brick("Complete_TMAX_Daily_LatLong1_2010.nc", varname='climatology')

for (bindex in 1:2){
  
  # BEST MIN
  if (bindex==1)  {
    l <- lmin
    c <-cmin
    ll <- llmin
    cc <- ccmin
    print("processing BEST TMIN")
    
  }
  
  #BEST MAX
  if (bindex ==2){
    l <- lmax
    c <-cmax
    ll <- llmax
    cc <- ccmax
    print("processing BEST TMAX")
    
  }
  
  #crop global raster to Brazil extent
  l <- crop(l, br)
  c <- crop(c, br)
  ll <- crop(ll, br)
  cc <- crop(cc, br)
  
  lll <-stack(l, ll)
  
  #leap year climatology doy 365 = 366
  decl <- c[[365]]
  cl <- stack(c, decl)
  
  #create 2013 climatology = only months January to November
  c2013 <- cc[[1:334]]
  
  #stack of climatologies
  c_clim <- stack(cl, c, c, c, cl, c, c, c, cl, c, cc, cc, cl, c2013)
  
  #add climatology to anomaly
  lll <- overlay(lll, c_clim, fun=sum)
  
  bpanel <- data.frame()
  # for each growing season year extract BEST values
  for (i in years) {
    t <- Sys.time()
    print(i)
    z <- subset(alldoys, year == i)
    zmin <- min(z$bestind)
    zmax <- max(z$bestind)
    
    if (i==2013){
      # get the next day Tmin (for EDD computation)- tha last GS year
      z <- subset(alldoys, year == i)
      ydoymax <- max(z$yeardoy)+1
      z <- subset(alldoys, year == i|yeardoy==2013213)
      zmin <- min(z$bestind)
      zmax <- max(z$bestind)
    }
    
    best <- lll[[zmin:zmax]]
    
    b_out <- extract(best, stpts, method='simple')
    
    r_ind <- stpts1
    
    r_out <- cbind(r_ind, b_out)
    
    for (zz in 1:nrow(z)) {
      z_id <- zz+infocol
      names(r_out)[z_id] <- z$yeardoy[zz]
    }
    
    r_out[,c((infocol+1):dim(r_out)[2])] <- round(r_out[,c((infocol+1):dim(r_out)[2])] ,digits=2)
    
    # reshape- long format
    r_out <- melt(r_out, id=c("u_id25", "x", "y"))
    names(r_out)[names(r_out) == "variable"] <- 'yeardoy'
    names(r_out)[names(r_out) == "value"] <- "best"
    
    
    #get gsyear, doy, etc
    r_out <- merge(r_out, ad, by=c('yeardoy'))
    r_out <- r_out[c("u_id25","yeardoy","best")]
    
    print("Appening Yearly panels ...........................")
    print(i)
    bpanel <- rbind(bpanel,r_out)
    print(Sys.time()-t)
    
  }
  
  if (bindex==1){
    bestminpanel <- bpanel
    names(bestminpanel)[names(bestminpanel) == "best"] <- "tminb"
  }
  
  if (bindex==2){
    bestmaxpanel <- bpanel
    names(bestmaxpanel)[names(bestmaxpanel) == "best"] <- "tmaxb"
  }
}

print("Merging BEST Min and Max Panel")
bestpanel <- merge(bestminpanel,bestmaxpanel, by  = c("u_id25","yeardoy"), all=TRUE)

print("Writing Best Panel ..................")
setwd(out_path)
save.dta13(bestpanel, paste("panel_25km_BEST.dta", sep=""))

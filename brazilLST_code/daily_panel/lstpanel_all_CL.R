## make LST panel- combine all years
rm(list = ls())

library(raster)
library(reshape)
library(readstata13)

# Paths in Cluster
code_path <- '/cluster/shared/jangus01/brazil_lst/code_path'
data_path  <- '/cluster/tufts/bhattarai02/BrazilLST/LST_Aqua/'
out_path <- '/cluster/tufts/duncan/'

setwd(out_path)

# daytime LST in degree C
lstmaxpanel <- data.frame()

for (igsyear in 2003:2012){
  print(igsyear)
  t <- Sys.time()
  
  # LST Max
  setwd(out_path)
  lstmax <- read.dta13(paste("lst_mhv_4_nw_",igsyear,".dta",sep=""))
  lstmax <- subset(lstmax, !is.na(lst)) # drop NA
  lstmaxpanel <- rbind(lstmaxpanel,lstmax)
  
  print(Sys.time()-t)
}

removeTmpFiles(h=1)


print("writing LST panel........")
setwd(out_path)
save.dta13(lstmaxpanel, paste("panel_lst_mhv_4_nw.dta", sep=""))






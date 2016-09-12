## make LST panel- combine all years

rm(list = ls())
library(raster)
library(reshape)
library(readstata13)

# Paths in Cluster
code_path <- '/cluster/shared/jangus01/brazil_lst/code_path'
out_path <- '/cluster/tufts/duncan/' 
best_path <- '/cluster/tufts/bhattarai02/best'
ppt_path <- '/cluster/tufts/bhattarai02/persiann/cdr/persiann'
nfc_path <- '/cluster/tufts/bhattarai02/nfbras' 
lst_path  <- '/cluster/tufts/bhattarai02/BrazilLST/LST_Aqua/'

setwd(out_path)

# daytime LST in degree C
lstmaxpanel <- data.frame()

for (igsyear in 2003:2012){
  print(igsyear)
  t <- Sys.time()
  
  # LST Max
  lstmax <- read.dta13(paste("lst_st_",igsyear,".dta",sep=""))
  lstmax <- subset(lstmax, !is.na(lst)) # drop nas
  lstmaxpanel <- rbind(lstmaxpanel,lstmax)
  
  print(Sys.time()-t)
}
removeTmpFiles(h=1)
print("writing LST panel........")
setwd(out_path)
save.dta13(lstmaxpanel, paste("panel_lst_st.dta", sep=""))

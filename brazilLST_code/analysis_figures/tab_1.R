#create table of min-max predictions
rm(list=ls())

path <- 'E:/brazil_lst/data'
setwd(path)

library(readstata13)

g <- matrix(NA, nrow=6, ncol=4) ## change ncol to number of halos + 1

x <- c(1, 2, 4) ## change this for halo radii

## table index

rid1<-1
rid2<-2
ii <- 1

for (i in x){
  
  
  b <- read.dta13(paste('hfa',i,'.dta', sep=''))   
  
  b$estimate <- signif(b$estimate, 3)
  b$stderr <- signif(b$stderr, 3)
  
  if (b$p<0.01){
    g[rid1,ii] <- paste('="',b$estimate,'***"', sep='')}
  else if (b$p<0.05) {
    g[rid1,ii] <- paste('="',b$estimate,'**"', sep='')}
  else if (b$p<0.1) {
    g[rid1,ii] <- paste('="',b$estimate,'*"', sep='')}
  else  { 
    g[rid1,ii] <- paste('="',b$estimate,'"')}
  
  g[rid2,ii] <- paste('="(',b$stderr,')"')
  
  ii <- ii+1
}

# Add in all NFB change to column 5

for (i in 1){
  
  b <- read.dta13('ch_all_fa1.dta')
  
  b$estimate <- signif(b$estimate, 3)
  b$stderr <- signif(b$stderr, 3)
  
  if (b$p<0.01){
    g[rid1,ii] <- paste('="',b$estimate,'***"', sep='')}
  else if (b$p<0.05) {
    g[rid1,ii] <- paste('="',b$estimate,'**"', sep='')}
  else if (b$p<0.1) {
    g[rid1,ii] <- paste('="',b$estimate,'*"', sep='')}
  else  { 
    g[rid1,ii] <- paste('="',b$estimate,'"')}
  
  g[rid2,ii] <- paste('="(',b$stderr,')"')
} 

# reset row index  \

rid1<-rid1+2
rid2<-rid2+2
ii <- 1

for (i in x){
  
  b <- read.dta13(paste('hfa',i,'_best.dta', sep='')) 
  
  b$estimate <- signif(b$estimate, 3)
  b$stderr <- signif(b$stderr, 3)
  
  if (b$p<0.01){
    g[rid1,ii] <- paste('="',b$estimate,'***"', sep='')}
  else if (b$p<0.05) {
    g[rid1,ii] <- paste('="',b$estimate,'**"', sep='')}
  else if (b$p<0.1) {
    g[rid1,ii] <- paste('="',b$estimate,'*"', sep='')}
  else  { 
    g[rid1,ii] <- paste('="',b$estimate,'"')}
  
  g[rid2,ii] <- paste('="(',b$stderr,')"')
  
  ii <- ii+1
}

# Add in all NFB change to column 5

for (i in 1) {
  
  b <- read.dta13('ch_all_fa2.dta')
  
  b$estimate <- signif(b$estimate, 3)
  b$stderr <- signif(b$stderr, 3)
  
  if (b$p<0.01){
    g[rid1,ii] <- paste('="',b$estimate,'***"', sep='')}
  else if (b$p<0.05) {
    g[rid1,ii] <- paste('="',b$estimate,'**"', sep='')}
  else if (b$p<0.1) {
    g[rid1,ii] <- paste('="',b$estimate,'*"', sep='')}
  else  { 
    g[rid1,ii] <- paste('="',b$estimate,'"')}
  
  g[rid2,ii] <- paste('="(',b$stderr,')"')  
}


### MEAN CHANGE IN NFC ###

# reset row index  

rid1<-rid1+2
rid2<-rid2+2
ii <- 1

for (i in x){
  b <- read.dta13(paste('mn_',i,'.dta', sep=''))
  
  b$estimate <- signif(b$estimate, 3)
  b$stderr <- signif(b$stderr, 3)
  
  if (b$p<0.01){
    g[rid1,ii] <- paste('="',b$estimate,'***"', sep='')}
  else if (b$p<0.05) {
    g[rid1,ii] <- paste('="',b$estimate,'**"', sep='')}
  else if (b$p<0.1) {
    g[rid1,ii] <- paste('="',b$estimate,'*"', sep='')}
  else  { 
    g[rid1,ii] <- paste('="',b$estimate,'"')}
  
  g[rid2,ii] <- paste('="(',b$stderr,')"')
  
  ii <- ii+1
}

# Add in all NFB change to column 5

for (i in 1) {
  b <- read.dta13('ch_all_fa3.dta')
  
  b$estimate <- signif(b$estimate, 3)
  b$stderr <- signif(b$stderr, 3)
  
  if (b$p<0.01){
    g[rid1,ii] <- paste('="',b$estimate,'***"', sep='')}
  else if (b$p<0.05) {
    g[rid1,ii] <- paste('="',b$estimate,'**"', sep='')}
  else if (b$p<0.1) {
    g[rid1,ii] <- paste('="',b$estimate,'*"', sep='')}
  else  { 
    g[rid1,ii] <- paste('="',b$estimate,'"')}
  
  g[rid2,ii] <- paste('="(',b$stderr,')"')  
}   


#### Add in row and column headers

rownames(g) <- c("LST", "", "BEST", "", "LST-mean", "")
colnames(g) <- c("location", "2 km", "2-4 km", "All")
write.csv(g, "Table_1.csv", row.names=T)
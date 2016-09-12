#Table showing average EDD per state and average DDW driven EDD perstate

rm(list=ls())

#warming maps made by DDW.do
data_path <- 'E:/brazil_lst/data'
setwd(data_path)

library(grid)
library(raster)
library(rgdal)
library(maptools)
library(sp)
library(readstata13)

infile <- read.dta13('edd_state.dta')

myproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
shp <- getData('GADM', country='Brazil', level=1)

state_name <- shp@data$NAME_1
state_id <- shp@data$ID_1

state <- cbind(state_name, state_id)

#### EDD by state

infile <- subset(infile, !is.na(state_id))
infile <- merge(infile, state, by=c("state_id"))
infile$pct_edd_diff <- 100-((infile$edd_lstadjsc / infile$edd_lstsc)*100)


#### State forest cover change 
write.csv(infile, 'edd_state_table.csv', row.names=FALSE)
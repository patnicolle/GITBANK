#bins loop 
rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")


library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)

file.ndvi <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/",full.names = TRUE)
file.precip <- list.files("/Volumes/P_Harddrive/Annual_data/Precipitation/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)

Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}


precip1 <-(flip(t(Precipitation1), direction = "x"))
precip2 <- precip1[[13:41]]
#----------------

ndvi <- brick() 
for (k in 1:length(file.ndvi)) {
  datan = brick(file.ndvi [k])
  ndvi <- addLayer(ndvi, datan) 
}
ndvi <-(flip(t(ndvi), direction = "x"))  


seqr<- seq(from = 0,to = 2975, by = 25)

percentile <- matrix( data=NA, nrow = length(file.ndvi), ncol = length(seqr))
for (b in 1: length(file.ndvi)) {
  o <- values(ndvi[[b]])
  precip <- values(precip2[[b]])
  
  for (k in seqr) {
    
    ind <- which(precip>= k | precip< k+25) 
    a <- o[ind] 
    percentile[b,k] <- quantile(a, probs=0.5, na.rm= TRUE)
    
  } 
}

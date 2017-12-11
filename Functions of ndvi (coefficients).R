setwd("/Volumes/P_Harddrive/")


library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF)


file.ndvi <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/", full.names = TRUE)
file.precip <- list.files("/Volumes/P_Harddrive/Annual_data/Precipitation/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)

net.ndvi <- brick(file.ndvi)


ndvi <- brick() 
for (k in 1:length(file.ndvi)) {
  datan= brick(file.ndvi [k])
  ndvi <- addLayer(ndvi, datan)
}

Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap= brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}


PET1 <- brick() 
for (k in 1:length(file.PET)) {
  dataP= brick(file.PET[k])
  PET1 <- addLayer(PET1, dataP)
}



#FOR TUESDAY!! the resample didnt work || "Error in .intersectExtent(x, y, validate = TRUE) : 
  #Objects do not intersect" 
#everything else worked 



#resampling for precip and PET

 new_precipitation <- resample(Precipitation1, ndvi) 
 
 new_PET <- resample(PET1, ndvi)
 
 
 
 
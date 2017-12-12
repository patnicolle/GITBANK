#loops through grids 
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF)


file.ndvi <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/", full.names = TRUE)
file.precip <- list.files("/Volumes/P_Harddrive/Annual_data/Precipitation/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)

Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}
pre1 <-(flip(t(Precipitation1), direction = "x")) 
 
#___________________________________________________________
ndviunflipped <- brick() 
  for (k in 1:length(file.ndvi)) {
    datan = brick(file.ndvi [k])
    ndviunflipped <- addLayer(ndviunflipped, datan)
}
 ndvi <-(flip(t(ndviunflipped), direction = "x"))

#___________________________________________________________
 
 pre <- brick() 
nd <- brick() 
PET <- brick() 
#_________________________________________________________
rainfall <- pre1[[13:41]]



rainfall <- resample(rainfall, ndvi)

rainfall <- crop(rainfall, ndvi)

rainfall <- mask(rainfall, ndvi)

slope <- rainfall[[1]]
slope [slope >=0] <- NA 

for (i in 1: nrow(rainfall)) {
  for (j in 1: ncol(rainfall)) {
    
 x <- as.vector(rainfall[i,j]) 
 y <- as.vector(ndvi[i,j])

  lm2.lm <- lm(y ~ x)
 
 slope[i,j] <- lm2.lm$coefficients[2]
 
  }
}


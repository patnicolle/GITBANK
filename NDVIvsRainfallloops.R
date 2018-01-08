# finding years with anomolously wet and dry rainfall 
rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")


library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 


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

meanndvi <- mean(ndvi)
#------------------------

rainfall <- precip2

rainfall <- resample(rainfall, ndvi)

rainfall <- crop(rainfall, ndvi)

rainfall <- mask(rainfall, ndvi)

#rainfallextremes <- rainfall[[c(5,13,19,21,29)]]

meanprecip <-mean(rainfall)


rainfalldry <- rainfall

rainfallwet <- rainfall



meanPRECIP <- vector(length = nlayers(rainfall))

for(k in 1: nlayers(rainfall)) {
  meanPRECIP[k] <- mean(values(rainfall[[k]]), na.rm=TRUE)
}

meanoverallprecip <- mean(meanPRECIP)

plot(meanPRECIP, type= "b")

#work out the upper limits using quantile 
quantile(meanPRECIP, probs = c(0.2, 0.8))

#use function of "which" to select layers of specific criteria 

which(meanPRECIP>531.8702)
which(meanPRECIP<412.6557)

#create layers from upper and lower percentage 

rainfalldry[rainfalldry>meanprecip]<- NA 

rainfallwet[rainfallwet<meanprecip]<- NA

#generating a value for slope for ndvi vs rainfall in high rain and low rain years

plot(x= values(rainfall), y= values(ndvi)) 
lm1.lm <- lm(values(ndvi)~values(rainfall)) 
abline(lm1.lm, col=3)


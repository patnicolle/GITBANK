# finding years with anomolously wet and dry rainfall 
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
x <- as.vector(values(rainfall))
y <- as.vector(values(ndvi))
x2 <- as.vector(values(rainfalldry))
x3 <- as.vector(values(rainfallwet))
#regular 
pdf("regualrndvirainfall.pdf")
plot(x,y) 
lm1.lm <- lm(y~x) 
abline(lm1.lm, col=3)
dev.off()
#dry
pdf("dryndvirainfall.pdf")
plot(x2,y) 
lm2.lm <- lm(y~x2) 
abline(lm2.lm, col=2) 
dev.off()
#wet 
pdf("wetndvirainfall.pdf")
plot(x3,y) 
lm3.lm <- lm(y~x3) 
abline(lm3.lm, col=4)
dev.off()
plot(1, type="n", xlab="", ylab="", xlim=c(0, 5000), ylim=c(0, 1))
abline(lm1.lm, col=1)
abline(lm2.lm, col=2)
abline(lm3.lm, col=4)
segmented_lm <- segmented(lm1.lm, seg.Z = ~x, psi = 300)

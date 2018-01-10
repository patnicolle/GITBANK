
rm(list=ls(all=TRUE)) # removes all old variables

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF)

file.ndvi <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/",full.names = TRUE)
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/ANUCLIM_precip_bimonthly_1982_2014_GIMMS_resolution.nc"
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)
file.vod <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"
data <- brick(file.precip)

#--------------------------------------------

data2 <- brick(file.vod)
annual_VOD <- brick()
yearsV <- nlayers(data2)/12
for (k in 1: yearsV) {
  annualV <- mean(data2[[(k*12-11):(k*12)]])
  annual_VOD <- addLayer(annual_VOD, annualV)
}

Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}


#------------------------------------------------------

data <- Precipitation1
annual_precip <- brick()
years <- nlayers(data)/24
for (k in 1: years) {
  annual <- mean(data[[(k*24-23):(k*24)]])
  annual_precip <- addLayer(annual_precip, annual)
}

anprec <- annual_precip[[12:31]]

#-----------------------------------------------------
vOd <- annual_VOD
rainfall <- anprec

vod <- resample(vOd, rainfall)

vod <- crop(vod, rainfall)

vod <- mask(vod, rainfall)

#nlayers, how many layers in the file

#creating vectors of mean values per layer

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
y <- as.vector(values(vod))
x2 <- as.vector(values(rainfalldry))
x3 <- as.vector(values(rainfallwet))
#regular 
pdf("regualrvodrainfall.pdf")
plot(x,y) 
lm1.lm <- lm(y~x) 
abline(lm1.lm, col=3)
dev.off()
#dry
pdf("dryndvivodfall.pdf")
plot(x2,y) 
lm2.lm <- lm(y~x2) 
abline(lm2.lm, col=2) 
dev.off()
#wet 
pdf("wetvodrainfall.pdf")
plot(x3,y) 
lm3.lm <- lm(y~x3) 
abline(lm3.lm, col=4)
dev.off()
plot(1, type="n", xlab="", ylab="", xlim=c(0, 5000), ylim=c(0, 1))
abline(lm1.lm, col=1)
abline(lm2.lm, col=2)
abline(lm3.lm, col=4)
pdf("aaa.pdf")
plot(x,y)
abline(lm1.lm, col=1)
abline(lm2.lm, col=2)
abline(lm3.lm, col=4)
dev.off()

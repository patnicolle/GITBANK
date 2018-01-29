rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 

file <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file2 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/VOD/ANUCLIM_precip_monthly_1993_2012_VOD_resolution.nc"
data2 <- brick(file2)
data2 <- data2/716 #work out to scale down to vod
data <- brick(file)

e <- extent(127,135,-28,-25)

attempt <- data
attempt <- crop(attempt,e)

precipattempt <- data2 
precipattempt<- crop(precipattempt,e)

meanVOD <- vector(length = nlayers(attempt))
for(k in 1: nlayers(attempt)) {
  meanVOD[k] <- mean(values(attempt[[k]]), na.rm=TRUE)
}

meanprecip <- vector(length = nlayers(precipattempt))
for(k in 1: nlayers(precipattempt)) {
  meanprecip[k] <- mean(values(precipattempt[[k]]), na.rm=TRUE)
}

a <- (meanprecip*2)+0.1
x1 <-seq(from= 1, to= nlayers(data), by=1)


smoothingSpline = smooth.spline(x1, meanVOD, spar=0.05)
smoothingSpline2 = smooth.spline(x1, a, spar=0.1)


plot(x1,meanVOD, col="white", ylim=c(0,0.5), main="(127,135,-28,-25)")
lines(smoothingSpline, col=3)
lines(smoothingSpline2, col="blue")

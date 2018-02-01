#rainfall reponse at specific lat/long aswell as mask area specific

rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 

file <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file2 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/VOD/ANUCLIM_precip_monthly_1993_2012_VOD_resolution.nc"
file3 <-"/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/VOD/VOD_annual_integrated_values_1993_2012_ANUCLIM_precipitation_detrended_extreme_dry_years.nc"
file4 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/VOD/VOD_annual_integrated_values_1993_2012_ANUCLIM_precipitation_detrended_dry_years.nc"
file5 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/VOD/VOD_annual_integrated_values_1993_2012_ANUCLIM_precipitation_detrended_normal_years.nc"
file6 <-"/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/VOD/VOD_annual_integrated_values_1993_2012_ANUCLIM_precipitation_detrended_wet_years.nc"
file7 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/VOD/VOD_annual_integrated_values_1993_2012_ANUCLIM_precipitation_detrended_extreme_wet_years.nc"

file8 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Vegetation_indices/VOD/VOD_annual_integrated_values_1993_2012_masked_gapfilled_detrended.nc"
#1. Mask area specific
prec <- brick(file2)
vod <- brick(file8)

vod[is.na(prec)] <- NA

p<-as.vector(prec)
v<-as.vector(vod)

pdf("A_NEW_PLOT/starightVOD~RAINFALL~correlation/extremedry~VOD.pdf")
plot(p,v)
lm1<-lm(v~p)
abline(lm1, col=3)
legend("topright", bty="n", legend=paste("R2 is",format(summary(lm1)$adj.r.squared, digits=4)))
dev.off

#______________________________________________________________________________

#2. Lat Long specific
data2 <- brick(file2)
data2 <- data2/716 #work out to scale down to vod
data <- brick(file)

e <- extent(131,133.6,-19.9,-17.4)



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

a <- (meanprecip/2)+0.15
x1 <-seq(from= 1, to= nlayers(data), by=1)


smoothingSpline = smooth.spline(x1, meanVOD, spar=0.05)
smoothingSpline2 = smooth.spline(x1, a, spar=0.1)


plot(x1,meanVOD, col="white", ylim=c(0,0.5), main="(131,133.6,-19.9,-17.4)")
lines(smoothingSpline, col=3)
lines(smoothingSpline2, col="blue")


x2<-seq(from= 1, to= length(vod), by=1)

p<-as.vector(prec)
v<-as.vector(vod)


plot(p,v)
lm1<-lm(v~p)
abline(lm1, col=3)
summary(lm1)$r.squared







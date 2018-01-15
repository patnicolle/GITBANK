#DELTA VOD

rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)

file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/VOD/ANUCLIM_precip_monthly_1970_2014_VOD_resolution_fixed.nc"
file.vod <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc"
data <- brick(file.precip) #or wetprecip/aridprecip
#data2 <- brick(file.ndvi) #or wetav/aridav

data2 <- brick(file.vod)
#data <- aridprecip
#data2 <- aridav
P <- data 
P <- P[[276:513]]
deltaVOD <- brick()
for (k in 1:(nlayers(data2)-2)) {
  r <- mean(data2[[k+1]], data2[[k+2]])- data2[[k]]
  deltaVOD <- addLayer(deltaVOD, r)
}
#abackup <- deltaNDVI
P[P<50] <- NA
deltaVOD[deltaVOD<0] <- NA 
P[is.na(deltaVOD)] <- NA
deltaVOD[is.na(P)] <- NA
#plot first few layers 
#plot(P[[1]], deltaNDVI[[1]])

x <- P
y <- deltaVOD
x2 <- values(x)
y2 <- values(y)

ind <- which(is.na(y2))
y2 <- y2[-ind]
x2 <- x2[-ind] 

lmtotal.lm <- lm(y2~x2)
summary(lmtotal.lm)$r.squared

png("aaadeltaVOD_precip_dry_aridity.png") #check which input data is used before running
plot(x2,y2, cex=0.5)
abline(lmtotal.lm, col=2)
dev.off()











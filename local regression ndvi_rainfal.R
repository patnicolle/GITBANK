# is small rainfall events linked to small delta ndvi 
# is large rainfall events linked to large delta ndvi  

rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/NDVI/ANUCLIM_precipitation_1982_2008_NDVI_resolution.nc"
file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc"
file.precipa <- list.files("/Volumes/P_Harddrive/Annual_data/Precipitation/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)
file.vod <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"

data <- brick(file.precip) #or wetprecip/aridprecip
data2 <- brick(file.ndvi) #or wetav/aridav
data2<- data2[[1:650]]

cropbox <-c(141,149,-30,-37)
newrain <- crop(data, cropbox)
newndvi <- crop(data2, cropbox)


dNDVI <- brick()
for (k in 1:(nlayers(newndvi)-2)) {
  r <- mean(newndvi[[k+1]], newndvi[[k+2]])- newndvi[[k]]
  dNDVI <- addLayer(dNDVI, r)
}
#recrop rainfall

dNDVI[dNDVI>0] <- NA
x <- as.vector(newrain)
y <- as.vector(dNDVI)
lm1.lm <- lm(y~x)
summary(lm1.lm)$r.squared

#data <- aridprecip
#data2 <- aridav
P <- data 
P <- P[[1:790]]
deltaNDVI <- brick()
for (k in 1:(nlayers(data2)-2)) {
  r <- mean(data2[[k+1]], data2[[k+2]])- data2[[k]]
  deltaNDVI <- addLayer(deltaNDVI, r)
}
abackup <- deltaNDVI
P[P<0 & P>50] <- NA
deltaNDVI[deltaNDVI<0 & deltaNDVI>0.1] <- NA 
P[is.na(deltaNDVI)] <- NA
deltaNDVI[is.na(P)] <- NA
#plot first few layers 
#plot(P[[1]], deltaNDVI[[1]])

x <- P[[1:790]]
y <- deltaNDVI[[1:790]]
x2 <- values(x)
y2 <- values(y)

ind <- which(is.na(y2))
y2 <- y2[-ind]
x2 <- x2[-ind] 

lmtotal.lm <- lm(y2~x2)
summary(lmtotal.lm)$r.squared

png("aaadeltaNDVI_precip_dry_aridity.png") #check which input data is used before running
plot(x2,y2, cex=0.5)
abline(lmtotal.lm, col=2)
dev.off()


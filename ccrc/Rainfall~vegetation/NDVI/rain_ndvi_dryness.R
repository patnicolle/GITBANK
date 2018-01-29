# does a period of drought correspond to a drop in ndvi 
# how long must that period be if so to be significant 

rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)
library(stats)
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/NDVI/ANUCLIM_precipitation_1982_2008_NDVI_resolution.nc"
file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc"

data <- brick(file.precip)
data2 <- brick(file.ndvi)
cropndvi<- data2[[1:650]] 

dNDVI <- brick()
for (k in 1:(nlayers(cropndvi)-2)) {
  r <- mean(cropndvi[[k+1]], cropndvi[[k+2]])- cropndvi[[k]]
  dNDVI <- addLayer(dNDVI, r)
}

negdNDVI <-dNDVI
negdNDVI[negdNDVI>0] <- NA 

lowrain <- data
 # change the limit of "low" rain 

#mask each set with those values that are missing to speed code 

lowrain[is.na(negdNDVI)] <- NA
negdNDVI[is.na(lowrain)] <- NA

x <- as.vector(lowrain)
y <- as.vector(negdNDVI)

ind <- which(is.na(y))
y <- y[-ind]
x <- x[-ind]

lmsah.lm <- glm(y~x)


summary(lmsah.lm)$r.squared

# at some point try to plot a polynomial regression 
#fit <- lm(y ~ x + I(x^2))





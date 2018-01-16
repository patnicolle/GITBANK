#VOD~Rainfall_slope(lm)

rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")


library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented) 
library(stats)

source("~/Desktop/newscripts/slope_per_pixel_function.R")

file.vod <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/VOD/ANUCLIM_precip_monthly_1993_2012_VOD_resolution.nc"


vod <- brick(file.vod) 
prec <-brick(file.precip)


# generate delta VOD 

deltaVOD <- brick()
for (k in 1:(nlayers(vod)-2)) {
  r <- mean(vod[[k+1]], vod[[k+2]])- vod[[k]]
  deltaVOD <- addLayer(deltaVOD, r)
} 


# match layers of x and y variable (e.g. rainfall and VOD)
prec <- prec[[1:238]]


x_data <- prec

y_data <- deltaVOD  

#double check x and y have same amount of layers 

combine_data <- x_data
combine_data <- addLayer(combine_data, y_data)



slope_pixel <- calc(combine_data, fun=slope_per_pixel)









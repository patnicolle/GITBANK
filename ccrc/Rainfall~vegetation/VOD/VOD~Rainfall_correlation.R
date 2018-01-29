rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")


library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented) 
library(stats)

source("~/Desktop/scripts/cor_per_pixel_function.R")

file.vod <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/VOD/ANUCLIM_precip_monthly_1993_2012_VOD_resolution.nc"

#generate a brick each 

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



#match variables to common code below 
x_data <- prec

y_data <- deltaVOD  

#double check x and y have same amount of layers 

combine_data <- x_data
combine_data <- addLayer(combine_data, y_data)


# run the predefined function "cor per pixel" as defined in the sourcing/library section
# calculate( using x, with function = cor_per_pixel)
correlation_pixel <- calc(combine_data, fun=cor_per_pixel)

#cor per pixel outputs [[1]]=correlation coefficient [[2]]=p-value 

plot(correlation_pixel[[1]])
plot(correlation_pixel[[2]])


pdf("vod_correlation[[1]].pdf") 
plot(correlation_pixel[[1]])
dev.off()

pdf("vod_correlation[[2]].pdf")
plot(correlation_pixel[[2]])
dev.off()










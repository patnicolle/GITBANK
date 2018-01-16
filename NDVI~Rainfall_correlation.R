rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")


library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented) 
library(stats)

source("~/Desktop/newscripts/cor_per_pixel_function.R")

file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/ANUCLIM_precipitation_1982_2008_NDVI_resolution.nc"

#generate a brick each 

ndvi <- brick(file.ndvi) 
prec <-brick(file.precip)

# generate delta VOD 

deltaNDVI <- brick()
for (k in 1:(nlayers(ndvi)-2)) {
  r <- mean(ndvi[[k+1]], ndvi[[k+2]])- ndvi[[k]]
  deltaNDVI <- addLayer(deltaNDVI, r)
} 


# match layers of x and y variable (e.g. rainfall and VOD)
dndvi <- deltaNDVI[[1:648]]



#match variables to common code below 
x_data <- prec

y_data <- dndvi  

#double check x and y have same amount of layers 

combine_data <- x_data
combine_data <- addLayer(combine_data, y_data)


# run the predefined function "cor per pixel" as defined in the sourcing/library section
# calculate( using x, with function = cor_per_pixel)
corr_pixel <- calc(combine_data, fun=cor_per_pixel)

#cor per pixel outputs [[1]]=correlation coefficient [[2]]=p-value 
plot(corr_pixel[[1]])

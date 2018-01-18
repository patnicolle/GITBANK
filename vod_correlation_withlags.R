#vod_corr_lags 

rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")


library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented) 
library(stats)

source("~/Desktop/scripts/cor_lagged_function.R")

file.vod <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/VOD/ANUCLIM_precip_monthly_1993_2012_VOD_resolution.nc"

#generate a brick each 

vod <- brick(file.vod) 
prec <-brick(file.precip)

# generate delta VOD 

deltaVOD <- brick()
for (k in 1:(nlayers(vod)-1)) {
  r <- vod[[k+1]]- vod[[k]]
  deltaVOD <- addLayer(deltaVOD, r)
} 


# match layers of x and y variable (e.g. rainfall and VOD)
prec <- prec[[1:nlayers(deltaVOD)]]



#match variables to common code below 
x_data <- prec

y_data <- deltaVOD  

#double check x and y have same amount of layers 

combine_data <- x_data
combine_data <- addLayer(combine_data, y_data)


lagcorrelation <- calc(combine_data, fun = cor_lagged)


outputfile <- "vodlagcorrelation.nc"

writeRaster(x=lagcorrelation, filename=outputfile, varname="correlation", 
            longname="Linear regression of lagged precip on VOD", overwrite=TRUE)


pdf("vodlagcorrelation[[1]].pdf")
plot(lagcorrelation[[1]])
dev.off()

pdf("vodlagcorrelation[[2]].pdf") 
plot(lagcorrelation[[2]])
dev.off()







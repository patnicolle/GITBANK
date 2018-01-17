#EVI

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

file.evi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/EVI/Australia_EVI_16day_composite_8km_2001_2014.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/EVI/ANUCLIM_precip_bimonthly_2001_2014_EVI_resolution.nc"

evi <- brick(file.evi)
precip <- brick(file.precip)

#evi is 2001:2015 

deltaEVI <- brick()
for (k in 1:(nlayers(evi)-2)) {
  r <- mean(evi[[k+1]], evi[[k+2]])- evi[[k]]
  deltaEVI <- addLayer(deltaEVI, r)
} 

#match layers of EVI to precip
deltaEVI <- deltaEVI[[1:322]] 

x_data <- precip

y_data <- deltaEVI  

#double check x and y have same amount of layers 

combine_data <- x_data
combine_data <- addLayer(combine_data, y_data)

lagcorrelation <- calc(combine_data, fun = cor_lagged)


outputfile <- "evilagcorrelation.nc"

writeRaster(x=lagcorrelation, filename=outputfile, varname="correlation", 
            longname="Linear regression of lagged precip on NDVI", overwrite=TRUE)

# read in the nc file
lagcor.file <- "/Volumes/P_Harddrive/evilagcorrelation.nc"
lagcorrelation <- brick(lagcor.file)

pdf("evilagcorrelation[[1]].pdf")
plot(lagcorrelation[[1]])
dev.off()

pdf("evilagcorrelation[[2]].pdf") 
plot(lagcorrelation[[2]])
dev.off()












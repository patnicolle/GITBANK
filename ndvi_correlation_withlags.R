#ndvi_correlation_withlags
#must redo ndvi file as layer 427 is bad




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

file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/"
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

lagcorrelation <- calc(combine_data, fun = cor_lagged)


outputfile <- "newndvilagcorrelation.nc"

writeRaster(x=lagcorrelation, filename=outputfile, varname="correlation", 
            longname="Linear regression of lagged precip on NDVI", overwrite=TRUE)

# read in the nc file
lagcor.file <- "/Volumes/P_Harddrive/newndvilagcorrelation.nc"
lagcorrelation <- brick(lagcor.file)


breaks <- c(-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5)
cols <- colorRampPalette(c("darkblue", "dodgerblue3", "darkslategray1",  "orange", "red", "darkred"))
legendbreaks <- breaks+0.5


pdf("ndvilagcorrelation[[1]].pdf")
plot(lagcorrelation[[1]])
dev.off()

pdf("ndvilagcorrelation[[2]].pdf") 
plot(lagcorrelation[[2]])
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols(length(legendbreaks)), bty="n")
dev.off()



















































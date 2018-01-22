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
source("~/Desktop/scripts/add_raster_legend.R")

file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/MONTHLYNDVI.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/MONTHLYPRECIPNDVI.nc"

#generate a brick each 

ndvi <- brick(file.ndvi) 
prec <-brick(file.precip)

# generate delta VOD 

deltaNDVI <- brick()
for (k in 1:(nlayers(ndvi)-1)) {
  r <- (ndvi[[k+1]]-ndvi[[k]])
  deltaNDVI <- addLayer(deltaNDVI, r)
} 

# match layers of x and y variable (e.g. rainfall and VOD)
deltaNDVI <- deltaNDVI[[1:nlayers(prec)]]
dndvi <- deltaNDVI

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


breaks <- c(0,1,2,3,4,5,6,7)
cols <- colorRampPalette(c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf", "#1c9099", "#016c59"))
cols2 <- colorRampPalette(c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc"))
cols1 <- colorRampPalette(c("darkblue", "dodgerblue3", "darkslategray1",  "orange", "red", "darkred"))
legendbreaks <- breaks
breaks2 <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

pdf("lag_correlation/ndvilagcorrelation[[1]].pdf")
plot(lagcorrelation[[1]], col=cols1(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols1(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/ndvilagcorrelation[[2]].pdf") 
plot(lagcorrelation[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/ndvilagcorrelation[[3]].pdf") 
plot(lagcorrelation[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()






























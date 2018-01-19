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
source("~/Desktop/scripts/add_raster_legend.R")
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


breaks <- c(-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5)
cols <- colorRampPalette(c("darkblue", "dodgerblue3", "darkslategray1",  "orange", "red", "darkred"))
legendbreaks <- breaks+0.5
breaks2 <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)


pdf("lag_correlation/vodlagcorrelation[[1]].pdf")
plot(lagcorrelation[[1]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/vodlagcorrelation[[2]].pdf") 
plot(lagcorrelation[[2]], col=cols(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols(length(legendbreaks)), bty="n")
dev.off()







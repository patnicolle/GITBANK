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
source("~/Desktop/scripts/add_raster_legend.R")
file.evi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/EVI/MONTHLYEVI.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/EVI/MONTHLYPRECIPEVI.nc"
evi <- brick(file.evi)
precip <- brick(file.precip)


deltaEVI <- brick()
for (k in 1:(nlayers(evi)-1)) {
  r <- evi[[k+1]] - evi[[k]]
  deltaEVI <- addLayer(deltaEVI, r)
} 

#match layers of EVI to precip
deltaEVI <- deltaEVI[[1:167]] 
precip <- precip[[1:nlayers(deltaEVI)]]

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

breaks <- c(-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5)
breaks2 <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
cols <- colorRampPalette(c("darkblue", "dodgerblue3", "darkslategray1",  "orange", "red", "darkred"))
legendbreaks <- breaks+0.5
a<-colorRampPalette(c("brown4","brown1","coral1","yellow","springgreen","royalblue"))
lats <- c(-43.83333,-5)
pdf("lag_correlation/evilagcorrelation[[1]].pdf")
plot(lagcorrelation[[1]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)
                   
dev.off()

pdf("lag_correlation/evilagcorrelation[[2]].pdf") 
plot(lagcorrelation[[2]], col=cols(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols(length(legendbreaks)), bty="n")
dev.off()

#add_raster_legend2(cols=a(length(breaks)-1), limits=breaks[2:(length(breaks)-1)], spt.cex=1, 
                   #main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)









rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented) 
library(stats)
library(graphics)
file.evi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/EVI/MONTHLYEVI.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/EVI/MONTHLYPRECIPEVI.nc"

evi <- brick(file.evi) 
prec <-brick(file.precip)

annual_prec <- brick()
years <- nlayers(prec)/12
for (k in 1: years) {
  annual <- sum(prec[[(k*12-11):(k*12)]])
  annual_prec <- addLayer(annual_prec, annual)
}
meanprecip <- mean(annual_prec)
source("~/Desktop/scripts/ccrc/functions/add_raster_legend.R")
source("~/Desktop/scripts/ccrc/functions/cor_function_perc~dir.R")
#----------------------------------------------------------------------------------------------

#create a function with monPrec+monNdvi

x_data <- prec

y_data <- evi  

#double check x and y have same amount of layers 

combine_data <- x_data
combine_data <- addLayer(combine_data, y_data)
#----------------------------------------------------------------------------------------------

lagsevinormal <- calc(combine_data, fun=cor_lagged)

lagseviwet<- calc(combine_data, fun=function(x) cor_lagged(x, perc=0.7, dir= "above"))

lagsevidry<- calc(combine_data, fun=function(x) cor_lagged(x, perc=0.3, dir= "below"))

#----------------------------------------------------------------------------------------------
breaks <- c(0,1,2,3,4,5,6,7)
cols <- colorRampPalette(c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf", "#1c9099", "#016c59"))
cols2 <- colorRampPalette(c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc"))
cols1 <- colorRampPalette(c("darkblue", "dodgerblue3", "darkslategray1",  "orange", "red", "darkred"))
legendbreaks <- breaks
breaks2 <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
#----------------------------------------------------------------------------------------------
#WET
pdf("lag_correlation/evi/eviwetlagcorrelation[[1]].pdf")
plot(lagseviwet[[1]], col=cols2(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols2(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/evi/eviwetlagcorrelation[[2]].pdf") 
plot(lagseviwet[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/evi/eviwetlagcorrelation[[3]].pdf") 
plot(lagseviwet[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()

#----------------------------------------------------------------------------------------------
#DRY
pdf("lag_correlation/evi/evidrylagcorrelation[[1]].pdf")
plot(lagsevidry[[1]], col=cols2(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols2(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/evi/evidrylagcorrelation[[2]].pdf") 
plot(lagsevidry[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/evi/evidrylagcorrelation[[3]].pdf") 
plot(lagsevidry[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()

#----------------------------------------------------------------------------------------------
#NORMAL
pdf("lag_correlation/evi/evinormallagcorrelation[[1]].pdf")
plot(lagsevinormal[[1]], col=cols2(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols2(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/evi/evinormallagcorrelation[[2]].pdf") 
plot(lagsevinormal[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/evi/evinormallagcorrelation[[3]].pdf") 
plot(lagsevinormal[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()
#-----------------------------------------------------------------------------------------------
pdf("lag_correlation/multilags/evilags.pdf")
par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
plot(lagsevidry[[3]],col=cols2(length(breaks)-1), breaks=breaks, main="Dry NDVI Lag")
plot(lagsevinormal[[3]],col=cols2(length(breaks)-1), breaks=breaks, main="Normal NDVI Lag") 
plot(lagseviwet[[3]],col=cols2(length(breaks)-1), breaks=breaks, main="Wet NDVI Lag")
dev.off()



par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
plot(lagsndvinormal[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE, main= "Normal")
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n", cex = 0.61)

plot(lagsndvidry[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE, main= "Dry")
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n", cex = 0.61)

plot(lagsndviwet[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE, main= "Wet")
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n", cex = 0.61)

#----------------------------------------------------------------------------------------------

pdf("lag_correlation/multilags/evilags.pdf")
par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
plot(lagsevidry[[3]], col=cols2(length(breaks)-1), breaks=breaks,main="Dry EVI Lag")
plot(lagsevinormal[[3]], col=cols2(length(breaks)-1), breaks=breaks,main="Normal EVI Lag") 
plot(lagseviwet[[3]],col=cols2(length(breaks)-1), breaks=breaks, main="Wet EVI Lag")
dev.off()


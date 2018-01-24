#test

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
file.vod <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/VOD/ANUCLIM_precip_monthly_1993_2012_VOD_resolution.nc"

vod <- brick(file.vod) 
prec <-brick(file.precip)


source("~/Desktop/scripts/add_raster_legend.R")
source("~/Desktop/scripts/cor_function_perc~dir.R")
#----------------------------------------------------------------------------------------------

#create a function with monPrec+monNdvi

x_data <- prec

y_data <- vod  

#double check x and y have same amount of layers 

combine_data <- x_data
combine_data <- addLayer(combine_data, y_data)
#----------------------------------------------------------------------------------------------

lagsvodnormal <- calc(combine_data, fun=cor_lagged)

lagsvodwet<- calc(combine_data, fun=function(x) cor_lagged(x, perc=0.7, dir= "above"))

lagsvoddry<- calc(combine_data, fun=function(x) cor_lagged(x, perc=0.3, dir= "below"))

#----------------------------------------------------------------------------------------------

breaks <- c(0,1,2,3,4,5,6,7)
cols <- colorRampPalette(c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf", "#1c9099", "#016c59"))
cols2 <- colorRampPalette(c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc"))
cols1 <- colorRampPalette(c("darkblue", "dodgerblue3", "darkslategray1",  "orange", "red", "darkred"))
legendbreaks <- breaks
breaks2 <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
#----------------------------------------------------------------------------------------------
#WET
pdf("lag_correlation/ndviwetlagcorrelation[[1]].pdf")
plot(lagsndviwet[[1]], col=cols1(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols1(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/ndviwetlagcorrelation[[2]].pdf") 
plot(lagsndviwet[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/ndviwetlagcorrelation[[3]].pdf") 
plot(lagsndviwet[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()

#----------------------------------------------------------------------------------------------
#DRY
pdf("lag_correlation/ndvidrylagcorrelation[[1]].pdf")
plot(lagsndvidry[[1]], col=cols1(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols1(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/ndvidrylagcorrelation[[2]].pdf") 
plot(lagsndvidry[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/ndvidrylagcorrelation[[3]].pdf") 
plot(lagsndvidry[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()

#----------------------------------------------------------------------------------------------
#NORMAL
pdf("lag_correlation/ndvinormallagcorrelation[[1]].pdf")
plot(lagsndvinormal[[1]], col=cols1(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols1(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/ndvinormallagcorrelation[[2]].pdf") 
plot(lagsndvinormal[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/ndvinormallagcorrelation[[3]].pdf") 
plot(lagsndvinormal[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()



par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
plot(lagsvoddry[[3]], main="Dry")
plot(lagsvodnormal[[3]], main="Normal") 
plot(lagsvodwet[[3]], main="Wet")







par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
plot(lagsvodnormal[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE, main= "Normal")
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n", cex = 0.61)

plot(lagsvoddry[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE, main= "Dry")
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n", cex = 0.61)

plot(lagsvodwet[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE, main= "Wet")
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n", cex = 0.61)



cellStats(lagsvoddry[[3]], stat='mean', na.rm=TRUE)

cellStats(lagsvodwet[[3]], stat='mean', na.rm=TRUE)

cellStats(lagsvodnormal[[3]], stat='mean', na.rm=TRUE)




a <- lagsvoddry[[3]]
a[lagsvoddry[[2]]>0.1] <- NA


b <- lagsvodwet[[3]]
b[lagsvodwet[[2]]>0.1] <- NA

c <- lagsvodnormal[[3]]
c[lagsvodnormal[[2]]>0.1] <- NA

par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
plot(a)
plot(b)
plot(c)

cellStats(a, stat='mean', na.rm=TRUE)
cellStats(b, stat='mean', na.rm=TRUE)
cellStats(c, stat='mean', na.rm=TRUE)


f <-b-a
plot(f, main="wet-dry")
g <- a-c
plot(g, main="dry-normal")
h <- b-c 
plot(h, main="wet-normal")



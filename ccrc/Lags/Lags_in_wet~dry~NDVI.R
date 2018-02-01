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

file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/MONTHLYNDVI~1982~2015.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/MONTHLYPRECIPNDVI~1982~2008.nc"

ndvi <- brick(file.ndvi) 
prec <-brick(file.precip)
ndvi <- ndvi[[1:324]]

source("~/Desktop/scripts/ccrc/functions/add_raster_legend.R")
source("~/Desktop/scripts/ccrc/functions/cor_function_perc~dir.R")
#----------------------------------------------------------------------------------------------
annual_prec <- brick()
years <- nlayers(prec)/12
for (k in 1: years) {
  annual <- sum(prec[[(k*12-11):(k*12)]])
  annual_prec <- addLayer(annual_prec, annual)
}
meanprecip <- mean(annual_prec)
#----------------------------------------------------------------------------------------------

#create a function with monPrec+monNdvi

x_data <- prec

y_data <- ndvi  

#double check x and y have same amount of layers 

combine_data <- x_data
combine_data <- addLayer(combine_data, y_data)
#----------------------------------------------------------------------------------------------

lagsndvinormal <- calc(combine_data, fun=cor_lagged)

lagsndviwet<- calc(combine_data, fun=function(x) cor_lagged(x, perc=0.7, dir= "above"))

lagsndvidry<- calc(combine_data, fun=function(x) cor_lagged(x, perc=0.3, dir= "below"))


#----------------------------------------------------------------------------------------------

breaks <- c(0,1,2,3,4,5,6,7)
cols <- colorRampPalette(c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf", "#1c9099", "#016c59"))
cols2 <- colorRampPalette(c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc"))
cols1 <- colorRampPalette(c("darkblue", "dodgerblue3", "darkslategray1",  "orange", "red", "darkred"))
legendbreaks <- breaks
breaks2 <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
#----------------------------------------------------------------------------------------------
#WET
pdf("lag_correlation/ndvi/ndviwetlagcorrelation[[1]].pdf")
plot(lagsndviwet[[1]], col=cols2(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols2(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/ndvi/ndviwetlagcorrelation[[2]].pdf") 
plot(lagsndviwet[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/ndvi/ndviwetlagcorrelation[[3]].pdf") 
plot(lagsndviwet[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()

#----------------------------------------------------------------------------------------------
#DRY
pdf("lag_correlation/ndvi/ndvidrylagcorrelation[[1]].pdf")
plot(lagsndvidry[[1]], col=cols2(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols2(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/ndvi/ndvidrylagcorrelation[[2]].pdf") 
plot(lagsndvidry[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/ndvi/ndvidrylagcorrelation[[3]].pdf") 
plot(lagsndvidry[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()

#----------------------------------------------------------------------------------------------
#NORMAL
pdf("lag_correlation/ndvi/ndvinormallagcorrelation[[1]].pdf")
plot(lagsndvinormal[[1]], col=cols2(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols2(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/ndvi/ndvinormallagcorrelation[[2]].pdf") 
plot(lagsndvinormal[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.05,0.07), xpd=NA)

dev.off()

pdf("lag_correlation/ndvi/ndvinormallagcorrelation[[3]].pdf") 
plot(lagsndvinormal[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()

pdf("lag_correlation/multilags/ndvilags.pdf")
par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
plot(lagsndvidry[[3]],col=cols2(length(breaks)-1), breaks=breaks, main="Dry NDVI Lag")
plot(lagsndvinormal[[3]],col=cols2(length(breaks)-1), breaks=breaks, main="Normal NDVI Lag") 
plot(lagsndviwet[[3]],col=cols2(length(breaks)-1), breaks=breaks, main="Wet NDVI Lag")
dev.off()



#par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
#plot(lagsndvinormal[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE, main= "Normal")
#legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n", cex = 0.61)

#legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n", cex = 0.61)
#plot(lagsndviwet[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE, main= "Wet")
#legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n", cex = 0.61)



#cellStats(lagsndvidry[[3]], stat='mean', na.rm=TRUE)

#cellStats(lagsndviwet[[3]], stat='mean', na.rm=TRUE)

#cellStats(lagsndvinormal[[3]], stat='mean', na.rm=TRUE)




#a <- lagsndvidry[[3]]
#a[lagsvoddry[[2]]>0.2] <- NA


#b <- lagsndviwet[[3]]
#b[lagsvodwet[[2]]>0.2] <- NA

#c <- lagsndvinormal[[3]]
#c[lagsvodnormal[[2]]>0.2] <- NA

#par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
#plot(b)
#plot(c)


#cellStats(a, stat='mean', na.rm=TRUE)
#cellStats(b, stat='mean', na.rm=TRUE)
#cellStats(c, stat='mean', na.rm=TRUE)


#f <-b-a
#plot(f, main="wet-dry")
#g <- a-c
#plot(g, main="dry-normal")
#h <- b-c 
#plot(h, main="wet-normal")


#----------------------------------------------------------------------------------------------

# LAG correlations 
#mask wet and dry
#a<-lagsndviwet[[3]]
#b<-lagsndvinormal[[3]]
#c<-lagsndvidry[[3]]
#----------------------------------------------------------------------------------------------

#meanprecip <- mean(annual_prec)
#meanprecip[is.na(a)] <- NA
#a[is.na(meanprecip)] <- NA

#a1<-as.vector(meanprecip)
#a2<-as.vector(a)

#z1<-cor.test(a1,a2)
#print(z1) 
#----------------------------------------------------------------------------------------------

#meanprecip <- mean(annual_prec)
#meanprecip[is.na(b)] <- NA
#b[is.na(meanprecip)] <- NA

#b1<-as.vector(meanprecip)
#b2<-as.vector(b)

#z2<-cor.test(b1,b2)
#print(z2) 
#----------------------------------------------------------------------------------------------

#meanprecip <- mean(annual_prec)
#meanprecip[is.na(c)] <- NA
#c[is.na(meanprecip)] <- NA

#c1<-as.vector(meanprecip)
#c2<-as.vector(c)

#z3<-cor.test(c1,c2)
#print(z3) 


#z1$estimate
#z2$estimate
#z3$estimate






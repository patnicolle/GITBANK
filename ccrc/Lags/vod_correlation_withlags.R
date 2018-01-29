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

# read in the nc file
lagcor.file <- "/Volumes/P_Harddrive/vodlagcorrelation.nc"
lagcorrelation <- brick(lagcor.file)

breaks <- c(0,1,2,3,4,5,6,7)
cols <- colorRampPalette(c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf", "#1c9099", "#016c59"))
cols2 <- colorRampPalette(c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc"))
cols1 <- colorRampPalette(c("darkblue", "dodgerblue3", "darkslategray1",  "orange", "red", "darkred"))
legendbreaks <- breaks
breaks2 <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

pdf("lag_correlation/vodlagcorrelation[[1]].pdf")
plot(lagcorrelation[[1]], col=cols1(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols1(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/vodlagcorrelation[[2]].pdf") 
plot(lagcorrelation[[2]], col=cols(length(breaks2)-1), breaks=breaks2, legend=FALSE)
add_raster_legend2(cols=cols(length(breaks2)-1), limits=breaks2[2:(length(breaks2)-1)], spt.cex=1, 
                   main_title= "lag (months)", plot_loc=c(0.1,0.9,0.01,0.04), xpd=NA)

dev.off()

pdf("lag_correlation/vodlagcorrelation[[3]].pdf") 
plot(lagcorrelation[[3]], col=cols2(length(breaks)-1), breaks=breaks, legend=FALSE)
legend("bottom", horiz=TRUE, legend=legendbreaks[1:(length(legendbreaks)-1)], fill=cols2(length(legendbreaks)), bty="n")
dev.off()

























#isolate 5 month response 
vodm <- vodm[[1:239]]
lagcor3 <- lagcorrelation[[3]]
#lagcor3[lagcor3<6]<- NA

vodmask <- deltaVOD
vodmask[lagcor3<5] <- NA
a<- seq(c(1:238, by=1))
meanvodmask <- vector(length = nlayers(vodmask))
for(k in 1: nlayers(vodmask)) {
  meanvodmask[k] <- mean(values(vodmask[[k]]), na.rm=TRUE)
}
months_from_jan_1993 <- a
mean_delta_VOD <- meanvodmask

plot(months_from_jan_1993,mean_delta_VOD, main="high lag dVOD over time",ylim=c(-0.1,0.1))
smoothingSpline = smooth.spline(a, meanvodmask, spar=0.35)
lines(smoothingSpline, col=2)




lagcor3 <- lagcorrelation[[3]]
#lagcor3[lagcor3<6]<- NA
vodmask2 <- deltaVOD
vodmask2[lagcor3>1] <- NA
a<- seq(c(1:239, by=1))
a1 <-seq(c(1:238, by=1))
meanvodmask2 <- vector(length = nlayers(vodmask2))
for(k in 1: nlayers(vodmask2)) {
  meanvodmask2[k] <- mean(values(vodmask2[[k]]), na.rm=TRUE)
}
months_from_jan_1993 <- a
mean_delta_VOD <- meanvodmask2
plot(months_from_jan_1993,mean_delta_VOD, main="low lag dVOD over time", ylim=c(-0.1,0.1))
smoothingSpline2 = smooth.spline(a, meanvodmask2, spar=0.35)
lines(smoothingSpline2, col=3)


#find a single pixel of lag = 6

lagcor3 <- lagcorrelation[[3]]
#lagcor3[lagcor3<6]<- NA
vodm <- vod
vodm[lagcor3<6] <- NA
mean <- mean(vodm, na.rm=TRUE)
which.min(mean)
coordinates(vodm)[15644]

plot(a,vodm[15644])
smoothingspline5 <- smooth.spline(a,vodm[15644])
lines(smoothingspline5, col=4)

plot(a1, prec[15644])
smoothingSpline10 <- smooth.spline(a, prec[15644])
lines(smoothingSpline10, col=2)



vod <- vod
meanvod <- vector(length = nlayers(vod))
for(k in 1: nlayers(vod)) {
  meanvod[k] <- mean(values(vod[[k]]), na.rm=TRUE)
}
plot(a, meanvod, col="white",ylim=c(0.15,1.1))
smoothingSpline33 <- smooth.spline(a, meanvod)
lines(smoothingSpline33, col=3)
lines(smoothingspline5, col=4)
lines(smoothingspline19, col=2)





which.max(lagcor3>5)

xyFromCell(lagcor3, 18685)

extract(lagcor3,SpatialPoints(cbind(146.125,-35.875)))

plot(a,vodm[18685], col="white", ylim=c(0,1))
smoothingspline6lag <- smooth.spline(a,vodm[18685])
lines(smoothingspline6lag, col=4)
lines(smoothingSpline33, col=3)

which.max(lagcor3<1)

xyFromCell(lagcor3, 6179)
plot(a1,vod[4949], col="white", ylim=c(0,1))
smoothline0lag <- smooth.spline(a1, vodm[4949])
lines(smoothline0lag, col="white")
lines(smoothingSpline33, col="white")
lines(smoothingspline6lag, col="white")
lines(smooth2, col=3) 
lines(smooth3, col=9)

plot(a1,precc[4949])
smooth2 <- smooth.spline(a1,precc[4929])
lines(smooth2, col=3)

plot(prec[4949], vodm[4949])

smooth3 <- smooth.spline(a1, precc[18685])


lm1.lm <- glm(as.vector(vodm[18685])~as.vector(prec[18685]))
summary(lm1.lm)

precc <- prec/1000

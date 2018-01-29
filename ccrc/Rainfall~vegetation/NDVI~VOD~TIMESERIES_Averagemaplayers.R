 
rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive")

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 


file <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc" 
file2 <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"

data <- brick(file)
data2 <- brick(file2)
 
#simple visulatistaion
breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

cols <- colorRampPalette(c("olivedrab1", "darkgreen")) 

plot(data[[1]], breaks=breaks, col=cols(length(breaks)-1)) 

plot(data2[[1]], breaks=breaks, col=cols(length(breaks)-1)) 

#nlayers, how many layers in the file
nlayers(data)

nlayers(data2)

#creating vectors of mean values per layer
meanndvi <- vector(length = nlayers(data))
for(k in 1: nlayers(data)) {
  meanndvi[k] <- mean(values(data[[k]]), na.rm=TRUE)
}
annual_ndvi <- brick()
years <- nlayers(data)/24
for (k in 1: years) {
  annual <- mean(data[[(k*24-23):(k*24)]])
  annual_ndvi <- addLayer(annual_ndvi, annual)
}

mean(annual_ndvi)
meanmeanndvi<- mean(meanndvi)
anomndvi <-meanndvi- meanmeanndvi

plot(anomndvi, type= "l")
abline(h=0, col=4)
#---------------------------------------------------------------------------------------
meanVOD <- vector(length = nlayers(data2))
for(k in 1: nlayers(data2)) {
  meanVOD[k] <- mean(values(data2[[k]]), na.rm=TRUE)
}

y1 <- (meanndvi[1:nlayers(data)]) 
y2 <- (meanVOD[1:nlayers(data2)]) 
#create sequnce for each dataset
x1 <-seq(from= 1, to= nlayers(data), by=1)  
x2 <-seq(from= 337, to= nlayers(data), by =2)

months_from_1982 <- x1
ndvi_vod_values <- y1


#create lines of fit, then plot (x1,y1), then fit (x2,y2) as lines.
smoothingSpline = smooth.spline(x1, y1, spar=0.35)
smoothingSpline2 = smooth.spline(x2, y2, spar=0.35)
pdf("A_NEW_PLOT/NDVI~VOD~time_series.pdf")
plot(months_from_1982,ndvi_vod_values, type="l", col="white", main= NDVI&VOD~1982~2015, ylim= c(0.2,0.45)) 
lines(smoothingSpline, col="red")
lines(smoothingSpline2, col=6)
dev.off()





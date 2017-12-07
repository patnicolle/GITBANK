
library(ncdf4)
library(raster) 
library(SPAr)
file <- "/Volumes/P_Harddrive/GIMMS3g_NDVI_Australia_1982_2015/Australia_NDVI3g_bimonthly_1982_2015.nc" 
file2 <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"

data <- brick(file)/10000 
data2 <- brick(file2) 

breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
cols <- colorRampPalette(c("olivedrab1", "darkgreen")) 

plot(data[[1]], breaks=breaks, col=cols(length(breaks)-1))


#crop data
dim(data)
# work out the extent 
data
#=112.9167, 154, -43.83333, -9  (xmin, xmax, ymin, ymax) 

cropdata <- crop(data, extent(c(149, 152, -33, -31)))
cropdata2 <- crop(data, extent(c(140,145,-33,-30)))
#31°52'21.2"S 151°17'46.7"E - omadale

plot(cropdata[[1]], breaks=breaks, col=cols(length(breaks)-1)) 

plot(cropdata2[[696]], breaks=breaks, col=cols(length(breaks)-1), main=2011) #2010
plot(cropdata2[[576]], breaks=breaks, col=cols(length(breaks)-1), main=2006) #2006
#creating vectors of mean values per layer
meancrop2ndvi <- vector(length = nlayers(cropdata2))

for(k in 1: nlayers(cropdata2)) {
  meancrop2ndvi[k] <- mean(values(cropdata2[[k]]), na.rm=TRUE)
}

y1 <- (meancrop2ndvi[1:nlayers(cropdata2)]) 
#generate sequence for ndvi
x1 <-seq(from= 1, to= nlayers(cropdata2), by=1)   
plot(x1, y1, type="l", col="red", main= "Interior_NDVI(140,145,-33,-30)") 
smoothingSpline = smooth.spline(x1, y1, spar=0.7)
lines(smoothingSpline, col="aquamarine1", lwd=3)
0.3148508




#NOTE MILLENNIAL DROUGHT STARTED IN 1996 (336) WITH LOW RAINFALL AND ENDED IN 2010 (672) WITH HIGH RIANFALL







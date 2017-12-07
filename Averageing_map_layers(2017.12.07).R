 
library(ncdf4)
library(raster) 
library(SPAr)
file <- "/Volumes/P_Harddrive/GIMMS3g_NDVI_Australia_1982_2015/Australia_NDVI3g_bimonthly_1982_2015.nc" 
file2 <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"

data <- brick(file)/10000 
data2 <- brick(file2)

data 

data2 
#simple visulatistaion
breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

cols <- colorRampPalette(c("olivedrab1", "darkgreen")) 

plot(data[[1]], breaks=breaks, col=cols(length(breaks)-1)) 

plot(data2[[1]], breaks=breaks, col=cols(length(breaks2)-1)) 

#nlayers, how many layers in the file
nlayers(data)

nlayers(data2)

#creating vectors of mean values per layer
meanndvi <- vector(length = nlayers(data))

for(k in 1: nlayers(data)) {
  meanndvi[k] <- mean(values(data[[k]]), na.rm=TRUE)
}


meanVOD <- vector(length = nlayers(data2))

for(k in 1: nlayers(data2)) {
  meanVOD[k] <- mean(values(data2[[k]]), na.rm=TRUE)
}

y1 <- (meanndvi[1:nlayers(data)]) 
y2 <- (meanVOD[1:240]) 
#create sequnce for each dataset
x1 <-seq(from= 1, to= nlayers(data), by=1)  
x2 <-seq(from= 337, to= nlayers(data), by =2)

#create lines of fit, then plot (x1,y1), then fit (x2,y2) as lines.
smoothingSpline = smooth.spline(x1, y1, spar=0.35)
smoothingSpline2 = smooth.spline(x2, y2, spar=0.35)
plot(x1, y1, type="l", col="white", main= NDVI~VOD~1982~2015) 
lines(smoothingSpline, col=2)
lines(x2, y2, col="white")
lines(smoothingSpline2, col=4)

#lm for each line
lm(y1~x1)
abline(3.041e-01, 2.465e-05, col=2)

lm(y2~x2)
abline(2.511e-01, 7.634e-05, col=4)

y3 <- 0.3148508

smoothingSpline = smooth.spline(x1, y1, spar=0.35)
plot(x1,y1, type="l", col=4)
lines(smoothingSpline, col=6)
abline(h=0.3, col=2)

#plot NDVI- mean(NDVI) to show anaomlaies of grpahd
anom <- data- mean_ndvi 

mean_overall <- mean(data) 

mean_overall
mean(mean_overall)
cellStats(mean_overall, mean)

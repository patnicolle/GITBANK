#NEW NDVI, EVI, VOD 
rm(list=ls(all=TRUE)) # removes all old variables

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(zoo)

file <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc"
file2 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file3 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/EVI/Australia_EVI_16day_composite_8km_2001_2014.nc"

data <- brick(file)
data2 <- brick(file2)
data3 <- brick(file3)



#simple visulatistaion
breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

cols <- colorRampPalette(c("olivedrab1", "darkgreen")) 

#nlayers, how many layers in the file
nlayers(data)

nlayers(data2)

nlayers(data3)

#creating vectors of mean values per layer 

               #NDVI
meanNDVI <- vector(length = nlayers(data))

for(k in 1: nlayers(data)) {
  meanNDVI[k] <- mean(values(data[[k]]), na.rm=TRUE)
}

               #VOD

meanVOD <- vector(length = nlayers(data2))

for(k in 1: nlayers(data2)) {
  meanVOD[k] <- mean(values(data2[[k]]), na.rm=TRUE)
}

            #EVI
meanEVI <- vector(length = nlayers(data2))

for(k in 1: nlayers(data2)) {
  meanEVI[k] <- mean(values(data3[[k]]), na.rm=TRUE)
}


#calculating the roll means of NDVI, VOD and EVI 

rollNDVI <- rollmean(meanNDVI,6)
rollEVI <- rollmean(meanEVI,6)
rollVOD <- rollmean(meanVOD,3)



y1 <- (rollNDVI[1:length(rollNDVI)]) 
y2 <- (rollVOD[1:length(rollVOD)])
y3 <- (rollEVI[1:length(rollEVI)])
#create sequnce for each dataset
x1 <-seq(from= 1, to= length(rollNDVI), by=1)  
x2 <-seq(from= 337, to= length(rollNDVI), by=2)
x3 <-seq(from=577, to=length(rollNDVI), by=1)
#length(x2) 
#length(y2)
#create lines of fit, then plot (x1,y1), then fit (x2,y2) as lines.
#smoothingSpline = smooth.spline(x1, y1, spar=0.35)
#smoothingSpline2 = smooth.spline(x2, y2, spar=0.35)
plot(x1, y1, type="l", col="red", main= NDVI~VOD~1982~2015, ylim=c(0,0.4)) 
#lines(smoothingSpline, col="white")
#lines(smoothingSpline2, col=6)
lines(x2,y2, col="seagreen")
lines(x3,y3, col="blue")





??ylim
#lm for each line
#lm(y1~x1)
#abline(3.041e-01, 2.465e-05, col=2)

#lm(y2~x2)
#abline(2.511e-01, 7.634e-05, col=4)



#smoothingSpline = smooth.spline(x1, y1, spar=0.35)
#plot(x1,y1, type="l", col=4)
#lines(smoothingSpline, col="red")
#abline(h=0.315, col=2)

#plot NDVI- mean(NDVI) to show anaomlaies of grpahd
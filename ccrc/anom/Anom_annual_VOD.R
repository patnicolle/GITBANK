#MEANVOD 2017.12.08 
rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive")
library(sp)
library(ncdf4)
library(raster) 
library(SPAr)
file2 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"

data <- brick(file2)


annual_VOD <- brick()
yearsV <- nlayers(data)/12
for (k in 1: yearsV) {
  annualV <- mean(data[[(k*12-11):(k*12)]])
  annual_VOD <- addLayer(annual_VOD, annualV)
}

meanannual_VOD <- mean(annual_VOD) 

anom.annaul.VOD<- annual_VOD- meanannual_VOD


anombreaksV <- c(-0.3, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.3)

anomcolsV <- colorRampPalette(c("red", "red", "orangered4", "rosybrown2","lightgrey", "lightgrey", "darkseagreen", "olivedrab2", "limegreen", "limegreen")) 

pdf(file = "A_NEW_PLOT/VOD_ANOM_IMAGES3.pdf", height = 12, width = 14)
par(mfrow=c(4,5))
par(mai=c(0.2,0.5,0.2,0.5))
par(omi=c(0.2,0.2,0.2,0.2))
for( k in 1:nlayers(annual_VOD)) {
  plot(anom.annaul.VOD[[k]], breaks=anombreaksV, col=anomcolsV(length(anombreaksV)-1), main=(k+1992))
}
dev.off()



# 1-1982  


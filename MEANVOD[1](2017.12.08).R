#MEANVOD 2017.12.08 

setwd("~/Desktop/r_practice")
library(sp)
library(ncdf4)
library(raster) 
library(SPAr)
file <- "/Volumes/P_Harddrive/GIMMS3g_NDVI_Australia_1982_2015/Australia_NDVI3g_bimonthly_1982_2015.nc" 
file2 <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"

data <- brick(file)/10000 
data2 <- brick(file2)


annual_VOD <- brick()
yearsV <- nlayers(data2)/12
for (k in 1: yearsV) {
  annualV <- mean(data2[[(k*12-11):(k*12)]])
  annual_VOD <- addLayer(annual_VOD, annualV)
}

yearsV
meanannual_VOD <- mean(annual_VOD) 

anom.annaul.VOD<- annual_VOD- meanannual_VOD


anombreaksV <- c(-0.3, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.3)

anomcolsV <- colorRampPalette(c("red", "red", "orangered4", "rosybrown2","lightgrey", "lightgrey", "darkseagreen", "olivedrab2", "limegreen", "limegreen")) 

pdf(file = "VODIMAGES3.pdf", height = 12, width = 14)
par(mfrow=c(4,5))
par(mai=c(0.2,0.5,0.2,0.5))
par(omi=c(0.2,0.2,0.2,0.2))
for( k in 1:20) {
  
  plot(anom.annaul.VOD[[k]], breaks=anombreaksV, col=anomcolsV(length(breaks)-1), main=(k+1992))
 
}
dev.off()



# 1-1982  


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
  annualV <- mean(data2[[(k*12-13):(k*12)]])
  annual_VOD <- addLayer(annual_VOD, annualV)
}


meanannual_VOD <- mean(annual_VOD) 

anom.annaul.VOD<- annual_VOD- meanannual_VOD


anombreaksV <- c(-0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25)

anomcolsV <- colorRampPalette(c( "red","sienna2", "sienna4", "wheat3", "lightgrey", "darkolivegreen4","darkolivegreen3","darkolivegreen2" )) 

plot(anom.annaul.VOD[[1:5]], breaks=anombreaksV, col=anomcolsV(length(breaks)-1)main= c("1993" : "1997")) 

# 1-1982  


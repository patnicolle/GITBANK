

setwd("~/Desktop/r_practice")
library(sp)
library(ncdf4)
library(raster) 
library(SPAr)
file <- "/Volumes/P_Harddrive/GIMMS3g_NDVI_Australia_1982_2015/Australia_NDVI3g_bimonthly_1982_2015.nc" 
file2 <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"


data <- brick(file)/10000 
data2 <- brick(file2)


annual_ndvi <- brick()
years <- nlayers(data)/24
for (k in 1: years) {
  annual <- mean(data[[(k*24-23):(k*24)]])
  annual_ndvi <- addLayer(annual_ndvi, annual)
}

meanannual_ndvi <- mean(annual_ndvi) 

anom.annaul.ndvi<- annual_ndvi- meanannual_ndvi

png("anomannualndvi.png")

plot(anom.annaul.ndvi[[19:29]]) 
dev.off


anombreaks <- c(-0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25)

anomcols <- colorRampPalette(c( "red","sienna2", "sienna4", "wheat3", "lightgrey", "darkolivegreen4","darkolivegreen3","darkolivegreen2" )) 

plot(anom.annaul.ndvi[[19:29]], breaks=anombreaks, col=anomcols(length(breaks)-1)) 

# 1-1982  


plot(anom.annaul.ndvi[[31:34]], breaks=anombreaks, col=anomcols(length(breaks)-1), main= c("2012" : "2015")) 


rm(list=ls(all=TRUE)) # removes all old variables
setwd("~/Desktop/r_practice")
library(sp)
library(ncdf4)
library(raster) 
library(SPAr)

file <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/",full.names = TRUE)
file2 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"



ndviunflipped <- brick() 
for (k in 1:length(file)) {
  datan = brick(file [k])
  ndviunflipped <- addLayer(ndviunflipped, datan)
}
ndvi <-(flip(t(ndviunflipped), direction = "x"))
data <- ndvi
data2 <- brick(file2)



plot(ndvi)

meanannual_ndvi <- mean(data)
anom.annaul.ndvi<- data - meanannual_ndvi

png("123anomannualndvireal.png")

plot(anom.annaul.ndvi) 
dev.off


anombreaks <- c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4)

anomcols <- colorRampPalette(c( "red","sienna2", "sienna4", "wheat3", "lightgrey", "darkolivegreen4","darkolivegreen3","darkolivegreen2" )) 


plot(anom.annaul.ndvi, breaks=anombreaks, col=anomcols(length(anombreaks)-1)) 





rm(list=ls(all=TRUE)) # removes all old variables
setwd("/Volumes/P_Harddrive")
library(sp)
library(ncdf4)
library(raster) 
library(SPAr)

file <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/",full.names = TRUE)

ndviunflipped <- brick() 
for (k in 1:length(file)) {
  datan = brick(file[k])
  ndviunflipped <- addLayer(ndviunflipped, datan)
}
annual_NDVI <-(flip(t(ndviunflipped), direction = "x"))

meanannual_NDVI <- mean(annual_NDVI) 

anom.annaul.NDVI<- annual_NDVI- meanannual_NDVI


anombreaksV <- c(-0.3, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.3)

anomcolsV <- colorRampPalette(c("red", "red", "orangered4", "rosybrown2","lightgrey", "lightgrey", "darkseagreen", "olivedrab2", "limegreen", "limegreen")) 

pdf(file = "A_NEW_PLOT/NDVI_ANOM_IMAGES3.pdf", height = 12, width = 14)
par(mfrow=c(4,5))
par(mai=c(0.2,0.5,0.2,0.5))
par(omi=c(0.2,0.2,0.2,0.2))
for( k in 1:nlayers(annual_NDVI)) {
  plot(anom.annaul.NDVI[[k]], breaks=anombreaksV, col=anomcolsV(length(anombreaksV)-1), main=(k+1981))
}
dev.off()




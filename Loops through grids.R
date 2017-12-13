 #loops through grids 

rm(list=ls(all=TRUE)) # removes all old variables

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF)


file.ndvi <- list.files("/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/", full.names = TRUE)
file.precip <- list.files("/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)

Pre1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Pre1 <- addLayer(Pre1, datap)
}
#pre1 <-(flip(t(Precipitation1), direction = "x")) 
 
#___________________________________________________________
ndvi <- brick() 
  for (k in 1:length(file.ndvi)) {
    datan = brick(file.ndvi [k])
    ndvi <- addLayer(ndvi, datan)
}
 #ndvi <-(flip(t(ndviunflipped), direction = "x"))

#_______________________________________________________________________________________
 
 pre <- brick() 
nd <- brick() 
PET <- brick() 
#_____________________________________________________________________________________

#generating a precip file with same extent and dimensions as ndvi 
rainfall <- pre1[[13:41]]

rainfall <- resample(rainfall, ndvi)

rainfall <- crop(rainfall, ndvi)

rainfall <- mask(rainfall, ndvi)

#-----------------------------------------------------------------------------------------

slope <- brick(nl=2, ncols=ncol(rainfall), nrows=nrow(rainfall), xmn=xmin(rainfall), xmx=xmax(rainfall), ymn=ymin(rainfall), ymx=ymax(rainfall))

for (i in 1: nrow(rainfall)) {
  for (j in 1: ncol(rainfall)) {
   
    x <- as.vector(rainfall[i,j]) 
    y <- as.vector(ndvi[i,j])
    
    if (all(is.na(x))) next  #use all (all(is.na(x))) to find complete na values, then next to skip to next loop
    
    lm2.lm <- lm(y ~ x)
    
    slope[[1]][i,j] <- lm2.lm$coefficients[2]

    slope[[2]][i,j] <- glance(lm2.lm)$p.value
    
  }
}
#----------------------------------------------------------------------------------------
outputfile <- "ndvirainfallregression.nc"

writeRaster(x=slope, filename=outputfile, varname="correlation", 
            longname="Linear Regression Ndvi~Precipitation")

import <- "~/Desktop/NDVI_VOD/ndvirainfallregression.nc"
data_import <- brick(import)
data_import

breaks <- c(-2e-04, 0, 2e-04, 4e-04, 6e-04, 8e-04, 10e-04)
cols <- colorRampPalette(c( "moccasin", "khaki", "lemonchiffon2" , "darkseagreen", "olivedrab2", "limegreen", "limegreen"))

breaks2 <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)
cols2 <- colorRampPalette(c("darkolivegreen", "darkseagreen4", "darkseagreen", "darkseagreen3", "olivedrab3", "olivedrab2", "white", "white", "white", "white"))
cols3 <- colorRampPalette(c("red", "blue"))

plot(data_import[[1]], breaks=breaks, col=cols(length(breaks)-1))
plot(data_import[[2]], breaks=breaks2, col=cols3(length(breaks2)-1))

class(data_import)

pvals <- data_import[[2]]
NDVIlineartrends <- data_import[[1]]




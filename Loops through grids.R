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



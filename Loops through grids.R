 #loops through grids 

rm(list=ls(all=TRUE)) # removes all old variables

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF)


file.ndvi <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/", full.names = TRUE)
file.precip <- list.files("/Volumes/P_Harddrive/Annual_data/Precipitation/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)

Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}
pre1 <-(flip(t(Precipitation1), direction = "x")) 
 
#___________________________________________________________
ndviunflipped <- brick() 
  for (k in 1:length(file.ndvi)) {
    datan = brick(file.ndvi [k])
    ndviunflipped <- addLayer(ndviunflipped, datan)
}
 ndvi <-(flip(t(ndviunflipped), direction = "x"))

#___________________________________________________________
 
 pre <- brick() 
nd <- brick() 
PET <- brick() 
#_________________________________________________________
rainfall <- pre1[[13:41]]



rainfall <- resample(rainfall, ndvi)

rainfall <- crop(rainfall, ndvi)

rainfall <- mask(rainfall, ndvi)


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
slope <- "grid_regression_rain_ndvi.nc"
output_file <- "grid_regression_rain_ndvi.nc"
writeRaster(x=slope, filename=output_file, varname="Regressed", 
            longname="Linear Regression Ndvi~Precipitation")




glance(lm2.lm)$p.value

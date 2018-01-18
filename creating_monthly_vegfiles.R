#making veg monthly 


rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented) 
library(stats)

file.precipndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/ANUCLIM_precipitation_1982_2008_NDVI_resolution.nc"

file.evi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/EVI/Australia_EVI_16day_composite_8km_2001_2014.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/EVI/ANUCLIM_precip_bimonthly_2001_2014_EVI_resolution.nc"
file.vod <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc"

data<- brick(file.evi)
data<- data[[1:322]]
data2<- brick(file.vod)
data3<- brick(file.ndvi)
data4<- brick(file.precip)
data5 <- brick(file.precipndvi)
#-------------------------------------------------------------------------------
eviyears <- brick()
for (k in 0:((nlayers(data)-1)/23)) {

  r <- data[[(1+(k*23)):(23+(k*23))]]

for (l in 1:12) {
  
if(l == 12) {  
a <- r[[nlayers(r)]] 
eviyears <- addLayer(eviyears, a)
} 
else {
b <- mean(r[[l*2-1]], r[[(l*2)]])
eviyears <- addLayer(eviyears, b)
}}}


outputfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/EVI/MONTHLYEVI.nc" 

writeRaster(x=eviyears, filename=outputfile, varname="EVI", 
            longname="EVIMONTHLY 2001:2014", overwrite=TRUE)

#-------------------------------------------------------------------------------

eviprecipmonthly <- brick()
for (k in 0:((nlayers(data4)-1)/23)) {
  
  r <- data4[[(1+(k*23)):(23+(k*23))]]
  
  for (l in 1:12) {
    
    if(l == 12) {  
      a <- r[[nlayers(r)]] 
      eviprecipmonthly <- addLayer(eviprecipmonthly, a)
    } 
    else {
      b <- mean(r[[l*2-1]], r[[(l*2)]])
      eviprecipmonthly <- addLayer(eviprecipmonthly, b)
    }}}


outputfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/EVI/MONTHLYPRECIPEVI.nc"

writeRaster(x=eviprecipmonthly, filename=outputfile, varname="precip", 
            longname="EVIMONTHLYprecip 2001:2014", overwrite=TRUE)

#-------------------------------------------------------------------------------

ndvimonthly <- brick() 
for (k in 0:((nlayers(data3)/24)-1)) {
  r <- data3[[((k*24)+1):((k*24)+24)]]
  
  for (l in 1:12) {
      b <- mean(r[[l*2-1]], r[[(l*2)]])
      ndvimonthly <- addLayer(ndvimonthly, b)
    }}


outputfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/MONTHLYNDVI.nc"

writeRaster(x=ndvimonthly, filename=outputfile, varname="ndvi", 
            longname="NDVIMONTHLYprecip 2001:2014", overwrite=TRUE)


#-------------------------------------------------------------------------------

ndvimonthly_precip <- brick() 
for (k in 0:((nlayers(data5)/24)-1)) {
  r <- data5[[((k*24)+1):((k*24)+24)]]
  
  for (l in 1:12) {
    b <- mean(r[[l*2-1]], r[[(l*2)]])
    ndvimonthly_precip <- addLayer(ndvimonthly_precip, b)
  }}


outputfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/MONTHLYPRECIPNDVI.nc"

writeRaster(x=ndvimonthly, filename=outputfile, varname="precip", 
            longname="NDVIMONTHLYprecip 82:08", overwrite=TRUE)




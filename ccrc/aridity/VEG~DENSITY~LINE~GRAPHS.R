rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 

# !!!!!$$$$$change input files and it should run for all veg data, !!!ALSO!!! change the pdf file name to the right veg type
file1 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/EVI/EVI_annual_integrated_values_2001_2014_ANUCLIM_precipitation_detrended_dry_years.nc"
file2 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/EVI/EVI_annual_integrated_values_2001_2014_ANUCLIM_precipitation_detrended_normal_years.nc"
file3 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/EVI/EVI_annual_integrated_values_2001_2014_ANUCLIM_precipitation_detrended_wet_years.nc"
file4 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/EVI/EVI_annual_integrated_values_2001_2014_ANUCLIM_precipitation_detrended_extreme_dry_years.nc"
file5 <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/EVI/EVI_annual_integrated_values_2001_2014_ANUCLIM_precipitation_detrended_extreme_wet_years.nc"

precfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precipitation/EVI/EVI_annual_integrated_values_2001_2014_ANUCLIM_precipitation_detrended.nc"
vegfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Vegetation_indices/EVI/EVI_annual_integrated_values_2001_2014_masked_gapfilled_detrended.nc"

dry <- brick(file1)
normal <- brick(file2)
wet <- brick(file3)
vdry <- brick(file4)
vwet <- brick(file5)

vegin <- brick(vegfile)
meanveg <- mean(vegin)
veg= values(vegin-meanveg)

inddry <- which(values(is.na(dry)))
indnormal <- which(values(is.na(normal)))
indwet <- which(values(is.na(wet)))
indvdry <- which(values(is.na(vdry)))
indvwet <- which(values(is.na(vwet)))

dryveg <- veg[-inddry]
normalveg <- veg[-indnormal]
wetveg <- veg[-indwet]
vdryveg <- veg[-indvdry]
vwetveg <- veg[-indvwet]

densitydry <- density(dryveg, na.rm=TRUE)
densitynormal <- density(normalveg, na.rm=TRUE)
densitywet <- density(wetveg, na.rm=TRUE)
densityvdry <- density(vdryveg, na.rm=TRUE)
densityvwet <- density(vwetveg, na.rm=TRUE)

#Plot density


pdf("A_NEW_PLOT/densitylines/evi~density~anomalousrainfallmask.pdf")
plot(densityvdry$x, densityvdry$y, type="l", col="#e41a1c", ylim=c(0,2.5),xlim=c(-2,2), lwd=2)
lines(densitydry$x, densitydry$y, col="#ff7f00",lwd=2)
lines(densitynormal$x, densitynormal$y, col="#984ea3",lwd=2)
lines(densitywet$x, densitywet$y, col="#4daf4a",lwd=2)
lines(densityvwet$x, densityvwet$y, col="#377eb8",lwd=2)
dev.off()
#plot(densitynormal$x, densitynormal$y, type="l", col="green")
#plot(densitywet$x, densitywet$y, type="l", col="blue")
#evi~density~anomalousrainfallmask.pdf



















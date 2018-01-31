rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 

precfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precipitation/NDVI/NDVI_annual_integrated_values_1982_2014_ANUCLIM_precipitation_detrended.nc"
vegfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Vegetation_indices/NDVI/NDVI_annual_integrated_values_1982_2014_masked_gapfilled_detrended.nc"
landfile <-"/Volumes/P_Harddrive/LAI_precip_variability/Data/land_cover/Dynamic_land_cover_map_7_classes.nc"
precipdryfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/NDVI/NDVI_annual_integrated_values_1982_2014_ANUCLIM_precipitation_detrended_dry_years.nc"
precipnormalfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/NDVI/NDVI_annual_integrated_values_1982_2014_ANUCLIM_precipitation_detrended_normal_years.nc"
precipwetfile <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precip_by_percentile/NDVI/NDVI_annual_integrated_values_1982_2014_ANUCLIM_precipitation_detrended_wet_years.nc"

dry <-brick(precipdryfile)
normal <- brick(precipnormalfile)
wet <- brick(precipwetfile)
prec <-brick(precfile)
veg <-brick(vegfile)
landbrick <-brick(landfile)


meanveg <- mean(veg)
anomveg= values(veg-meanveg)




cols <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
cols1 <- c("red", "purple", "blue")
breaks <- 4
land_cover_names <- c("agricultural", "openclosed_herbaceous", "sparsescattered_herbaceous",
                      "openclosed_shrub", "sparsescattered_shrub", "openclosed_woody", 
                      "sparsescattered_woody")


land_cover_resampled <- resample(landbrick[[1]], veg, method="ngb")
 
#---------------------------------------------------------------------------------------------------------
inddry <- which(values(is.na(dry)))
indnormal <- which(values(is.na(normal)))
indwet <- which(values(is.na(wet))) 

dryveg <- anomveg[-inddry]
normalveg <- anomveg[-indnormal]
wetveg <- anomveg[-indwet]


#---------------------------------------------------------------------------------------------------------
#mean response comparing land cover types 

pdf("A_NEW_PLOT/densitylines/landcover_ndvi_lines.pdf")
par(mfrow=c(2,4))
plot(x=NULL, xlim=c(-2,2), ylim=c(0,4))
for (k in 1:length(land_cover_names)){

    ind <- which(values(land_cover_resampled) == k)
  
  veg_land <- anomveg[ind]
  landdensity <-density(as.vector(veg_land), na.rm=TRUE)
  lines(landdensity$x, landdensity$y, type="l", col=cols[k]) 
}
 dev.off()
 #---------------------------------------------------------------------------------------------------------
 #dry normal wet comparsion within land cover class
 pdf("A_NEW_PLOT/densitylines/landcover_ndvi_plots.pdf")
 par(mfrow=c(4,2))
 
 for (k in 1:length(land_cover_names)){
   tryCatch({
   ind <- which(values(land_cover_resampled) == k)
   
  vegdry <- dryveg[ind]
  vegnorm <- normalveg[ind]
  vegwet <- wetveg[ind]
   
  densitydry <- density(vegdry, na.rm=TRUE)
   densitynormal <- density(vegnorm, na.rm=TRUE)
    densitywet <- density(vegwet, na.rm=TRUE)
   
    plot(densitydry$x, densitydry$y, type="l", col="red", main=land_cover_names[k], ylim=c(0,5), xlim=c(-2,2)) 
    lines(densitynormal$x, densitynormal$y, type='l', col="purple")
    lines(densitywet$x, densitywet$y, type='l', col="blue")
    legend("topright", inset=.05, c("dry","normal","wet"), fill=cols1, horiz=TRUE, cex=0.5)
 }, error=function(e){})
  }
 
   dev.off()
 
 #---------------------------------------------------------------------------------------------------------
 
 
 tryCatch({
   print(i)
   if (i==7) stop("Urgh, the iphone is in the blender !")
 }, error=function(e){})
 }
 




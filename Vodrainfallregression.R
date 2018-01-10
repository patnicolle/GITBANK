
rm(list=ls(all=TRUE)) # removes all old variables

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF)

file.ndvi <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/",full.names = TRUE)
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/ANUCLIM_precip_bimonthly_1982_2014_GIMMS_resolution.nc"
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)
file.vod <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"
data <- brick(file.precip)

#--------------------------------------------

data2 <- brick(file.vod)
annual_VOD <- brick()
yearsV <- nlayers(data2)/12
for (k in 1: yearsV) {
  annualV <- mean(data2[[(k*12-11):(k*12)]])
  annual_VOD <- addLayer(annual_VOD, annualV)
}

Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}


#------------------------------------------------------

data <- Precipitation1
annual_precip <- brick()
years <- nlayers(data)/24
for (k in 1: years) {
  annual <- mean(data[[(k*24-23):(k*24)]])
  annual_precip <- addLayer(annual_precip, annual)
}

anprec <- annual_precip[[12:31]]

#-----------------------------------------------------
vOd <- annual_VOD
rainfall <- anprec

vod <- resample(vOd, rainfall)

vod <- crop(vod, rainfall)

vod <- mask(vod, rainfall)

#nlayers, how many layers in the file

#creating vectors of mean values per layer


x <- as.vector(rainfall)
y<- as.vector(vod) 

plot(x,y)
lm1.lm <- lm(y~x)
abline(lm1.lm, col=3)



seqr<- seq(from = 0,to = 297.5, by = 2.5)

percentile <- matrix(data=NA, nrow = nlayers(vod), ncol = length(seqr))
for (b in 1: nlayers(vod)) {
  o <- values(vod[[b]])
  precip <- values(rainfall[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+2.5) 
    a <- o[ind] 
    percentile[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}

precipins<- matrix(data=seqr, nrow = nlayers(vod), ncol = length(seqr), byrow = TRUE)
p <- as.vector(precipins)
q <- as.vector(percentile) 
plot(p,q)
lm2.lm <- lm(q~p)
abline(lm2.lm, col=3)
segmented_lm <- segmented(lm2.lm, seg.Z= ~p, psi = 0.1)


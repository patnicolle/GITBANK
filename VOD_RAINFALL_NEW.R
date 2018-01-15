#new aridity vod 

rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)

file.precipitation <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/VOD/ANUCLIM_precip_monthly_1970_2014_VOD_resolution_fixed.nc"
file.vod <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"
file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc"
file.precip <- list.files("/Volumes/P_Harddrive/Annual_data/Precipitation/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)



data1 <- brick(file.precipitation) #or wetprecip/aridprecip
data <- data1[[276:515]]



#datan <- brick(file.ndvi) #or wetav/aridav
av <- brick(file.vod)
#dataPET <- brick(file.PET)
#datap <- brick(file.precip)

#Annual Precip
Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}

annual_precip <-(flip(t(Precipitation1), direction = "x"))

rainfall <- annual_precip[[12:31]]

#-----------------------------------------------------

#annual PET
PET<- brick() 
for (k in 1:length(file.PET)) {
  dataPET = brick(file.PET [k])
  PET <- addLayer(PET, dataPET) 
}
#---------------------------------------
#mean layers 
meanPET <- mean(PET, na.rm= TRUE)
meanPrecip <- mean(rainfall) 

ariditylayer <- (meanPET/meanPrecip)
ariditylayer <- resample(ariditylayer, av)
ariditylayer <- crop(ariditylayer, av)
ariditylayer <- mask(ariditylayer, av)

wet <- ariditylayer
arid <- ariditylayer


wet[wet>1] <- NA 
arid[arid<5] <- NA 

#--------------------------------------------
#VOD MASK
wetav <- resample(wet,av)
wetav <- mask(av, wetav) 
#--------------------------------------------
aridav <- resample(arid,av)
aridav <- mask(av, aridav) 

#---------------------------------------------
#PRECIPITATION MASK
wetprecip <- data
wetprecip <- resample(wetprecip, av)
wetprecip <- crop(wetprecip, av)
wetprecip <- mask(wetprecip, wet) 

aridprecip <- data
aridprecip <- resample(aridprecip, av)
aridprecip <- crop(aridprecip, av)
aridprecip <- mask(aridprecip, arid) 

x1 <- as.vector(values(wetprecip))
x4 <- as.vector(values(aridprecip))

y1 <- as.vector(values(wetav))
y4 <- as.vector(values(aridav))

pdf("wetvodregression.pdf") 
plot(x1,y1, cex=0.5, pch=20) 
lm1.lm <- lm(y1~x1) 
abline(lm1.lm, col=1)
dev.off() 

pdf("aridvodregression.pdf") 
plot(x4,y4, cex=0.5, pch=20) 
lm4.lm <- lm(y4~x4) 
abline(lm4.lm, col=4)
dev.off()



# ALL
seqr<- seq(from = 0,to = 700, by = 5)

percentile <- matrix(data=NA, nrow = nlayers(av), ncol = length(seqr))
for (b in 1: nlayers(av)) {
  o <- values(av[[b]])
  precip <- values(rainfall[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentile[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#WET
seqr<- seq(from = 0,to = 700, by = 5)

percentilewet <- matrix(data=NA, nrow = nlayers(av), ncol = length(seqr))
for (b in 1: nlayers(av)) {
  o <- values(wetav[[b]])
  precip <- values(wetprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilewet[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}

#ARID
seqr<- seq(from = 0,to =700, by = 5)

percentilearid <- matrix(data=NA, nrow = nlayers(av), ncol = length(seqr))
for (b in 1: nlayers(av)) {
  o <- values(aridav[[b]])
  precip <- values(aridprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilearid[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}

precipins<- matrix(data=seqr, nrow = nlayers(av), ncol = length(seqr), byrow = TRUE)
v <- as.vector(precipins)
#plot(precipins,percentile)

all <- as.vector(percentile) 
w <- as.vector(percentilewet)
aridd <- as.vector(percentilearid)

lmall.lm <- lm(all~v)
lmw.lm <- lm(w~v)
lma.lm <- lm(aridd~v)

plot(v,w) 
abline(lmw.lm, col="blue")
plot(v,aridd)
abline(lma.lm, col= "red")



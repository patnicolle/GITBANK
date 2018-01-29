rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/ANUCLIM_precip_bimonthly_1982_2014_GIMMS_resolution.nc"
file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc"
file.precipa <- list.files("/Volumes/P_Harddrive/Annual_data/Precipitation/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)
file.vod <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"
P <- brick(file.precip)
data2 <- brick(file.ndvi)
av <- data2
Precipitation1 <- brick() 
for (k in 1:length(file.precipa)) {
  datap = brick(file.precipa[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}
precip1 <-(flip(t(Precipitation1), direction = "x"))
rainfall <- precip1[[12:31]]
#-----------------------------------------------------
#mask ndvi to rainfalal???
#--------------------------------------
PET<- brick() 
for (k in 1:length(file.PET)) {
  dataPET = brick(file.PET [k])
  PET <- addLayer(PET, dataPET) 
}
#---------------------------------------
#mean layers/ ARIDITY LAYERS
meanPET <- mean(PET, na.rm= TRUE)
meanPrecip <- mean(rainfall) 
ariditylayer <- (meanPET/meanPrecip)
ariditylayer <- resample(ariditylayer, av)
ariditylayer <- crop(ariditylayer, av)
ariditylayer <- mask(ariditylayer, av)
ariditylayer <- ariditylayer[[1:792]]
wet <- ariditylayer
sub_humid <- ariditylayer
semi_arid <- ariditylayer
arid <- ariditylayer
wet[wet>1] <- NA 
sub_humid[sub_humid<1 | sub_humid>2] <- NA 
semi_arid[semi_arid<2 | semi_arid>5] <- NA 
arid[arid<5] <- NA 
#--------------------------------------------
#VEGETATION MASK
wetav <- resample(wet,av)
wetav <- mask(av, wetav) 
#--------------------------------------------
aridav <- resample(arid,av)
aridav <- mask(av, aridav) 
#---------------------------------------------
#PRECIPITATION MASK
wetprecip <- P
wetprecip <- resample(wetprecip, av)
wetprecip <- crop(wetprecip, av)
wetprecip <- mask(wetprecip, wet) 
aridprecip <- P
aridprecip <- resample(aridprecip, av)
aridprecip <- crop(aridprecip, av)
aridprecip <- mask(aridprecip, arid) 

x1 <- as.vector(values(wetprecip))
x4 <- as.vector(values(aridprecip))
y1 <- as.vector(values(wetav))
y4 <- as.vector(values(aridav))
#pdf("wetregression.pdf") 
#plot(x1,y1, cex=0.5, pch=20) 
#lm1.lm <- lm(y1~x1) 
#abline(lm1.lm, col=1)
#dev.off() 
#pdf("sub_humidregression.pdf") 
#plot(x2,y2, cex=0.5, pch= 20) 
#lm2.lm <- lm(y2~x2) 
#abline(lm2.lm, col=2)
#dev.off() 
#pdf("semi_aridregression.pdf") 
#plot(x3,y3, cex=0.5, pch=20) 
#lm3.lm <- lm(y3~x3) 
#abline(lm3.lm, col=3)
#dev.off()
#pdf("aridregression.pdf") 
#plot(x4,y4, cex=0.5, pch=20) 
#lm4.lm <- lm(y4~x4) 
#abline(lm4.lm, col=4)
#dev.off()
#WET
seqr<- seq(from = 0,to = 2975, by = 25)
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
seqr<- seq(from = 0,to = 2975, by = 25)

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
lmw.lm <- lm(w~v)
lma.lm <- lm(aridd~v)
data <- brick(file.precip) #or wetprecip/aridprecip
data2 <- brick(file.ndvi) #or wetav/aridav
data2 <- data2[[1:792]] 
P <- data 
P[P<1] <- NA
P <- P[[1:790]]
deltaNDVI <- brick()
for (k in 1:(nlayers(data2)-2)) {
  r <- mean(data2[[k+1]], data2[[k+2]])- data2[[k]]
  deltaNDVI <- addLayer(deltaNDVI, r)
}

P[deltaNDVI<0] <- NA
deltaNDVI[deltaNDVI<0 &] <- NA 
deltaNDVI[is.na(P)] <- NA
#plot first few layers 
#plot(P[[1]], deltaNDVI[[1]])
#p1 <- as.vector(P[[1]])
#d1 <- as.vector(deltaNDVI[[1]])
#lm1.lm <- lm(d1~p1)
#abline(lm1.lm, col=3)
x <- values(P)
y <- values(deltaNDVI)
ind <- which(is.na(y))
y <- y[-ind]
x <- x[-ind] 
lmtotal.lm <- lm(y~x)
summary(lmtotal.lm)$r.squared
png("deltaNDVI_precip_10biweek.png") #check which input data is used before running
plot(x,y, cex=0.5)
abline(lmtotal.lm, col=3)
dev.off()




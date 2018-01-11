rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")


library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)

file.ndvi <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/",full.names = TRUE)
file.precip <- list.files("/Volumes/P_Harddrive/Annual_data/Precipitation/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)
file.vod <- "/Volumes/P_Harddrive/VOD_Australia_1993_2012/Australia_VOD_monthly_1993_2012.nc"
file.evi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/EVI/Australia_EVI_16day_composite_8km_2001_2014.nc"

Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}

precip1 <-(flip(t(Precipitation1), direction = "x"))

rainfall <- precip1[[32:41]]

#-----------------------------------------------------

data2 <- brick(file.evi)
annual_EVI <- brick()
yearsV <- nlayers(data2)/24
for (k in 1: yearsV) {
  annualE <- mean(data2[[(k*24-23):(k*24)]])
  annual_EVI <- addLayer(annual_EVI, annualE)
}
evi <- annual_EVI[[1:10]]

evi <- resample(evi, rainfall)
evi <- crop(evi, rainfall)
evi <- mask(evi, rainfall)

#--------------------------------------
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
ariditylayer <- resample(ariditylayer, evi)
ariditylayer <- crop(ariditylayer, evi)
ariditylayer <- mask(ariditylayer, evi)

wet <- ariditylayer
sub_humid <- ariditylayer
semi_arid <- ariditylayer
arid <- ariditylayer


wet[wet>1] <- NA 
sub_humid[sub_humid<1 | sub_humid>2] <- NA 
semi_arid[semi_arid<2 | semi_arid>5] <- NA 
arid[arid<5] <- NA 
#--------------------------------------------
#VOD MASK
wetevi <- resample(wet,evi)
wetevi <- mask(evi, wetevi) 
#--------------------------------------------
sub_humidevi <- resample(sub_humid,evi)
sub_humidevi <- mask(evi, sub_humidevi)  
#---------------------------------------------
s_aridevi <- resample(semi_arid,evi)
s_aridevi <- mask(evi, s_aridevi) 
#--------------------------------------------
aridevi <- resample(arid,evi)
aridevi <- mask(evi, aridevi) 

#---------------------------------------------
#PRECIPITATION MASK
wetprecip <- rainfall
wetprecip <- resample(wetprecip, evi)
wetprecip <- crop(wetprecip, evi)
wetprecip <- mask(wetprecip, wet) 

s_humidprecip <- rainfall
s_humidprecip <- resample(s_humidprecip, evi)
s_humidprecip <- crop(s_humidprecip, evi)
s_humidprecip <- mask(s_humidprecip, sub_humid) 

s_aridprecip <- rainfall
s_aridprecip <- resample(s_aridprecip, evi)
s_aridprecip <- crop(s_aridprecip, evi)
s_aridprecip <- mask(s_aridprecip, semi_arid)

aridprecip <- rainfall
aridprecip <- resample(aridprecip, evi)
aridprecip <- crop(aridprecip, evi)
aridprecip <- mask(aridprecip, arid) 

x1 <- as.vector(values(wetprecip))
x2 <- as.vector(values(s_humidprecip))
x3 <- as.vector(values(s_aridprecip))
x4 <- as.vector(values(aridprecip))

y1 <- as.vector(values(wetevi))
y2 <- as.vector(values(sub_humidevi))
y3 <- as.vector(values(s_aridevi))
y4 <- as.vector(values(aridevi))

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



#NDVI MASK
wetndvi 
#--------------------------------------------
sub_humidndvi 
#---------------------------------------------
s_aridndvi 
#--------------------------------------------
aridndvi 
#---------------------------------------------
#PRECIPITATION MASK

wetprecip 
#---------------
s_humidprecip
#---------------
s_aridprecip
#---------------
aridprecip 
#---------------
# ALL
seqr<- seq(from = 0,to = 2975, by = 25)

percentile <- matrix(data=NA, nrow = nlayers(evi), ncol = length(seqr))
for (b in 1: nlayers(evi)) {
  o <- values(evi[[b]])
  precip <- values(rainfall[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentile[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#WET
seqr<- seq(from = 0,to = 2975, by = 25)

percentilewet <- matrix(data=NA, nrow = nlayers(evi), ncol = length(seqr))
for (b in 1: nlayers(evi)) {
  o <- values(wetevi[[b]])
  precip <- values(wetprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilewet[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#SUBHUMID
seqr<- seq(from = 0,to = 2975, by = 25)

percentilesub <- matrix(data=NA, nrow = nlayers(evi), ncol = length(seqr))
for (b in 1: nlayers(evi)) {
  o <- values(sub_humidevi[[b]])
  precip <- values(s_humidprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilesub[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#SEMIARID 
seqr<- seq(from = 0,to = 2975, by = 25)

percentilesemi <- matrix(data=NA, nrow = nlayers(evi), ncol = length(seqr))
for (b in 1: nlayers(evi)) {
  o <- values(s_aridevi[[b]])
  precip <- values(s_aridprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilesemi[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#ARID
seqr<- seq(from = 0,to = 2975, by = 25)

percentilearid <- matrix(data=NA, nrow = nlayers(evi), ncol = length(seqr))
for (b in 1: nlayers(evi)) {
  o <- values(aridevi[[b]])
  precip <- values(aridprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilearid[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}





precipins<- matrix(data=seqr, nrow = nlayers(evi), ncol = length(seqr), byrow = TRUE)
v <- as.vector(precipins)
#plot(precipins,percentile)

all <- as.vector(percentile) 
w <- as.vector(percentilewet)
sub <- as.vector(percentilesub)
semi <- as.vector(percentilesemi)
aridd <- as.vector(percentilearid)

lmw.lm <- lm(w~v)
lmsh.lm <- lm(sub~v)
lmsa.lm <- lm(semi~v)
lma.lm <- lm(aridd~v)

plot(v,w) 
abline(lmw.lm, col="blue")
plot(v,sub)
abline(lmsh.lm, col= "green")
plot(v,semi)
abline(lmsa.lm, col= "tan")
plot(v,aridd)
abline(lma.lm, col= "red")

plot(v,all)
abline(lmw.lm, col="blue")
abline(lmsh.lm, col= "green")
abline(lmsa.lm, col= "tan")
abline(lma.lm, col= "red")

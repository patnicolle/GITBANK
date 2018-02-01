rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)

file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Precipitation/VOD/VOD_annual_integrated_values_1993_2012_ANUCLIM_precipitation_detrended.nc"
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)
file.vod <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Annual_data/Vegetation_indices/VOD/VOD_annual_integrated_values_1993_2012_masked_gapfilled_detrended.nc"

#Precipitation1 <- brick() 
#for (k in 1:length(file.precip)) {
  #datap = brick(file.precip[k])
 # Precipitation1 <- addLayer(Precipitation1, datap)
#}

#precip1 <-(flip(t(Precipitation1), direction = "x"))

#rainfall <- precip1[[12:31]]

#-----------------------------------------------------

rainfall <- brick(file.precip)
data2 <- brick(file.vod)
annual_VOD <- data2
#annual_VOD <- brick()
#yearsV <- nlayers(data2)/12
#for (k in 1: yearsV) {
#  annualV <- mean(data2[[(k*12-11):(k*12)]])
#  annual_VOD <- addLayer(annual_VOD, annualV)
#}

av <- annual_VOD/12
av <- resample(av, rainfall)
av <- crop(av, rainfall)
av <- mask(av, rainfall)

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

meanPET <- resample(meanPET,meanPrecip)
ariditylayer <- (meanPET/meanPrecip)
ariditylayer <- resample(ariditylayer, av)
ariditylayer <- crop(ariditylayer, av)

wet <- ariditylayer
sub_humid <- ariditylayer
semi_arid <- ariditylayer
arid <- ariditylayer


wet[wet>1] <- NA 
sub_humid[sub_humid<1 & sub_humid>2] <- NA 
semi_arid[semi_arid<2 & semi_arid>5] <- NA 
arid[arid<5] <- NA 
#--------------------------------------------

#VOD MASK
wetav <- resample(wet,av)
wetav <- mask(av, wetav) 
#--------------------------------------------
sub_humidav <- resample(sub_humid,av)
sub_humidav <- mask(av, sub_humidav)  
#---------------------------------------------
s_aridav <- resample(semi_arid,av)
s_aridav <- mask(av, s_aridav) 
#--------------------------------------------
aridav <- resample(arid,av)
aridav <- mask(av, aridav) 

#---------------------------------------------
#PRECIPITATION MASK
wetprecip <- rainfall
wetprecip <- resample(wetprecip, av)
wetprecip <- crop(wetprecip, av)
wetprecip <- mask(wetprecip, wet) 

shumidprecip <- rainfall
shumidprecip <- resample(shumidprecip, av)
shumidprecip <- crop(shumidprecip, av)
shumidprecip <- mask(shumidprecip, sub_humid) 

s_aridprecip <- rainfall
s_aridprecip <- resample(s_aridprecip, av)
s_aridprecip <- crop(s_aridprecip, av)
s_aridprecip <- mask(s_aridprecip, semi_arid)

aridprecip <- rainfall
aridprecip <- resample(aridprecip, av)
aridprecip <- crop(aridprecip, av)
aridprecip <- mask(aridprecip, arid) 

x1 <- as.vector(values(wetprecip))
x2 <- as.vector(values(shumidprecip))
x3 <- as.vector(values(s_aridprecip))
x4 <- as.vector(values(aridprecip))

y1 <- as.vector(values(wetav))
y2 <- as.vector(values(sub_humidav))
y3 <- as.vector(values(s_aridav))
y4 <- as.vector(values(aridav))

pdf("A_NEW_PLOT/aridity/wetregression.pdf") 
plot(x1,y1, cex=0.5, pch=20) 
lm1.lm <- lm(y1~x1) 
abline(lm1.lm, col=1)
legend("topright", bty="n", legend=paste("r2=",format(summary(lm1.lm)$adj.r.squared, digits=4)))
dev.off() 

pdf("A_NEW_PLOT/aridity/sub_humidregression.pdf") 
plot(x2,y2, cex=0.5, pch= 20) 
lm2.lm <- lm(y2~x2) 
abline(lm2.lm, col=2)
legend("topright", bty="n", legend=paste("r2=",format(summary(lm2.lm)$adj.r.squared, digits=4)))
dev.off() 

pdf("A_NEW_PLOT/aridity/semi_aridregression.pdf") 
plot(x3,y3, cex=0.5, pch=20) 
lm3.lm <- lm(y3~x3) 
abline(lm3.lm, col=3)
legend("topright", bty="n", legend=paste("r2=",format(summary(lm3.lm)$adj.r.squared, digits=4)))
dev.off()

pdf("A_NEW_PLOT/aridity/aridregression.pdf") 
plot(x4,y4, cex=0.5, pch=20) 
lm4.lm <- lm(y4~x4) 
abline(lm4.lm, col=4)
legend("topright", bty="n", legend=paste("r2=",format(summary(lm4.lm)$adj.r.squared, digits=4)))
dev.off()



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
#SUBHUMID
seqr<- seq(from = 0,to = 2975, by = 25)

percentilesub <- matrix(data=NA, nrow = nlayers(av), ncol = length(seqr))
for (b in 1: nlayers(av)) {
  o <- values(sub_humidav[[b]])
  precip <- values(s_humidprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilesub[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#SEMIARID 
seqr<- seq(from = 0,to = 2975, by = 25)

percentilesemi <- matrix(data=NA, nrow = nlayers(av), ncol = length(seqr))
for (b in 1: nlayers(av)) {
  o <- values(s_aridav[[b]])
  precip <- values(s_aridprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilesemi[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
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

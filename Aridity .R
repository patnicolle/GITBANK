#aridity classes 

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

Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}


precip1 <-(flip(t(Precipitation1), direction = "x"))
precip2 <- precip1[[13:41]] 

precipitationy <- 



#----------------

ndvi <- brick() 
for (k in 1:length(file.ndvi)) {
  datan = brick(file.ndvi [k])
  ndvi <- addLayer(ndvi, datan) 
}
ndvi <-(flip(t(ndvi), direction = "x"))  
#--------------------------------------
PET<- brick() 
for (k in 1:length(file.PET)) {
  dataPET = brick(file.PET [k])
  PET <- addLayer(PET, dataPET) 
}
#---------------------------------------
#mean layers 
meanPET <- mean(PET, na.rm= TRUE)
meanPrecip <- mean(precip2) 

ariditylayer <- (meanPET/meanPrecip)
ariditylayer <- resample(ariditylayer, ndvi)
ariditylayer <- crop(ariditylayer, ndvi)
ariditylayer <- mask(ariditylayer, ndvi)
  
  wet <- ariditylayer
sub_humid <- ariditylayer
semi_arid <- ariditylayer
arid <- ariditylayer


wet[wet>1] <- NA 
sub_humid[sub_humid<1 | sub_humid>2] <- NA 
semi_arid[semi_arid<2 | semi_arid>5] <- NA 
arid[arid<5] <- NA 
#--------------------------------------------
#NDVI MASK
wetndvi <- resample(wet,ndvi)
wetndvi <- mask(ndvi, wetndvi) 
#--------------------------------------------
sub_humidndvi <- resample(sub_humid,ndvi)
sub_humidndvi <- mask(ndvi, sub_humidndvi)  
#---------------------------------------------
s_aridndvi <- resample(semi_arid,ndvi)
s_aridndvi <- mask(ndvi, s_aridndvi) 
#--------------------------------------------
aridndvi <- resample(arid,ndvi)
aridndvi <- mask(ndvi, aridndvi) 

#---------------------------------------------
#PRECIPITATION MASK
wetprecip <- precip2
wetprecip <- resample(wetprecip, ndvi)
wetprecip <- crop(wetprecip, ndvi)
wetprecip <- mask(wetprecip, wet) 

s_humidprecip <- precip2
s_humidprecip <- resample(s_humidprecip, ndvi)
s_humidprecip <- crop(s_humidprecip, ndvi)
s_humidprecip <- mask(s_humidprecip, sub_humid) 

s_aridprecip <- precip2
s_aridprecip <- resample(s_aridprecip, ndvi)
s_aridprecip <- crop(s_aridprecip, ndvi)
s_aridprecip <- mask(s_aridprecip, semi_arid)

aridprecip <- precip2
aridprecip <- resample(aridprecip, ndvi)
aridprecip <- crop(aridprecip, ndvi)
aridprecip <- mask(aridprecip, arid) 

x1 <- as.vector(values(wetprecip))
x2 <- as.vector(values(s_humidprecip))
x3 <- as.vector(values(s_aridprecip))
x4 <- as.vector(values(aridprecip))
  
y1 <- as.vector(values(wetndvi))
y2 <- as.vector(values(sub_humidndvi))
y3 <- as.vector(values(s_aridndvi))
y4 <- as.vector(values(aridndvi))

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

percentile <- matrix(data=NA, nrow = length(file.ndvi), ncol = length(seqr))
for (b in 1: length(file.ndvi)) {
  o <- values(ndvi[[b]])
  precip <- values(precip2[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentile[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#WET
seqr<- seq(from = 0,to = 2975, by = 25)

percentilewet <- matrix(data=NA, nrow = length(file.ndvi), ncol = length(seqr))
for (b in 1: length(file.ndvi)) {
  o <- values(wetndvi[[b]])
  precip <- values(wetprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilewet[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#SUBHUMID
seqr<- seq(from = 0,to = 2975, by = 25)

percentilesub <- matrix(data=NA, nrow = length(file.ndvi), ncol = length(seqr))
for (b in 1: length(file.ndvi)) {
  o <- values(ndvi[[b]])
  precip <- values(s_humidprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilesub[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#SEMIARID 
seqr<- seq(from = 0,to = 2975, by = 25)

percentilesemi <- matrix(data=NA, nrow = length(file.ndvi), ncol = length(seqr))
for (b in 1: length(file.ndvi)) {
  o <- values(ndvi[[b]])
  precip <- values(s_aridprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilesemi[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}
#ARID
seqr<- seq(from = 0,to = 2975, by = 25)

percentilearid <- matrix(data=NA, nrow = length(file.ndvi), ncol = length(seqr))
for (b in 1: length(file.ndvi)) {
  o <- values(ndvi[[b]])
  precip <- values(aridprecip[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentilearid[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}





precipins<- matrix(data=seqr, nrow = length(file.ndvi), ncol = length(seqr), byrow = TRUE)
v <- as.vector(precipins)
c <- as.vector(percentile) 
plot(precipins,percentile)
lm10.lm <- lm(c~v)

w <- as.vector(percentilewet)
sub <- as.vector(percentilesub)
semi <- as.vector(percentilesemi)
aridd <- as.vector(percentilearid)


plot(v,w)
plot(v,sub)
plot(v,semi)
plot(v,aridd)
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

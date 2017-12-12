setwd("/Volumes/P_Harddrive/")


library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF)


file.ndvi <- list.files("/Volumes/P_Harddrive/Annual_data/ndvi_1982_2011_Australia/", full.names = TRUE)
file.precip <- list.files("/Volumes/P_Harddrive/Annual_data/Precipitation/", full.names = TRUE)
file.PET <- list.files("/Volumes/P_Harddrive/Annual_data/PET_raster/", full.names = TRUE)

file.precip
file.PET

length(file.ndvi)

ndvi <- brick() 
for (k in 1:length(file.ndvi)) {
  datan = brick(file.ndvi [k])
  ndvi <- addLayer(ndvi, datan)
}
#Flip NDVI 
ndvi1 <-(flip(t(ndvi), direction = "x"))


Precipitation1 <- brick() 
for (k in 1:length(file.precip)) {
  datap = brick(file.precip[k])
  Precipitation1 <- addLayer(Precipitation1, datap)
}

precip1 <-(flip(t(Precipitation1), direction = "x"))


PET1 <- brick() 
for (k in 1:length(file.PET)) {
  dataP = brick(file.PET[k])
  PET1 <- addLayer(PET1, dataP)
}



#FOR TUESDAY!! the resample didnt work || "Error in .intersectExtent(x, y, validate = TRUE) : 
  #Objects do not intersect" 
#everything else worked 

#~EXTENTS OF FILES (RESAMPLING)~

#PET1= 112.875, 153.975, -43.775, -9.025
#NDVI1= 110, 155.0833, -45.08333, -9 
#precip1=112.875, 153.975, -43.775, -9.025

#new_PET= 110, 155.0833, -45.08333, -9
#new_precipitation= 110, 155.0833, -45.08333, -9


#resampling for precip and PET

 new_precipitation <- resample(precip1, ndvi1) 

  new_PET <- resample(PET1, ndvi1)
 
 #creating mean vectors of layers
                                        #MEANPRECIP
    meanprecip <- vector(length = nlayers(new_precipitation))
  
  for(k in 1: nlayers(new_precipitation)) {
    meanprecip[k] <- mean(values(new_precipitation[[k]]), na.rm=TRUE)
  }
 
                                       #MEANPET 
  meanPET <- vector(length = nlayers(new_PET))
  
  for(k in 1: nlayers(new_PET)) {
    meanPET[k] <- mean(values(new_PET[[k]]), na.rm=TRUE)
  }
  
                                      #MEANNDVI
  meanndvi1 <- vector(length = nlayers(ndvi1))
  for(k in 1: nlayers(ndvi1)) {
    meanndvi1[k] <- mean(values(ndvi1[[k]]), na.rm=TRUE)
  }
 mean2ndvi<- mean(meanndvi1)
  #0.3674486
 new_mean_precip <- meanprecip[13:41] 
#log all data
 
 qmeanprecip <- log(new_mean_precip)
 qmeanndvi <- log(meanndvi1)
 qmeanPET <- log(meanPET)
                 
#lm 
 fit1.lm <- lm (qmeanndvi~qmeanprecip)
 fit2.lm <- lm(ndvi_lessPET~qmeanprecip) 
 fit3.lm <- lm(qmeanndvi~qmeanPET)
 fit5.lm <- lm(ndvi_lessprecip~qmeanPET) 
 summary(fit5.lm)$r.squared
 
 
 # PET vs NDVI
fit3.lm <- lm(qmeanndvi~qmeanPET)
plot(qmeanPET, qmeanndvi)
resid_PET <- resid(fit3.lm)
summary(fit3.lm)$r.squared

ndvi_lessPET<- (mean2ndvi+resid_PET)
  
  #plot raninfall against ndvi without effects of PET 
  
new_mean_precip <- meanprecip[13:41] 
fit2.lm <- lm(ndvi_lessPET~new_mean_precip) 
plot(new_mean_precip, ndvi_lessPET) 
  abline(fit2.lm) 
  summary(fit2.lm)$r.squared
summary(fit3.lm)$r.squared 

              # PRECIPITATION VS NDVI 
new_mean_precip <- meanprecip[13:41] 
logprecip <- log(new_mean_precip)

fit1.lm <- lm (qmeanndvi~qmeanprecip) 
plot(qmeanprecip, qmeanndvi) 
abline(fit1.lm) 
resid_precip <- residuals(fit1.lm)
summary(fit1.lm)$r.squared



#try again with removing rainfall
mean2ndvi<- mean(qmeanndvi)
#0.3674486

             # PRECIP vs NDVI residuals
fit1.lm <- lm(qmeanndvi~qmeanprecip)
plot(qmeanprecip, qmeanndvi)
resid_precip <- residuals(fit1.lm)
summary(fit1.lm)$r.squared

ndvi_lessprecip <- (mean2ndvi+resid_precip)

#plot raninfall against ndvi without effects of PET 
fit5.lm <- lm(ndvi_lessprecip~qmeanPET) 
plot(qmeanPET, ndvi_lessprecip) 
abline(fit5.lm) 
summary(fit1.lm)$r.squared 
summary(fit2.lm)$r.squared
summary(fit3.lm)$r.squared
summary(fit5.lm)$r.squared 

fit1.lm <- lm (meanndvi1~new_mean_precip)
plot(fit1.lm)

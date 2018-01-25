rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented) 
library(stats)
library(graphics)
file.evi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/EVI/MONTHLYEVI.nc"
file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/EVI/MONTHLYPRECIPEVI.nc"

evi <- brick(file.evi) 
prec <-brick(file.precip)

annual_prec <- brick()
years <- nlayers(prec)/12
for (k in 1: years) {
  annual <- sum(prec[[(k*12-11):(k*12)]])
  annual_prec <- addLayer(annual_prec, annual)
}
meanprecip <- mean(annual_prec)
source("~/Desktop/scripts/add_raster_legend.R")
source("~/Desktop/scripts/cor_function_perc~dir.R")
#----------------------------------------------------------------------------------------------

#create a function with monPrec+monNdvi

x_data <- prec

y_data <- evi  

#double check x and y have same amount of layers 

combine_data <- x_data
combine_data <- addLayer(combine_data, y_data)
#----------------------------------------------------------------------------------------------

lagsevinormal <- calc(combine_data, fun=cor_lagged)

lagseviwet<- calc(combine_data, fun=function(x) cor_lagged(x, perc=0.7, dir= "above"))

lagsevidry<- calc(combine_data, fun=function(x) cor_lagged(x, perc=0.3, dir= "below"))

#----------------------------------------------------------------------------------------------
# LAG correlations 
#mask wet and dry
a<-lagseviwet[[3]]
b<-lagsevinormal[[3]]
c<-lagsevidry[[3]]
#----------------------------------------------------------------------------------------------

meanprecip <- mean(annual_prec)
meanprecip[is.na(a)] <- NA
a[is.na(meanprecip)] <- NA

a1<-as.vector(meanprecip)
a2<-as.vector(a)

z1<-cor.test(a1,a2)
print(z1) 
#----------------------------------------------------------------------------------------------

meanprecip <- mean(annual_prec)
meanprecip[is.na(b)] <- NA
b[is.na(meanprecip)] <- NA

b1<-as.vector(meanprecip)
b2<-as.vector(b)

z2<-cor.test(b1,b2)
print(z2) 
#----------------------------------------------------------------------------------------------

meanprecip <- mean(annual_prec)
meanprecip[is.na(c)] <- NA
c[is.na(meanprecip)] <- NA

c1<-as.vector(meanprecip)
c2<-as.vector(c)

z3<-cor.test(c1,c2)
print(z3) 
#----------------------------------------------------------------------------------------------

pdf("lag_correlation/multilags/evilags.pdf")
par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
plot(lagsevidry[[3]], main="Dry EVI Lag")
plot(lagsevinormal[[3]], main="Normal EVI Lag") 
plot(lagseviwet[[3]], main="Wet EVI Lag")
dev.off()

z1$estimate
z2$estimate
z3$estimate


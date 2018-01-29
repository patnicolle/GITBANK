#DELTA VOD

rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")
library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)

file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/VOD/ANUCLIM_precip_monthly_1993_2012_VOD_resolution.nc"
file.vod <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/VOD/Australia_VOD_monthly_1993_2012.nc"

data <- brick(file.precip) #or wetprecip/aridprecip
#data2 <- brick(file.ndvi) #or wetav/aridav

data2 <- brick(file.vod)
#data <- aridprecip
#data2 <- aridav


deltaVOD <- brick()
for (k in 1:(nlayers(data2)-2)) {
  r <- mean(data2[[k+1]], data2[[k+2]])- data2[[k]]
  deltaVOD <- addLayer(deltaVOD, r)
}



P <- data 
P <- P[[1:nlayers(deltaVOD)]]
#mask .na
P[P<1] <- NA
P[deltaVOD<0] <- NA
deltaVOD[deltaVOD<0] <- NA 
deltaVOD[is.na(P)] <- NA
P[is.na(deltaVOD)] <- NA



#plot first few layers 

#plot(P[[1]], deltaNDVI[[1]])

#p1 <- as.vector(P[[1]])
#d1 <- as.vector(deltaNDVI[[1]])
#lm1.lm <- lm(d1~p1)
#abline(lm1.lm, col=3)


x <- values(P)
y <- values(deltaVOD)

ind <- which(is.na(y))

y <- y[-ind]
x <- x[-ind] 

lmtotal.lm <- lm(y~x)

summary(lmtotal.lm)$r.squared

#png("A_NEW_PLOT/deltaNDVI_precip_full.png")
#plot(x,y, cex=0.5)
#abline(lmtotal.lm, col=3)
#dev.off()

seqr<- seq(from = 0,to = 2975, by = 25)

percentile <- matrix(data=NA, nrow = nlayers(deltaVOD), ncol = length(seqr))
for (b in 1: nlayers(deltaVOD)) {
  o <- values(deltaVOD[[b]])
  precip <- values(P[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentile[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}

precipins<- matrix(data=seqr, nrow = nlayers(deltaVOD), ncol = length(seqr), byrow = TRUE)

y2 <- as.vector(percentile)
x2 <- as.vector(precipins)

pdf('A_NEW_PLOT/deltaVOD~Rainfall~lm.pdf')
abc.lm <- lm(y2~x2)
a<-summary(abc.lm)$r.squared
a <- format(round(a, 2), nsmall = 2)
plot(x2, y2, xlim=c(0,1500), ylim= c(0,0.5))
legend("topright", bty="n", legend=paste("R2 =",format(summary(abc.lm)$adj.r.squared, digits=4)))
abline(lm(y2~x2), col=3)
dev.off()










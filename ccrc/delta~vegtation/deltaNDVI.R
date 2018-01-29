rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")

library(sp)
library(raster) 
library(SPAr) 
library(ncdf4) 
library(RNetCDF) 
library(segmented)

file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/ANUCLIM_precipitation_1982_2008_NDVI_resolution.nc"
file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc" 

data <- brick(file.precip)
data2 <- brick(file.ndvi)



deltaNDVI <- brick()
for (k in 1:(nlayers(data2)-2)) {
  r <- mean(data2[[k+1]], data2[[k+2]])- data2[[k]]
  deltaNDVI <- addLayer(deltaNDVI, r)
}

deltaNDVI <- deltaNDVI[[1:nlayers(data)]]

P <- data 

#mask .na
P[P<1] <- NA
P[deltaNDVI<0] <- NA
deltaNDVI[deltaNDVI<0] <- NA 
deltaNDVI[is.na(P)] <- NA
P[is.na(deltaNDVI)] <- NA



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

#png("A_NEW_PLOT/deltaNDVI_precip_full.png")
#plot(x,y, cex=0.5)
#abline(lmtotal.lm, col=3)
#dev.off()

seqr<- seq(from = 0,to = 2975, by = 25)

percentile <- matrix(data=NA, nrow = nlayers(deltaNDVI), ncol = length(seqr))
for (b in 1: nlayers(deltaNDVI)) {
  o <- values(deltaNDVI[[b]])
  precip <- values(P[[b]])
  
  for (k in 1: length(seqr)) {
    
    ind <- which(precip>= seqr[k] & precip< seqr[k]+25) 
    a <- o[ind] 
    percentile[b,k] <- quantile(a, probs=c(0.5), na.rm= TRUE)
    
  } 
}

precipins<- matrix(data=seqr, nrow = nlayers(deltaNDVI), ncol = length(seqr), byrow = TRUE)

y2 <- as.vector(percentile)
x2 <- as.vector(precipins)

pdf('A_NEW_PLOT/deltaNDVI~Rainfall~lm.pdf')
abc.lm <- lm(y2~x2)
a<-summary(abc.lm)$r.squared
a <- format(round(a, 2), nsmall = 2)
plot(x2, y2, xlim=c(0,1500), ylim= c(0,0.5))
legend("topright", bty="n", legend=paste("R2 =",format(summary(abc.lm)$adj.r.squared, digits=4)))
abline(lm(y2~x2), col=3)
dev.off()







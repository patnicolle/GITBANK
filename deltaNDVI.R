rm(list=ls(all=TRUE))
setwd("/Volumes/P_Harddrive/")

file.precip <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Precipitation/NDVI/ANUCLIM_precip_bimonthly_1982_2014_GIMMS_resolution.nc"
file.ndvi <- "/Volumes/P_Harddrive/LAI_precip_variability/Data/Vegetation_indices/NDVI/Australia_NDVI3g_bimonthly_1982_2015.nc" 

data <- brick(file.precip)
data2 <- brick(file.ndvi)
data2 <- data2[[1:792]] 

P <- data 
P[P<1] <- NA
P <- P[[2:792]]




deltaNDVI <- brick()
for (k in 1:(nlayers(data2)-1)) {
  r = (data2[[k+1]])-(data2[[k]])
  deltaNDVI <- addLayer(deltaNDVI, r)
}

P[deltaNDVI<0] <- NA
deltaNDVI[deltaNDVI<0] <- NA 
deltaNDVI[is.na(P)] <- NA

x <- as.vector(P)
y <- as.vector(deltaNDVI)
lmtotal.lm <- lm(y~x)

pdf("deltaNDVI~precip.pdf")
plot(x,y)
abline(lmtotal.lm, col=3)
dev.off()



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

precipins<- matrix(data=seqr, nrow = nlayers(evi), ncol = length(seqr), byrow = TRUE)

x2 <- as.vector(percentile)
y2 <- as.vector(precipins)
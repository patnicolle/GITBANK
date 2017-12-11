#sensitivity coefficients NDVI 

# σ NDVI(PET) = δln(NDVI)/ δln(PET)  

p1 <- lnNDVI[1:468]
p2 <- lnC02 






lm(lnC02~lnNDVI[1:468])
plot(p1,p2)
abline(6.065, 0.2062)
#as CO2 increases as does ndvi 





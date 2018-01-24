dryyearsrain <- function(x_data){
  
  annual_prec <- brick()
  years <- nlayers(x_data)/12
  for (k in 1: years) {
    annual <- mean(x_data[[(k*12-11):(k*12)]])
    annual_prec <- addLayer(annual_prec, annual)
  }
  
  quantiles<- calc(annual_prec, fun=function(x) quantile(x, probs=0.3), na.rm=TRUE)
  
  dryrain <- brick()
  for (i in 1:nlayers(annual_prec)) {
    
    ind <- Which(annual_prec[[i]]<quantiles[[2]])
    a <- x_data[[((i*12)-11):(i*12)]]
    ind[ind<1]<- NA
    a[is.na(ind)] <- NA
    dryrain <- stack(dryrain, a)
    
    
  }
  return(dryrain)
}



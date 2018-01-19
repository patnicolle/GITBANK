cor_lagged <- function(data_vec,   lag.max=6){
  
  
  len <- length(data_vec)
  
  #Get x and y data
  x_data <- data_vec[1 : (len/2)]
  y_data <- data_vec[(len/2 + 1) : len]
  
  #All missing, return NA
  if(all(is.na(x_data)) | all(is.na(y_data))) {
    return(c(NA,NA))
    
    #Data available, perform linear regression
  } else {
    
    cor <- tryCatch(ccf(x=x_data, y=y_data, lag.max=lag.max, na.action=na.pass, plot=FALSE),  error = function(e) NULL)
    
    if(is.null(cor)){
      
      return(c(NA,NA))
      
    } else {
      
      lags <- seq(lag.max * -1, 0,  by=1)
      
      #Only save negative and zero lag coeffs
      cor_coefs <- cor$acf[1:(lag.max+1)]
      
      #Get slope and p-value
      max_val  <- max(cor_coefs)
      ind <- which(cor_coefs==max_val)
      
      if(!is.numeric(ind) | length(ind)!=1) return(c(NA,NA))
      
      
      coef <- cor_coefs[ind]
      
      max_lag <- lags[ind]
    }
    
  }
  
  return(c(coef, max_lag))
}
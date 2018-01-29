#

slope_per_pixel <- function(data_vec){
  
  len <- length(data_vec)
  
  #Get x and y data
  x_data <- data_vec[1 : (len/2)]
  y_data <- data_vec[(len/2 + 1) : len]
  
  #All missing, return NA
  if(all(is.na(x_data)) | all(is.na(y_data))) {
    return(c(NA,NA))
    
    
    #Data available, perform linear regression
  } else {
    
    lm <- lm(y_data ~ x_data)
    
    #Get slope and p-value
    coef <- lm$coefficients[2]
    
    pval <- summary(lm)$coefficients[8]
    
    return(c(coef,pval))
    
  }
}





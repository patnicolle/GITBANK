

#Define function to calculate slope and p-value
cor_per_pixel <- function(data_vec){
  
  len <- length(data_vec)
  
  #Get x and y data
  x_data <- data_vec[1 : (len/2)]
  y_data <- data_vec[(len/2 + 1) : len]
  
  #All missing, return NA
  if(all(is.na(x_data)) | all(is.na(y_data))) {
    return(c(NA,NA))
    
    
    #Data available, perform linear regression
  } else {
    
    cor <- tryCatch(cor.test(x=x_data, y=y_data),  error = function(e) NULL)
    
    if(is.null(cor)){
      
      return(c(NA,NA))
      
    } else {
      
      #Get slope and p-value
      coef <- cor$estimate
      
      pval <- cor$p.value
    
    return(c(coef,pval))
    
  }
  }
}



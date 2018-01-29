cor_test_with_lag <- function(x, y, lag){
  
  if(lag > length(x)) stop("lag too large for time series")
  
  x <- x[1 : (length(x) - lag)]
  y <- y[(1+lag) : length(y)]
  
  corr <- tryCatch(cor.test(x, y, na.action=na.pass),
                   error = function(e) NULL)
  
  if(is.null(corr)){
    
    return(c(NA,NA))
    
  } else {
    
    return(c(corr$estimate, corr$p.value))
    
  }
  
}


#Main function
cor_lags <- function(data_vec,  lag.max=6){
  
  
  len <- length(data_vec)
  
  #Get x and y data
  x_data <- data_vec[1 : (len/2)]
  y_data <- data_vec[(len/2 + 1) : len]
  
  
  #All missing, return NA
  if(all(is.na(x_data)) | all(is.na(y_data))) {
    return(c(coef = NA,
             pval = NA,
             lag = NA))
    
    
    #Data available, calculate correlation
  } else {
    
    
    #Set lags
    lags <- 0:lag.max
    
    
    #Calculate lagged correlations
    correl <- sapply(lags, function(x) cor_test_with_lag(x_data, y_data, x))
    
    
    #Determine maximum lag
    ind <- which.max(correl[1,])
    
    
    if(length(ind) != 1 | !is.numeric(ind)) {
      
      outs <- c(coef = NA,
                pval = NA,
                lag = NA)
      
    } else {
      
      #Create output vector
      outs <- c(coef = correl[1, ind],
                pval = correl[2, ind],
                lag = lags[ind])
    }
  }
  
  return(outs)
}
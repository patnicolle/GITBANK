



subset_data_by_annual <- function(x_data, perc, dir){
  
  
  #Average to annual
  ind_start <- seq(1, by=12, length.out = length(x_data) / 12)
  
  
  annual_x <- sapply(ind_start, function(x) mean(x_data[x:(x+11)]))
  
  
  #Find which years below/above percentile
  
  perc_value <- quantile(annual_x, probs=perc)
  
  
  if(dir == "above"){
    
    ind <- which(annual_x >= perc_value)
    
  } else if (dir == "below"){
    
    ind <- which(annual_x <= perc_value)
    
  } else {
    
    stop("Direction not set correctly, must be one of 'above' or 'below'")
    
  }
  
  #Extract years for threshold
  yr_starts <- ind_start[ind]
  
  #Create sequence for monthly values
  all_inds <- as.vector(sapply(yr_starts, function(x) x:(x+11)))
  
  
  return(all_inds)
}



#------------------------------------------------------------------------
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

#------------------------------------------------------------------------

cor_lagged <- function(data_vec,  lag.max=6, perc=NA, dir=NA){
  
  
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
    
    
    #Subset data
    if(!is.na(perc)){
      
      #Check that provided monthly inputs
      if( length(x_data) %% 12 != 0){
        stop(paste("Subsetting assumes monthly inputs are provided, please amend.", 
                   "Length =", length(x_data)))
      }
      
      inds <- subset_data_by_annual(x_data, perc, dir)
      
      x_data <- x_data[inds]
      y_data <- y_data[inds]
      
    }
    
    
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



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



files <- list.files(path = "Coefficient_optimization/Multi_step_predictions/Job_splitting/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
L <- lapply(paste("Coefficient_optimization/Multi_step_predictions/Job_splitting/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)
       


orderList <- function(x){
  values <- vector()
  n <- length(x)
  for (i in (1:n)){
    temp <- unlist(x[[i]]$value)
    if (is.null(temp) || (is.nan(temp))){
      values[i] <- NA
    }
    else{
      values[i] <- as.numeric(temp) ## Make this the other parameter we want to minimize
    }
  }
  orderedList <- x[order(values, decreasing = TRUE)]
  return(orderedList)
}


Lnew <- orderList(L)

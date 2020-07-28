
meta_optim=function(order){
  templist <- list()
  uRoot_test <- function(order, pars){
    # Check abs(polyroot) of AR, MA and Regressive terms to force stationarity.
    p <- order[1]
    d <- order[2]
    q <- order[3]
    if (p > 0){
      AR <- pars[1:p]
      AR_flag <- !all(abs(polyroot(c(1,-1*AR))) > 1) ## If AR_flag is 1, the AR polynominal roots are not stationary
    }else{AR_flag = FALSE}
    if (q > 0){
      MA <- pars[((p+1):(p+q))]
      MA_flag <- !all(abs(polyroot(c(1, MA))) > 1) ## If MA_flag is 1, the MA polynominals are non-invertible
    }else{MA_flag = FALSE}
    
    if (AR_flag || MA_flag){
      #print(pars)
      #print("Inf returned due to flag")
      return(1) ## return 1 if roots are less than 1
    }else{return(0)}
  }
  #This function is supposed to minimze sum of squares, which will be the same as minimizing mse (sum of squares gives error)
  compute_arime_mse=function(pars,order){
    
    uRoot <- uRoot_test(order, pars)
    if (uRoot == 1){
      #print("Unit root")
      return(Inf)
    }
    
    model=arima(x=training, xreg = regressors, order = order, fixed = pars)
    mse=sum((model$residuals[wwIndex == 1])^2) 
    return(mse) 
  }
  
  n <- order[1]+order[3]
  
  if (order[2] == 0){n <- n+1}
  
  if (!is.null(regressors)){ n <- n+1 }
  
  if (n>0){
    counter <- 1
    max_counter <- 5
    results_list <- list()
    startpars=  vector(length = n)
    while (counter <= max_counter){
      uRoot <- 1
      while (uRoot == 1){
        startpars <- rnorm(n = n, mean = 0, sd = sample(1:2, 1))
        uRoot <- uRoot_test(order, startpars)
      }
      results = try(optim(par = startpars, fn = compute_arime_mse, order=order, method = "Nelder-Mead", control = list(maxit = 1000)), silent = TRUE)
      if ((is(results, 'try-error')) || (results$convergence != 0))
      {
        results_list[[counter]] <- results
        counter = counter + 1
      } else
        if ((uRoot_test(order, results$par) == 1) || (results$convergence != 0)){
          results_list[[counter]] <- results
          counter = counter + 1
        } else{ break; }
    }
    if (counter >= max_counter){
      min_value <- min(sapply(1:max_counter, function (i) { results_list[[i]]$value }))
      where_min_value <-  which(sapply(1:max_counter, function (i) { results_list[[i]]$value }) == min_value)[1]
      
      
      list_return <- c(list(order = order), 
                       results_list[[where_min_value]],
                       tries = counter)
      list_return$message <- paste((max_counter)," tries: failed to converge")
      
      templist <- list_return
    } else{
      templist <- list(order = order, 
                       par = results$par, 
                       value = results$value, 
                       counts = results$counts, 
                       convergence = results$convergence,
                       message = results$message,
                       tries = counter)
    }
  } else{      
    templist <- list(order = order, message = "No parameters to optimize for")
  }
  return(templist)
}



## In compare code, mcss seems to optimize sligtly better, for some reason.
## Thus we will use that function to optimize for and then return ml SC ml value


meta_optim <- function(order, External_Regressor, reg.nr, reg.lag){
  source(paste(getwd(), "/Coefficient_optimization/dds_optimization/ddsoptim.r", sep = ""))
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
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
    if (!is.null(reg.nr)){
      reg <- pars[(p+q+2-d):length(pars)]
      reg_flag <-  !all(abs(polyroot(c(1, -1*reg))) > 1)
    }else{reg_flag = FALSE}
    
    if (AR_flag || MA_flag || reg_flag){
      #print(pars)
      #print("Inf returned due to flag")
      return(1) ## return 1 if roots are less than 1
    }else{return(0)}
  }
  multistepARIMA <- function(ts, order, coefficients, Xreg){
    step1 <- function(ts, step1res, Xreg){
      
      ######## If mean is not zero
      if (mean(ts, na.rm = T) > tol){
        ts <- ts - mean(ts, na.rm = TRUE)
      }
      X <- matrix(ncol = (p + q + 1), ## Number of terms
                  nrow = length(observed)
      )
      
      if ((p > 0) & (q > 0)){
        for (i in ((r+nstep+d):length(observed))){ ## i is the n we are predicting, we need to add d
          y <- ts[(i - 1):(i - p)]
          eps <- step1res[(i - 1):(i-q)]
          X[i, ] <- c(1, y, eps) #X[(i-r-d), ]
        }
      } else if ((p > 0) & (q == 0)){
        for (i in ((r+nstep+d):(length(observed)))){ ## i is the n we are predicting, we need to add d
          y <- ts[(i - 1):(i - p)]
          X[i, ] <- c(1, y)
        }
      } else if ((p == 0) & (q > 0)){
        for (i in ((r+nstep+d):(length(observed)))){ ## i is the n we are predicting, we need to add d
          eps <- step1res[(i - 1):(i-q)]
          X[i, ] <- c(1, eps)
        }
      } else{
        X[] <- 1
      }
      
      
      if (!is.null(Xreg)){
        X <- cbind(X, Regressor)
        X[(1:(reg.lag.num + q)), ] <- NA
      }
      
      pred <- X %*% delta
      
      if (d > 0){
        pred_ <- pred
        pred <- pred + c(NA, observed[1:(length(observed)-1)])
        res <- observed - pred
      } else{
        res <- observed - pred
        pred_ <- NULL
      }
      
      return(list(pred, 
                  res,
                  X,
                  pred_))
    }
    step_n <- function(priorPred, priorRes, nstep, X, Xreg, priorPred_nosubtrack){## might not need ts or step1res
      
      ######## If mean is not zero
      if (abs(mean(priorPred, na.rm = TRUE)) > tol){
        priorPred <- priorPred - mean(priorPred, na.rm = TRUE)
      }
      
      if (nstep == 2){
        priorPred <-append(NA, priorPred[(1):(length(priorPred)-1)])
        priorRes <- rep(0, length(priorPred)) 
        
        
        if (p > 0){
          X[,2] <- priorPred
          #X[temp, ] <- NA #Not sure wht this is supposed to do 
        }
        if (q > 0){
          X[,2+p] <- priorRes
        }
        
      }
      
      else if (nstep > 2){
        
        if ((p > 0) && (q > 0)){
          P <- as.matrix(X[,(2:(p+1))])
          Q <- as.matrix(X[,((p+2):(ncol(X)-length(REG)))])
          
          P <- rbind(rep(NA, ncol(P)), P)
          Q <- rbind(rep(NA, ncol(Q)), Q)
        } else if ((p == 0) && (q > 0)){
          P <- NULL
          Q <- as.matrix(X[,((p+2):(ncol(X)-length(REG)))])
          Q <- rbind(rep(NA, ncol(Q)), Q)
        } else if ((p > 0) && (q == 0)){
          P <- as.matrix(X[,(2:(p+1))])        
          P <- rbind(rep(NA, ncol(P)), P)
          Q <- NULL
        }
        
        
        if (p >= 1){
          priorPred <- priorPred[1:(length(priorPred)-1)]
          priorPred <- append(NA, priorPred)
          if (p == 1){
            P1 <- NULL
            P2 <- NULL
          } else if (nstep >= p){
            P1 <- as.matrix(P[(1:(nrow(P)-1)),(1:(p-1))]) 
            P2 <- NULL
          } else {
            P1 <- as.matrix(P[(1:(nrow(P)-1)),(1:(nstep-2))]) ## This will be shifted
            P2 <- as.matrix(P[2:nrow(P), (nstep-2+1):(ncol(P)-1)])
          }
        } else{
          P1 <- NULL
          P2 <- NULL
          priorPred <- NULL
        }
        
        if (q >= 1){
          priorRes <- rep(0, nrow(X))#priorRes[1:(length(priorRes)-1)]
          #priorRes[1:(r+nstep-1+d)] <- NA
          if (q == 1){
            Q1 <- NULL
            Q2 <- NULL
          } else if (nstep >= q){
            Q1 <- as.matrix(Q[(1:(nrow(Q)-1)),1:(q - 1)]) ## This will be shifted
            Q2 <- NULL
          } else{
            Q1 <- as.matrix(Q[(1:(nrow(Q)-1)),(1:(nstep-2))]) ## This will be shifted
            Q2 <- as.matrix(Q[2:nrow(Q), (nstep-2+1):(ncol(Q)-1)])
          } 
        } else{
          Q1 <- NULL
          Q2 <- NULL
          priorRes <- NULL
        }
        
        
        X <- cbind(1,
                   priorPred,
                   P1,
                   P2,
                   priorRes,
                   Q1,
                   Q2,
                   Regressor)
      }
      
      if (is.null(Xreg)){
        X[1:(nstep + r - 1 + d), ] <- NA
      } else{
        X[1:(reg.lag.num + q + nstep-1), ] <- NA
      }
      
      pred <- X %*% delta
      
      if (d > 0){
        pred_ <- pred
        pred <- pred + c(NA, priorPred_nosubtrack[1:(length(priorPred_nosubtrack)-1)])
        res <- observed-pred
      } else{
        res <- observed-pred
        pred_ <- NULL
      }
      
      return(list(pred, 
                  res,
                  X, 
                  pred_))
    }
    
    observed <- ts
    tol <- 1e-4
    p <- order[1]
    q <- order[3]
    d <- order[2]
    r <- max(p, q)
    if (is.null(Xreg)){
      model <- arima(x = ts, order = order, fixed = coefficients)
      REG <- NULL
      Regressor <- NULL
    }else{
      model <- arima(x = ts, order = order, fixed = coefficients, xreg = Xreg)
      if (d ==0){
        REG <- coefficients[(p+q+d+2):length(coefficients)]
      }else 
        if (d == 1){
          REG <- coefficients[(p+q+d):length(coefficients)]
        }
      Regressor <- as.matrix(Xreg)
      reg.lag.num <- which(diff(is.na(Xreg[,ncol(Xreg)])) == -1)[1]
    }
    step1res <- residuals(model)
    
    if ((p > 0) && (q > 0)){
      AR <- coefficients[1:p]
      MA <- coefficients[(p+1) : (p  + q)]
    }else if ((p == 0) && (q > 0)){
      AR <- NULL
      MA <- coefficients[(p+1) : (q  + p)]
    }else if ((p > 0) && (q == 0)){
      AR <- coefficients[1:p]
      MA <- NULL
    }else{
      AR <- NULL
      MA <- NULL
      print("No parameters to optimize for")
      exit()
    }
    if (d > 0){  
      I <-  0
    } else{
      I <- as.numeric(coefficients[(p + q + 1)])
    }
    
    delta <- as.matrix(c(interc = I, AR, MA, REG))
    
    ## 1-step prediction and getting treated time series and delta
    nstep <- 1
    output <- step1(ts, step1res, Xreg)
    pred <- unlist(output[1])
    res <- unlist(output[2])
    X <- matrix(unlist(output[3]),   ncol = (p + q + 1 + length(REG)), nrow = length(ts))
    if (d > 0){ pred_ <- unlist(output[4]) } ## pred_ is used for iteration when d > 0
    res_list<- list()
    res_list[[1]] <- res
    pred_list <- list()
    pred_list[[1]] <- pred
    
    for (i in 2:10){
      nstep <- i
      if (all(is.nan(pred))){ 
        print("Model returns NaN, model is not appropriate for data")
        quit()}
      if (d > 0){ 
        output <- step_n(priorPred = pred_, priorRes = res, nstep, X, Xreg, priorPred_nosubtrack = pred) 
        pred_ <- unlist(output[4])
      } else{ 
        output <- step_n(priorPred = pred, priorRes = res, nstep, X, Xreg, priorPred_nosubtrack = NULL)
      }
      pred <- unlist(output[1])
      res <- unlist(output[2])
      X <- matrix(unlist(output[3]),   ncol = (p + q + 1 + length(REG)), nrow = length(observed))
      
      res_list[[i]] <- res
      pred_list[[i]] <- pred
    }
    
    return(list(residuals = res_list, predictions = pred_list))
  }
  optimize_arima=function(pars,order, Xreg, reg.nr){
    
    #print(pars)
    uRoot <- uRoot_test(order, pars)
    if (uRoot == 1){
      #print("Unit root")
      return(Inf)
    }
    
    output <- multistepARIMA(ts = ts, order = order, coefficients = pars, Xreg = Xreg)
    residuals <- matrix(unlist(output$residuals), ncol = length(unlist(output$residuals))[1], nrow = 10)
    k <- 10
    temp1 <- 0
    for (j in 1:k){ temp1 <- temp1 + k - j + 1 }
    SC_ <- vector()
    for (i in 1:length(output$residuals[[1]])){
      temp2 <- 0
      for (j in 1:k){
        temp2 <-  temp2 + ((k - j + 1) * residuals[j,i])
      }
      SC_[i] <-  temp2
    }
    
    SC <- (1/(temp1)) * SC_
    SC <-  sum(SC[wwIndex == 1]^2, na.rm = T) #
    return(SC) 
  }
  templist <- list()
  n <- order[1]+order[3]
  
  if (External_Regressor == FALSE){
    reg.nr <- NULL
    reg.lag <- NULL
    reg <- NULL
  } else
    if (External_Regressor == TRUE){
      if (missing(reg.nr) || missing(reg.lag)){
        print("If you want regressor, please provide reg.nr and reg.lag")
        exit()
      }
      ## here we make reg
      if ((!is.null(reg.lag)) || (!is.null(reg.nr))){
        reg = as.vector(1:reg.nr)
        reg <- unlist(sapply(reg, function(i){
          idx <- i - 1
          if (idx == 0){
            Regressor
          }
          else{
            c(rep(NA, idx), head(Regressor, -idx))
          }
        }))
        if (reg.nr > 1){reg <- rbind(matrix(NA, nrow = reg.lag, ncol = ncol(reg)) ,reg[1:(nrow(reg)-reg.lag),])}
        else if (reg.nr == 1){reg <- c(rep(NA, reg.lag) ,reg[1:(nrow(reg)-reg.lag),])}
        else if (reg.nr < 1){
          print("Reg.nr needs to be more than 1 if an external regressor is wanted")
          exit()}
      }
      n = n + reg.nr
    }
  
  if (order[2] == 0){n <- n+1}
  
  if ((order[1] > 0) || (order[3] > 0)){
    counter <- 1
    max_counter <- 5
    results_list <- list()
    startpars <- vector(length = n)
    while (counter <= max_counter){
      uRoot <- 1
      while (uRoot == 1){
        startpars <- rnorm(n = n, mean = 0, sd = sample(1:2, 1))
        uRoot <- uRoot_test(order, startpars)
      }
      ##use dds to optimize
      ## Dds does not give any information on whether it has converged
      results <- try(ddsoptim(f = optimize_arima, start = startpars, order=order, reg.nr = reg.nr, Xreg = reg, xmax = rep(5, length(startpars)), xmin = rep(-5, length(startpars))), silent = TRUE)
      
      if (is(results, 'try-error'))
      {
        results_list[[counter]] <- results
        counter = counter + 1
      } else
        if (uRoot_test(order, results$par) == 1){
          results_list[[counter]] <- results
          counter = counter + 1
        }else{ break;}
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
                       #counts = results$counts, 
                       #convergence = results$convergence,
                       #message = results$message,
                       tries = counter)
    }
  } else{      
    templist <- list(order = order, message = "No parameters to optimize for")
  }
  return(templist)
}

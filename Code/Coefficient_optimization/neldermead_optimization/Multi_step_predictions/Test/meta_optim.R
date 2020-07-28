
meta_optim=function(orders, ARIMAX, reg.nr, reg.lag){
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  compute_arima_mse=function(pars,order, external_regressor, reg.nr){
    multistepARIMA <- function(order, coefficients, external_regressor){
      step1 <- function(ts, step1res, external_regressor){
        
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
        
        
        if (!missing(external_regressor)){
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
      step_n <- function(priorPred, priorRes, nstep, X, external_regressor, priorPred_nosubtrack){## might not need ts or step1res
        
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
        
        if (missing(external_regressor)){
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
      
      tol <- 1e-4
      p <- order[1]
      q <- order[3]
      d <- order[2]
      r <- max(p, q)
      if (missing(external_regressor)){
        model <- arima(x = ts, order = order, fixed = coefficients)
        REG <- NULL
        Regressor <- NULL
      }else{
        model <- arima(x = ts,order = order, fixed = coefficients, xreg = external_regressor)
        if (d ==0){
          REG <- coefficients[(p+q+d+2):length(coefficients)]
        }else 
          if (d == 1){
            REG <- coefficients[(p+q+d):length(coefficients)]
          }
        Regressor <- as.matrix(external_regressor)
        reg.lag.num <- which(diff(is.na(Regressor[,ncol(Regressor)])) == -1)[1]
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
      output <- step1(ts, step1res, external_regressor)
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
          output <- step_n(priorPred = pred_, priorRes = res, nstep, X, external_regressor, priorPred_nosubtrack = pred) 
          pred_ <- unlist(output[4])
        } else{ 
          output <- step_n(priorPred = pred, priorRes = res, nstep, X, external_regressor, priorPred_nosubtrack = NULL)
        }
        pred <- unlist(output[1])
        res <- unlist(output[2])
        X <- matrix(unlist(output[3]),   ncol = (p + q + 1 + length(REG)), nrow = length(observed))
        
        res_list[[i]] <- res
        pred_list[[i]] <- pred
      }
      
      return(list(residuals = res_list, predictions = pred_list))
    }
  
    #print(pars)
    ## Check abs(polyroot) of AR, MA and Regressive terms to force stationarity.
    
    p <- order[1]
    d <- order[2]
    q <- order[3]
    if (p > 0){
      AR <- pars[1:p]
      AR_flag <- !all(abs(polyroot(AR)) > 1)
    }
    if (q > 0){
      MA <- pars[((p+1):(p+q))]
      MA_flag <- !all(abs(polyroot(MA)) > 1)
    }
    if (!missing(reg.nr)){
      reg <- pars[(p+q+2-d):length(pars)]
      reg_flag <-  !all(abs(polyroot(reg)) > 1)
    }
    
    if (AR_flag || MA_flag || reg_flag){
      return(Inf)
    }
    
    
    output <- multistepARIMA(order = order, coefficients = pars, external_regressor = external_regressor)
    x <- matrix(unlist(output$residuals), ncol = length(unlist(output$residuals))[1], nrow = 10)
    k <- 10
    under <- 0
    for (j in 1:k){ under <- under + k - j + 1 }
    e <- vector()
    
    
    for (i in 1:19781){
      upper <- 0
      for (j in 1:k){
        upper <- upper + ((k - j + 1) * x[j,i] )
      }
      e[i] <- upper
    }
    
    SC <-  sum(e[wwIndex == 1]^2, na.rm = T) #
    return(SC) 
  }
  
  
  templist <- list()
  n <- orders[1]+orders[3]

  if (ARIMAX == FALSE){
    reg.nr <- NULL
    reg.lag <- NULL
    reg <- NULL
  } else
  if (ARIMAX == TRUE){
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
  
  if (orders[2] == 0){n <- n+1}
  
  if (n>0){
    # startpars <- vector(length = n)
    # sd <- sample(2:4, 1)
    # startpars <- rnorm(n = n, mean = 0, sd = sd)
    # results = optim(par = startpars, fn = compute_arima_mse, order=orders, reg.nr = reg.nr, external_regressor = reg, method = "Nelder-Mead", control = list(maxit = 1000))
    
    counter <- 1
    max_counter <- 50

    if (ARIMAX == FALSE){
      while (counter <= max_counter){
        sd <- sample(2:4, 1)
        startpars <- rnorm(n = n, mean = 0, sd = sd)
        results = try(optim(par = startpars, fn = compute_arima_mse, order=orders, method = "Nelder-Mead", control = list(maxit = 500)), silent = TRUE) #
        if ((is(results, 'try-error')) || (results$convergence != 0))
        {
          counter = counter + 1
        } else{
          break;
        }
      }
    } else{
      while (counter <= max_counter){
        sd <- sample(2:4, 1)
        startpars <- rnorm(n = n, mean = 0, sd = sd)
        results = try(optim(par = startpars, fn = compute_arima_mse, external_regressor = reg, order=orders, method = "Nelder-Mead", control = list(maxit = 500)), silent = TRUE) #
        if ((is(results, 'try-error')) || (results$convergence != 0))
        {
          counter = counter + 1
        } else{
          break;
        }
      }
    }
    
    
    
    if (counter >= max_counter){
      templist[[1]] <- orders
      templist[[2]] = "500 tries: failed to converge"
    } else{
    if (ARIMAX == TRUE){
        model=arima(x=ts, order = orders, fixed = results$par, xreg = reg)
      } else{
        model=arima(x=ts, order = orders)
      }
      
      templist <- list(order = orders, parameters = results$par, value = results$value, iterations = results$counts, convergence = results$convergence, message = results$message, loglikelihood = model$loglik, AIC = model$aic)
      }
    } else{      
    templist[[1]] <- orders
    templist[[2]] <- "No parameters to optimize for"
    }
  return(templist)
}


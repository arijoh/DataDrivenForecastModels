
multistepARIMA <- function(order, coefficients, external_regressor){
  
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  
  step1 <- function(ts, step1res){
    
    ######## If mean is not zero
    if (mean(ts, na.rm = T) > tol){
      ts <- ts - mean(ts, na.rm = TRUE)
    }
    X <- matrix(ncol = (p + q + 1 + length(REG)), ## Number of terms
                nrow = length(observed)
    )
    if (external_regressor == FALSE){
      if ((p > 0) & (q > 0)){
        for (i in ((r+nstep+d):length(observed))){ ## i is the n we are predicting, we need to add d
          y <- ts[(i - 1 - d + d):(i - p - d + d)]
          eps <- step1res[(i - 1):(i-q)]
          X[i, ] <- c(1, y, eps) #X[(i-r-d), ]
        }
      } else if ((p > 0) & (q == 0)){
        for (i in ((r+nstep+d):(length(observed)))){ ## i is the n we are predicting, we need to add d
          y <- ts[(i - 1 - d + d):(i - p - d + d)]
          X[i, ] <- c(1, y)
        }
      } else if ((p == 0) & (q > 0)){
        for (i in ((r+nstep+d):(length(observed)))){ ## i is the n we are predicting, we need to add d
          eps <- step1res[(i - 1 + d + d):(i-q + d + d)]
          X[i, ] <- c(1, eps)
        }
      } else{
        X[] <- 1
      }
    } else {
      #### With regressor
      if ((p > 0) & (q > 0)){
        for (i in ((r+nstep+d):length(observed))){ ## i is the n we are predicting, we need to add d
          y <- ts[(i - 1 - d + d):(i - p - d + d)]
          eps <- step1res[(i - 1):(i-q)]
          X[i, ] <- c(1, y, eps, Regressor[i])
        }
      } else if ((p > 0) & (q == 0)){
        for (i in ((r+nstep+d):(length(observed)))){ ## i is the n we are predicting, we need to add d
          y <- ts[(i - 1 - d + d):(i - p - d + d)]
          X[i, ] <- c(1, y, Regressor[i])
        }
      } else if ((p == 0) & (q > 0)){
        for (i in ((r+nstep+d):(length(observed)))){ ## i is the n we are predicting, we need to add d
          eps <- step1res[(i - 1 + d + d):(i-q + d + d)]
          X[i, ] <- c(1, eps, Regressor[i])
        }
      } else{
        X[] <- 1
      }
      
      
    }
    
    
    pred <- X %*% delta
    
    if (d > 0){
      pred_ <- pred
      pred <- pred + lag(observed, 1)
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
  step_n <- function(priorPred, priorRes, nstep, X){## might not need ts or step1res
    
    ######## If mean is not zero
    if (abs(mean(priorPred, na.rm = TRUE)) > tol){
      priorPred <- priorPred - mean(priorPred, na.rm = TRUE)
    }
    
    if (nstep == 2){
      priorPred <-append(NA, priorPred[(1):(length(priorPred)-1)])
      priorRes <- rep(0, length(priorPred)) 
      temp <- nstep + r - 1 + d
      priorRes[1:temp] <- NA

      if (p > 0){
        X[,2] <- priorPred
        X[temp, ] <- NA 
      }
      if (q > 0){
        X[,2+p] <- priorRes
      }
      
      X[temp, ] <- NA
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
        priorRes[1:(r+nstep-1+d)] <- NA
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

      X[1:(nstep+r-1+d),] <- NA

    }
    
    pred <- X %*% delta
    
    if (d > 0){
      pred_ <- pred
      pred <- pred + lag(observed,1)
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
  if(external_regressor == FALSE){
    model <- arima(x = ts,order = order, fixed = coefficients)
    REG <- NULL
    Regressor <- NULL
  }else{
    model <- arima(x = ts,order = order, fixed = coefficients, xreg = Regressor)
    REG <- coefficients[length(coefficients)]
    Regressor <- Regressor
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
  output <- step1(ts, step1res)
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
    if (d > 0){ 
      output <- step_n(priorPred = pred_, priorRes = res, nstep = nstep, X = X) 
      pred_ <- unlist(output[4])
    } else{ 
      output <- step_n(priorPred = pred, priorRes = res, nstep = nstep, X = X) 
    }
    pred <- unlist(output[1])
    res <- unlist(output[2])
    X <- matrix(unlist(output[3]),   ncol = (p + q + 1 + length(REG)), nrow = length(observed))
  
    res_list[[i]] <- res
    pred_list[[i]] <- pred
  }
  
  return(list(residuals = res_list, predictions = pred_list))
}
compute_arime_mse=function(pars,order){
  
  output <- multistepARIMA(order = order, coefficients = pars)
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
  xxx <-  mean(e^2, na.rm = T)
  return(xxx) 
}


TS <-  read.csv("../Data/Training data/s1_training.txt", header = TRUE, sep = "\t")
Regressor <-  read.csv("../Data/Training data/d_training.txt", header = TRUE, sep = "\t")
TS <- TS$Value
Regressor <- Regressor$Value
ts <- TS
observed <- TS


# s <- seq(from = 0, to = 4, by = 2)
# orders <- expand.grid(s, 0:2, s)


# order <- c(0,2,4)
# n <- order[1]+order[3]
# if (order[2] == 0){n <- n+1}
# pars <- vector(length = n)
# pars <- rnorm(n = n, mean = 0, sd = 1)
# coefficients = pars
# coefficients = pars
# output <- multistepARIMA(order, coefficients = pars, regressor??/)

#Problems with c(0,1,8)
# (0, 2, 8)


order <- c(1,1,1)  ## add fourth dimention 

model <- arima(ts, order = order, xreg = Regressor)
coefficients = model$coef
output <- multistepARIMA(order, coefficients = coefficients, external_regressor = TRUE)
mse_regressed <- mean(unlist(output$residuals)^2, na.rm = T)


model <- arima(ts, order = order)
coefficients = model$coef
output <- multistepARIMA(order, coefficients = coefficients, external_regressor = FALSE)
mse <- mean(unlist(output$residuals)^2, na.rm = T)

print(paste("MSE:", mse))
print(paste("MSE Regressed", mse_regressed))

# system.time({
#   result <- optim(par = pars, fn = compute_arime_mse, order=order, method = "L-BFGS-B", control = list(maxit = 500))
# })








cores <- as.numeric(Sys.getenv('LSB_DJOB_NUMPROC'))
setwd("/zhome/6e/9/133731/Desktop/Thesis/Thesis")
#cores = parallel::detectCores()


meta_optim=function(orders, external_regressor){
  compute_arime_mse=function(pars,order, external_regressor){
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
    SC <-  mean(e[wwIndex == 1]^2, na.rm = T) #mean could provide better results, not really sure why..
    return(SC) 
  }
  
  templist <- list()
  
  n <- orders[1]+orders[3]
  
  if (external_regressor == TRUE){
    n = n + 1
  }
  
  if (orders[2] == 0){n <- n+1}
  
  if (n>0){
    startpars=  vector(length = n)
    counter <- 1
    max_counter <- 50
    while (counter <= max_counter){
      
      startpars <- rnorm(n = n)
      results = try(optim(par = startpars, fn = compute_arime_mse, order=orders, external_regressor = external_regressor, method = "L-BFGS-B"), silent = TRUE) #control = list(maxit = 500))
      if ((is(results, 'try-error')) || (results$convergence != 0))
      {
        counter = counter + 1
      } else{
        break;
      }
    }
    if (counter >= max_counter){
      templist[[1]] <- orders
      templist[[2]] = "500 tries: failed to converge"
    } else{
      if (external_regressor == TRUE){
        model=arima(x=ts, order = orders, fixed = results$par, xreg = Regressor)
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



##################### Reading in data ##################### 
s1_training <-  read.csv("Data/Training data/s1_training.txt", header = TRUE, sep = "\t")
s1 <- s1_training$Value
s2_training <-  read.csv("Data/Training data/s2_training.txt", header = TRUE, sep = "\t")
s2 <- s2_training$Value
s1_wwIndex <- read.csv("Data/Training data/s1_WW_training.txt", header = TRUE, sep = "\t")
s2_wwIndex <- read.csv("Data/Training data/s2_WW_training.txt", header = TRUE, sep = "\t")
Regressor <-  read.csv("Data/Training data/d_training.txt", header = TRUE, sep = "\t")
Regressor <- Regressor$Value

##################### Parameters ##################### 
MAX <- 8
#x <- as.data.frame(rbind(c(4,2,4)))
x <- expand.grid(0:MAX, 0:2, 0:MAX)
colnames(x) <- c("p", "d", "q")

##################### Optimization for Dammning (station 1) ##################### 

## training data and observed data are the same but ts is modified within functions before calculating the residuals
ts <- s1 
observed <- s1
wwIndex <- s1_wwIndex$Flag

system.time(
  CO_s1 <- parallel::mclapply(1:nrow(x), function(i){
    temp <- as.matrix(x[i,])
    temp <- c(temp[1], temp[2], temp[3])
    start_time <- Sys.time()
    results <- meta_optim(temp, external_regressor = TRUE)
    end_time <- Sys.time()
    time_taken <- end_time - start_time
    print(paste("Order: ", "(", temp[1], ", ", temp[2], ", ", temp[3], ")", " takes ", signif(time_taken, 4), " seconds to optimize", sep = ""))
  }, mc.cores = cores, mc.allow.recursive = TRUE, mc.preschedule=FALSE)
)


save(CO_s1, file = "Code/Coefficient optimization/Multi step predictions/HPC/CO_multistep_regressor_s1.RData")


##################### Optimization for Damhusaen (station 2) ##################### 

ts <- s2
observed <- s2
wwIndex <- s2_wwIndex$Flag
system.time(
  CO_s2 <- parallel::mclapply(1:nrow(x), function(i){
    temp <- as.matrix(x[i,])
    temp <- c(temp[1], temp[2], temp[3])
    start_time <- Sys.time()
    results <- meta_optim(temp, external_regressor = TRUE)
    end_time <- Sys.time()
    time_taken <- end_time - start_time
    print(paste("Order: ", "(", temp[1], ", ", temp[2], ", ", temp[3], ")", " takes ", signif(time_taken, 4), " seconds to optimize", sep = ""))
  }, mc.cores = cores, mc.allow.recursive = TRUE, mc.preschedule = FALSE)
)

save(CO_s2, file = "Code/Coefficient optimization/Multi step predictions/HPC/CO_multistep_regressor_s2.RData")






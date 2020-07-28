setwd("/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code")
wd <- getwd()
source(paste(wd,"/Coefficient_optimization/Multi_step_predictions/meta_optim_functions/evaluationPart_of_meta_optim.r", sep = ""))


evaluate <- function(ts, pred30, pred60, pred90){
  
  
  #output <- multistepARIMA(ts = ts, order = order, coefficients = coefficients, Xreg = Xreg)
  #pred <- unlist(output$predictions[3])
  #pred <- norm_const_station * pred
  runoff_validation <- norm_const_station * ts
  
  data <- as.data.frame(cbind(as.character(coredata(time)), coredata(runoff_validation), coredata(pred30)), stringsAsFactors=FALSE)
  colnames(data) <- c("Timestamp" , "Meas", "pred")
  # Evaluating 60 min forecast.
  evaluation30 <- evaluateModel(data)
  
  
  ## Produce 60 min forecast
  #output <- multistepARIMA(ts = ts, order = order, coefficients = coefficients, Xreg = Xreg)
  #pred <- unlist(output$predictions[6])
  #pred <- norm_const_station * pred
  #runoff_validation <- norm_const_station * ts
  data <- as.data.frame(cbind(as.character(coredata(time)), coredata(runoff_validation), coredata(pred60)), stringsAsFactors=FALSE)
  colnames(data) <- c("Timestamp" , "Meas", "pred")
  # Evaluating 60 min forecast.
  evaluation60 <- evaluateModel(data)
  
  #output <- multistepARIMA(ts = ts, order = order, coefficients = coefficients, Xreg = Xreg)
  #pred <- unlist(output$predictions[9])
  #pred <- norm_const_station * pred
  #runoff_validation <- norm_const_station * ts
  data <- as.data.frame(cbind(as.character(coredata(time)), coredata(runoff_validation), coredata(pred90)), stringsAsFactors=FALSE)
  colnames(data) <- c("Timestamp" , "Meas", "pred")
  # Evaluating 60 min forecast.
  evaluation90 <- evaluateModel(data)
  
  # lim <- 60
  # data <- as.data.frame(cbind(p1[50:lim], p2[50:lim], p3[50:lim], runoff_validation[50:lim]))
  # library(ggplot2)
  # ggplot(data, aes(x=50:lim)) +
  #   geom_line(aes(y=V1, color = "30 minutes"))+
  #   geom_line(aes(y=V2, color = "60 minutes"))+
  #   geom_line(aes(y=V3, color = "90 minutes"))+
  #   geom_line(aes(y=V4, color = 'Measured values'))
  return(list(evaluation30 = evaluation30, 
              evaluation60 = evaluation60,
              evaluation90 = evaluation90))
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
      } else{
        P <- NULL
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
      
      
      X <- cbind(rep(1, length(ts)),
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
    #print("No parameters to optimize for")
    #exit()
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
orderList <- function(x){
  values <- vector()
  for (i in (1:length(x))){
    temp <- unlist(x[[i]]$PI[2])
    if (is.null(temp) || (is.nan(temp))){
      values[i] <- NA
    }
    else{
      values[i] <- as.numeric(temp) 
    }
  }
  orderedList <- x[order(values, decreasing = TRUE)]
  return(orderedList)
}
exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
model_accuracy <- function(x){
  x <- as.data.frame(x)
  
  upper_correct <- x$correct + x$correct_negative
  under_correct <- x$down+x$false_alarm+x$missed+x$correct_negative+x$correct
  
  accuracy_correct <- round(upper_correct/under_correct, digits = 3)
  
  upper_correct_plus_early <- upper_correct + x$early
  under_correct_plus_early <- under_correct + x$early
  
  accuracy_correct_plus_early <- round(upper_correct_plus_early/under_correct_plus_early, digits = 3)
  
  return(list(accuracy_correct = accuracy_correct,
              accuracy_correct_plus_early = accuracy_correct_plus_early))
  
}
evaluateLists <- function(L){
  
  if (External_Regressor == TRUE){
    Xreg <- Regressor
  }else{Xreg <- NULL}
  
  for (i in (1:length(L))){
    print(i)
    order <- L[[i]]$order
    coefficients <- L[[i]]$par
    
    if (!is.null(coefficients)){
      
      out <- multistepARIMA(coredata(ts), order = order, coefficients = coefficients, Xreg = Xreg)
      pred30 <- as.xts(unlist(out$predictions[3]), order.by = time)*norm_const_station
      pred60 <- as.xts(unlist(out$predictions[6]), order.by = time)*norm_const_station
      pred90 <- as.xts(unlist(out$predictions[9]), order.by = time)*norm_const_station

      E <- evaluate(ts, pred30, pred60, pred90)
      

      #### Here are the error calculations
      L[[i]]$evaluation <- list(evaluation30 = E$evaluation30,
                                evaluation60 = E$evaluation60,
                                evaluation90 = E$evaluation90)
      
      L[[i]]$accuracy <- list(accuracy30 = model_accuracy(E$evaluation30),
                              accuracy60 = model_accuracy(E$evaluation60),
                              accuracy90 = model_accuracy(E$evaluation90))
      
      observed <- (ts * norm_const_station)[wwIndex]
      L[[i]]$PI <-  list(PI30 = PI(pred = pred30, obs = observed, nstep = 3),
                         PI60 = PI(pred = pred60, obs = observed, nstep = 6),
                         PI90 =  PI(pred = pred90, obs = observed, nstep = 9))
      
      L[[i]]$RMSE <- list(RMSE30 = sqrt(sum((observed - pred30)^2, na.rm = T)/length(observed)),
                          RMSE60 = sqrt(sum((observed - pred60)^2, na.rm = T)/length(observed)),
                          RMSE90 = sqrt(sum((observed - pred90)^2, na.rm = T)/length(observed)))
      
      L[[i]]$MAE <- list(MAE30 = sum(abs(observed - pred30), na.rm = T)/length(observed),
                         MAE60 = sum(abs(observed - pred60), na.rm = T)/length(observed),
                         MAE90 = sum(abs(observed - pred90), na.rm = T)/length(observed))
      
    }
  }

  L_ordered <- orderList(L)
  return(L_ordered)
}
PI <- function(obs, pred, nstep){
  
  tsData <- cbind(obs, pred)
  colnames(tsData) <- c("Observed", "Predicted")
  
  ref_point <- which(!is.na(diff(tsData$Predicted)))[1] - 1
  
  ref <- as.numeric(rep(NA, ref_point))
  ref <- as.xts(ref, order.by = as.POSIXct(index(tsData)[1:ref_point], tz = "GMT"))
  for (i in seq(from = ref_point, to = (nrow(tsData)-nstep), by = nstep)){
    newref <- as.xts(rep(tsData$Observed[i-1],nstep), order.by = as.POSIXct(index(tsData)[(i):(i+nstep-1)]), tz = "GMT")
    ref <- rbind(ref, newref)
  }
  tsData <- merge(tsData, ref, join = "left")
  
  over <- (tsData$Observed-tsData$Predicted)^2
  under <- (tsData$Observed-tsData$ref)^2
  
  PI <- 1 - sum(over[wwIndex == 1], na.rm = T)/sum(under[wwIndex == 1], na.rm = T)
  return(PI)
}
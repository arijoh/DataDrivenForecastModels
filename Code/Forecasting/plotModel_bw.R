plotModel <- function(model, from, to, type){
  multistep_forecast <- function(ts, norm_const_station, order, coefficients, External_Regressors, reg.nr, reg.lag){
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
    getReg <- function(External_Regressor, reg.nr, reg.lag){
      if (External_Regressor == FALSE){
        reg.nr <- NULL
        reg.lag <- NULL
        reg <- NULL
      } else
        if (External_Regressor == TRUE){
          reg.nr <- reg.nr
          reg.lag <- reg.lag
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
        }
      return(reg)
    }
    exit <- function() {
      .Internal(.invokeRestart(list(NULL, NULL), NULL))
    }
    
    
    if (External_Regressors == TRUE){
      if (missing(reg.nr)||missing(reg.lag)){
        print("Provide reg.nr and reg.lag")
        exit()
      }
      Xreg <- getReg(External_Regressor = External_Regressors, reg.nr, reg.lag)
    } else {Xreg <- NULL}
    
    
    
    ### Analyze why higher n forecasts start later than others
    output <- multistepARIMA(coredata(ts), order = order, coefficients = coefficients, Xreg = Xreg)
    pred10 <- unlist(output$predictions[1])
    pred20 <- unlist(output$predictions[2])
    pred30 <- unlist(output$predictions[3])
    pred40 <- unlist(output$predictions[4])
    pred50 <- unlist(output$predictions[5])
    pred60 <- unlist(output$predictions[6])
    pred70 <- unlist(output$predictions[7])
    pred80 <- unlist(output$predictions[8])
    pred90 <- unlist(output$predictions[9])
    
    
    
    forecast30 <- vector(length=length(ts))
    forecast30[1:length(forecast30)] <- NA
    lastvalue <- forecast30
    idxstart <- which(!is.na(diff(pred10)))[1]+2  ## +2 to get forecast to start at xx:00
    for (i in seq(from = idxstart, to = (length(ts)-3), by = 3)){
      
      lastvalue[i:(i+2)] <- ts[i-1]
      
      forecast30[i] <- pred10[i]
      forecast30[i+1] <- pred20[i+1]
      forecast30[i+2] <- pred30[i+2]
      
    }
    
    
    data <- cbind(ts, lastvalue, pred10, pred20, pred30, forecast30)
    lastvalue30 <- xts(lastvalue, order.by = time)
    forecast30 <- xts(forecast30, order.by = time)
    
    
    forecast60 <- vector(length=length(ts))
    forecast60[1:length(forecast60)] <- NA
    lastvalue <- forecast60
    idxstart <- which(!is.na(diff(pred10)))[1]+2
    for (i in seq(from = idxstart, to = (length(ts)-6), by = 6)){
      
      lastvalue[i:(i+5)] <- ts[i-1]
      
      forecast60[i] <-   pred10[i]
      forecast60[i+1] <- pred20[i+1]
      forecast60[i+2] <- pred30[i+2]
      forecast60[i+3] <- pred40[i+3]
      forecast60[i+4] <- pred50[i+4]
      forecast60[i+5] <- pred60[i+5]
    }
    
    data <- cbind(ts, pred10, pred20, pred30, pred40, pred50, pred60, forecast60)
    lastvalue60 <- xts(lastvalue, order.by = time)
    forecast60 <- xts(forecast60, order.by = time)
    
    forecast90 <- vector(length=length(ts))
    forecast90[1:length(forecast90)] <- NA
    lastvalue <- forecast90
    idxstart <- which(!is.na(diff(pred10)))[1]+2
    for (i in seq(from = idxstart, to = (length(ts)-9), by = 9)){
      
      lastvalue[i:(i+8)] <- ts[i-1]
      
      forecast90[i] <-   pred10[i]
      forecast90[i+1] <- pred20[i+1]
      forecast90[i+2] <- pred30[i+2]
      forecast90[i+3] <- pred40[i+3]
      forecast90[i+4] <- pred50[i+4]
      forecast90[i+5] <- pred60[i+5]
      forecast90[i+6] <- pred70[i+6]
      forecast90[i+7] <- pred80[i+7]
      forecast90[i+8] <- pred90[i+8]
    }
    
    data <- cbind(ts, pred10, pred20, pred30, pred40, pred50, pred60,pred70, pred80, pred90, forecast90)
    forecast90 <- xts(forecast90, order.by = time)
    lastvalue90 <- xts(lastvalue, order.by = time)
    
    
    forecast30 <- norm_const_station * forecast30
    forecast60 <- norm_const_station * forecast60
    forecast90 <- norm_const_station * forecast90
    lastvalue30 <- norm_const_station * lastvalue30
    lastvalue60 <- norm_const_station * lastvalue60
    lastvalue90 <- norm_const_station * lastvalue90
    observation <- norm_const_station * ts
    data <- cbind(observation, lastvalue30, lastvalue60, lastvalue90, forecast30, forecast60, forecast90)
    
    ### Clean up
    rm(pred10, pred20, pred30, pred40, pred50, pred60, pred70, pred80, pred90, lastvalue30, lastvalue60, lastvalue90, forecast30, forecast60, forecast90)
    return(data)
  }
  plotMulti <- function(data_plot, ymin, ymax, title){
    
    yl=c(ymin, ymax)
    yl_r=c(0, max(data_plot$precipitation, na.rm = T)) 
    scalingFactor=10000/yl_r[2]
    dummy_R=as.numeric(coredata(data_plot$precipitation)*scalingFactor)
    in_R  = data.frame(x1=as.character(index(data_plot)),
                       x2=c(as.character(index(data_plot))[2:length(index(data_plot))],as.character(index(data_plot)[length(index(data_plot))]+60)),   
                       y1=rep(yl[2],length(index(data_plot))),   
                       y2=(yl[2]-dummy_R))
    
    dy_r=0.2
    yTick=c(seq(yl[2]/scalingFactor,yl[2]/scalingFactor-yl_r[2],-dy_r))
    ylab=seq(0,(length(yTick)-1)*dy_r,dy_r)
    
    
    ggplot(data_plot, aes(x = as.POSIXct(index(data_plot))))+
      geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
                fill='#787878', colour = "#787878", alpha=0.3,color=NA) +
      #geom_point(aes(y = forecast30, color = "Forecast"), size = 1,  shape = 17) + 
      geom_line(aes(y = forecast30, color = "Forecast", group = lastvalue90), linetype = "dotted", size = 0.5)+
      geom_point(aes(y = station, color = "Measured"), size = 0.3) + 
      geom_line(aes(y = station, color = "Measured"), linetype = "solid", size = 0.3) +
      scale_x_datetime(labels = date_format("%H:%M"))+
      scale_y_continuous(limits = yl, expand = c(0, 0), breaks=seq(yl[1],yl[2],5000),
                         sec.axis = sec_axis(~./scalingFactor, name = "Rain [mm/hr]  ", breaks=yTick,labels =ylab))  +
      ylab("Runoff [m3/hr]")+xlab("Time") +
      ggtitle(title)+
      theme_pubclean()+
      theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
            panel.grid.minor = element_blank(),
            panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
            legend.title = element_blank(),
            plot.title = element_text(size = 10),
            axis.title = element_text(size = 8),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
      scale_color_manual(values=c("Measured" = "#808080", "Forecast" = "#303030"))+
      guides(color = guide_legend(override.aes = list(linetype = c("solid", "dotted"))))
  }
  
  
  dammningList_ <- lapply(X=(1:5), function(i){ dammningList[[i]]})
  damhusaenList_ <- lapply(X=(1:5), function(i){ damhusaenList[[i]]})
  
  data_dammning <- dammningList[[model]]
  data_damhusaen <- damhusaenList[[model]]
  
  reg.nr_dammning <- data_dammning$reg.nr
  reg.lag_dammning <- data_dammning$reg.lag
  order_dammning <- data_dammning$order
  coefficients_dammning <- data_dammning$par
  #type_damningen <- paste(data_dammning$model, " ", data_dammning$ofc, sep = "")
  type_damningen <- data_dammning$ofc
  
  reg.nr_damhusaen <- data_damhusaen$reg.nr
  reg.lag_damhusaen <- data_damhusaen$reg.lag
  order_damhusaen <- data_damhusaen$order
  coefficients_damhusaen <- data_damhusaen$par
  #type_damhusaen <- paste(data_damhusaen$model, " ", data_damhusaen$ofc, sep = "")
  type_damhusaen <- data_damhusaen$ofc
  
  if (is.null(reg.nr_dammning) && is.null(reg.lag_dammning)){
    External_Regressors_dammning <- FALSE
  }else {External_Regressors_dammning <- TRUE}
  
  if (is.null(reg.nr_damhusaen) && is.null(reg.lag_damhusaen)){
    External_Regressors_damhusaen <- FALSE
  }else {External_Regressors_damhusaen <- TRUE}
  
  
  forecasts_S1 <- multistep_forecast(data$Damning, norm_const_station = station1_norm, 
                                     order = order_dammning, 
                                     coefficients = coefficients_dammning, 
                                     External_Regressors = External_Regressors_dammning,
                                     reg.nr = reg.nr_dammning,
                                     reg.lag = reg.lag_dammning)
  
  forecasts_S2 <- multistep_forecast(data$Damhusaen,norm_const_station = station2_norm, 
                                     order = order_damhusaen, 
                                     coefficients = coefficients_damhusaen, 
                                     External_Regressors = External_Regressors_damhusaen,
                                     reg.nr = reg.nr_damhusaen,
                                     reg.lag = reg.lag_damhusaen)
  
  
  forecasts_S1$precipitation <- Regressor *  precipitation_norm
  forecasts_S2$precipitation <- Regressor *  precipitation_norm
  
  from <- which(index(data) == as.character(from))
  to <- which(index(data) == as.character(to))
  data_plot_S1 <- forecasts_S1[from:to,]
  data_plot_S2 <- forecasts_S2[from:to,]
  colnames(data_plot_S1)[1] <- "station"
  colnames(data_plot_S2)[1] <- "station"
  
  PI1 <- round(as.numeric(data_dammning$PI[3]), digits = 3)
  PI2 <- round(as.numeric(data_damhusaen$PI[3]), digits = 3)
  
  ymin <- 1000
  ymax <- max(data_plot_S2, na.rm = T)
  
  if (External_Regressors_dammning){
    title_dammning <- paste("Dæmningen, ", type_damningen, " (", order_dammning[1], ", ", order_dammning[2], ", ", order_dammning[3], ", ", reg.nr_dammning, ", ", reg.lag_dammning, ") , PI90 = ", PI1, sep = "")
  }else{title_dammning <- paste("Dæmningen, ", type_damningen, " (", order_dammning[1], ", ", order_dammning[2], ", ", order_dammning[3], "), PI90 = ", PI1, sep = "")}
  
  if (External_Regressors_damhusaen){
    title_damhusaen <- paste("Damhusåen, ", type_damhusaen, " (", order_damhusaen[1], ", ", order_damhusaen[2], ", ", order_damhusaen[3], ", ", reg.nr_damhusaen, ", ", reg.lag_damhusaen, ") , PI90 = ", PI2, sep = "")
  }else{title_damhusaen <- paste("Damhusåen, ", type_damhusaen, " (", order_damhusaen[1], ", ", order_damhusaen[2], ", ", order_damhusaen[3], "), PI90 = ", PI2, sep = "")}
  
  
  p1 <- plotMulti(data_plot_S1, ymin, ymax, title = title_dammning)
  p2 <- plotMulti(data_plot_S2, ymin, ymax, title = title_damhusaen)
  plot <- ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")
  plot <- annotate_figure(plot, top = text_grob(paste(type, " forecasts on ", as.Date(index(data_plot_S1)[1]), sep = ""), size = 12, vjust = 0.35))
  
  return(plot)
}

library(ggplot2)
library(xts)
library(scales)
Sys.setenv(TZ = "GMT")

d <- read.csv("../Data/Validation_data/d_validation.txt", header = TRUE, sep = "\t")
s1 <- read.csv("../Data/Validation_data/s1_validation.txt", header = TRUE, sep = "\t")
s2 <- read.csv("../Data/Validation_data/s2_validation.txt", header = TRUE, sep = "\t")

timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)
timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)
timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)
Regressor <- coredata(d_xts)
precipitation_norm <- 6.64898
station1_norm <- 9866.368
station2_norm <- 26776.54

stations <- cbind(s1_xts, s2_xts)
colnames(stations) <- c("Damningen", "Damhusaen")
colnames(d_xts) <- "Precipitation"

data <- cbind(d_xts, stations)
data$Precipitation[which(is.na(data$Precipitation))] <- 0
time <- index(data)



from <- "2018-03-09 00:10:00"
to <- "2018-03-09 12:00"


plotModel <- function(model1_damningen, model2_damningen, model1_damhusaen, model2_damhusaen, titles){
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
  plot <- function(df){
    ## Input df and output plot
    
    
    from_idx <- which(index(df) == from)
    to_idx <-  which(index(df) == to)
    
    df <- df[from_idx:to_idx]
    
    ymin <- 0
    ymax <- max(df, na.rm = T)
    yl=c(ymin, ymax)
    yl_r=c(0, max(df$Precipitation, na.rm = T)) 
    scalingFactor=10000/yl_r[2]
    dummy_R=as.numeric(coredata(df$Precipitation)*scalingFactor)
    in_R  = data.frame(x1=as.character(index(df)),
                       x2=c(as.character(index(df))[2:length(index(df))],as.character(index(df)[length(index(df))]+60)),   
                       y1=rep(yl[2],length(index(df))),   
                       y2=(yl[2]-dummy_R))
    
    dy_r=0.1
    yTick=c(seq(yl[2]/scalingFactor,yl[2]/scalingFactor-yl_r[2],-dy_r))
    ylab=seq(0,(length(yTick)-1)*dy_r,dy_r)

    #### Go carefully over plots!!
    p1 <- ggplot(df, aes(x = as.POSIXct(index(df))))+
      geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
                fill='#0394fc', colour = "#246ea3", alpha=0.3,color=NA)+
      geom_line(aes(y = forecast_m1_damningen, colour = "Forecast", group = group_forecast_m1_damningen))+
      geom_line(aes(y = Damningen, color = "Measured"), linetype = "dotted", size = 1.15)+
      scale_x_datetime(labels = date_format("%H:%M"))+
      scale_y_continuous(limits = yl, expand = c(0, 0), breaks=seq(yl[1],yl[2],5000),
                         sec.axis = sec_axis(~./scalingFactor, name = "Rain [mm/hr]  ", breaks=yTick,labels =ylab))  +
      ylab("Runoff [m3/hr]")+xlab("Time") +
      ggtitle(titles[1])+
      theme_pubclean()+
      theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
            panel.grid.minor = element_blank(),
            panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
            legend.title = element_blank(),
            plot.title = element_text(size = 10),
            axis.title = element_text(size = 8),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
      scale_color_manual(values=c("Measured" = "black", "Forecast" = "#fc0303"))+
      guides(color = guide_legend(override.aes = list(linetype = c("solid", "dotted"))))
    
    p2 <- ggplot(df, aes(x = as.POSIXct(index(df))))+
      geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
                fill='#0394fc', colour = "#246ea3", alpha=0.3,color=NA)+
      geom_line(aes(y = forecast_m2_damningen, colour = "Forecast", group = group_forecast_m2_damningen))+
      geom_line(aes(y = Damningen, color = "Measured"), linetype = "dotted", size = 1.15) +
      scale_x_datetime(labels = date_format("%H:%M"))+
      scale_y_continuous(breaks=seq(yl[1],yl[2],5000),
                         sec.axis = sec_axis(~./scalingFactor, name = "Rain [mm/hr]  ", breaks=yTick,labels =ylab))  +
      coord_cartesian(ylim = c(0, 20000))+
      ylab("Runoff [m3/hr]")+xlab("Time") +
      ggtitle(titles[2])+
      theme_pubclean()+
      theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
            panel.grid.minor = element_blank(),
            panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
            legend.title = element_blank(),
            plot.title = element_text(size = 10),
            axis.title = element_text(size = 8),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
      scale_color_manual(values=c("Measured" = "black", "Forecast" = "#fc0303"))+
      guides(color = guide_legend(override.aes = list(linetype = c("solid", "dotted"))))
    
    p3 <- ggplot(df, aes(x = as.POSIXct(index(df))))+
      geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
                fill='#0394fc', colour = "#246ea3", alpha=0.3,color=NA)+
      geom_line(aes(y = forecast_m1_damhusaen, colour = "Forecast", group = group_forecast_m1_damhusaen))+
      geom_line(aes(y = Damhusaen, color = "Measured"), linetype = "dotted", size = 1.15) +
      scale_x_datetime(labels = date_format("%H:%M"))+
      scale_y_continuous(limits = yl, expand = c(0, 0), breaks=seq(yl[1],yl[2],5000),
                         sec.axis = sec_axis(~./scalingFactor, name = "Rain [mm/hr]  ", breaks=yTick,labels =ylab))  +
      ylab("Runoff [m3/hr]")+xlab("Time") +
      ggtitle(titles[3])+
      theme_pubclean()+
      theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
            panel.grid.minor = element_blank(),
            panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
            legend.title = element_blank(),
            plot.title = element_text(size = 10),
            axis.title = element_text(size = 8),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
      scale_color_manual(values=c("Measured" = "black", "Forecast" = "#fc0303"))+
      guides(color = guide_legend(override.aes = list(linetype = c("solid", "dotted"))))
    
    p4 <- ggplot(df, aes(x = as.POSIXct(index(df))))+
      geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
                fill='#0394fc', colour = "#246ea3", alpha=0.3,color=NA)+
      geom_line(aes(y = forecast_m2_damhusaen, colour = "Forecast", group = group_forecast_m2_damhusaen))+
      geom_line(aes(y = Damhusaen, color = "Measured"), linetype = "dotted", size = 1.15) +
      scale_x_datetime(labels = date_format("%H:%M"))+
      scale_y_continuous(breaks=seq(yl[1],yl[2],5000),
                         sec.axis = sec_axis(~./scalingFactor, name = "Rain [mm/hr]  ", breaks=yTick,labels =ylab))  +
      coord_cartesian(ylim = c(0, 20000))+
      ylab("Runoff [m3/hr]")+xlab("Time") +
      ggtitle(titles[4])+
      theme_pubclean()+
      theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
            panel.grid.minor = element_blank(),
            panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
            legend.title = element_blank(),
            plot.title = element_text(size = 10),
            axis.title = element_text(size = 8),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
      scale_color_manual(values=c("Measured" = "black", "Forecast" = "#fc0303"))+
      guides(color = guide_legend(override.aes = list(linetype = c("solid", "dotted"))))
    
    
    library(ggpubr)
    p <- ggarrange(p1, p2, p3, p4, common.legend = T, legend = "bottom")
    p
    
    return(p)
  }
  forecast <- function(model, station){
    
    order <- model$order
    coefficients <- model$par
    reg.lag <- model$reg.lag
    reg.nr <- model$reg.nr
    type2 <- model$ofc
    type1 <- model$model

    ## Also get errors if we with
    if (is.null(reg.nr) && is.null(reg.lag)){
      External_Regressors <- FALSE
    }else {External_Regressors <- TRUE}
    
    
    if (station == "Damningen"){
      forecast <- multistep_forecast(data$Damningen, norm_const_station = station1_norm,
                                     order = order,
                                     coefficients = coefficients,
                                     External_Regressors = External_Regressors,
                                     reg.nr = reg.nr,
                                     reg.lag = reg.lag)
    }else
    if (station == "Damhusaen"){
      forecast <- multistep_forecast(data$Damhusaen, norm_const_station = station2_norm,
                                     order = order,
                                     coefficients = coefficients,
                                     External_Regressors = External_Regressors,
                                     reg.nr = reg.nr,
                                     reg.lag = reg.lag)
    }else{
      print("Error, please type Damningen/Damhusaen!")
      forecast <- NULL
    }
    

  return(forecast)
    
  }
  prepareData <- function(f1, f2, f3, f4){
    ### This function only extracts 90 minute forecast!!
    
    observations <- cbind(data$Precipitation*precipitation_norm, f1$Damningen, f3$Damhusaen)
    
    ### here!!
    select_ <- c("lastvalue90", "forecast90")
    f1 <- f1[,select_]
    f2 <- f2[,select_]
    f3 <- f3[,select_]
    f4 <- f4[,select_]
    
    colnames(f1) <- c("group_forecast_m1_damningen", "forecast_m1_damningen")
    colnames(f2) <- c("group_forecast_m2_damningen", "forecast_m2_damningen")
    colnames(f3) <- c("group_forecast_m1_damhusaen", "forecast_m1_damhusaen")
    colnames(f4) <- c("group_forecast_m2_damhusaen", "forecast_m2_damhusaen")
    
    df <- cbind(observations, f1, f2, f3, f4)
    
    return(df)    
  }
  getTitles <- function(m1,m2,m3,m4, titles){
    
    titles[1] <- paste(titles[1], "\n",  m1$model, " - ", m1$ofc, " PI = ", m1$PI$PI90, " for 90 minute horizon.", sep = "")
    titles[2] <- paste(titles[2], "\n",  m2$model, " - ", m2$ofc, " Accuracy = ", m2$accuracy$accuracy90$accuracy_correct, " for 90 minute horizon", sep = "")
    titles[3] <- paste(titles[3], "\n",  m3$model, " - ", m3$ofc, " PI = ", m3$PI$PI90, " for 90 minute horizon", sep = "")
    titles[4] <- paste(titles[4], "\n",  m4$model, " - ", m4$ofc, " Accuracy = ", m4$accuracy$accuracy90$accuracy_correct, " for 90 minute horizon", sep = "")
    
    return(titles)
  }
  
  #### Data for Damningen w. model 1
  forecast_model1_damningen <- forecast(model1_damningen, "Damningen")
  ### Data for Damhusaen w. model 1
  forecast_model2_damningen <- forecast(model2_damningen, "Damningen")
  #### Data for Damningen w. model 2
  forecast_model1_damhusaen <- forecast(model1_damhusaen, "Damhusaen")
  #### Data for Damhusaen w. model 2
  forecast_model2_damhusaen <- forecast(model2_damhusaen, "Damhusaen")

  titles <- getTitles(model1_damningen, model2_damningen, model1_damhusaen, model1_damhusaen, titles)
  ## Merge all data together and sent it to a plot function
  df <- prepareData(forecast_model1_damningen, forecast_model2_damningen, forecast_model1_damhusaen, forecast_model2_damhusaen)
  return(plot(df))
}












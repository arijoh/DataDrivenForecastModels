library(xtable)

#### HERE WE GET THE TOP PERFORMING MODELS (NO MATTER THE MODEL TYPE, I.E. SINGLE/MULTI-STEP ARIMA/ARIMAX)
#### THE BEST MODELS ARE SAVED AS DATA FRAME AND CAN BE PRINTED OUT TO LATEX TABLES

## Singlestep
files <- list.files(path = "Evaluation/DDS/Evaluation_singlestep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S1 <- lapply(paste("Evaluation/DDS/Evaluation_singlestep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_singlestep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S2 <- lapply(paste("Evaluation/DDS/Evaluation_singlestep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_singlestep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S1 <- lapply(paste("Evaluation/DDS/Evaluation_singlestep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_singlestep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S2 <- lapply(paste("Evaluation/DDS/Evaluation_singlestep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

## Multistep
files <- list.files(path = "Evaluation/DDS/Evaluation_multistep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S1 <- lapply(paste("Evaluation/DDS/Evaluation_multistep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_multistep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S2 <- lapply(paste("Evaluation/DDS/Evaluation_multistep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_multistep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S1 <- lapply(paste("Evaluation/DDS/Evaluation_multistep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_multistep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S2 <- lapply(paste("Evaluation/DDS/Evaluation_multistep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)



labelList <- function(L, ofc_, model_){
  for (i in (1:length(L))){
    L[[i]]$ofc <- ofc_
    L[[i]]$model <- model_
  }
  return(L)
}

## take together all models for station
ARIMA_ss_S1 <- labelList(ARIMA_ss_S1, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S1 <- labelList(ARIMAX_ss_S1, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S1 <- labelList(ARIMA_ms_S1, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S1 <- labelList(ARIMAX_ms_S1, ofc_ = "multi-step", model_ = "ARIMAX")
DamningenList <- c(ARIMA_ss_S1, ARIMAX_ss_S1, ARIMA_ms_S1, ARIMAX_ms_S1)

ARIMA_ss_S2 <- labelList(ARIMA_ss_S2, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S2 <- labelList(ARIMAX_ss_S2, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S2 <- labelList(ARIMA_ms_S2, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S2 <- labelList(ARIMAX_ms_S2, ofc_ = "multi-step", model_ = "ARIMAX")
DamhusaenList <- c(ARIMA_ss_S2, ARIMAX_ss_S2, ARIMA_ms_S2, ARIMAX_ms_S2)

##
orderListPI <- function(x){
  values <- vector()
  for (i in (1:length(x))){
    temp <- unlist(x[[i]]$PI[3])
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

orderListAccuracy <- function(x){
  values <- vector()
  for (i in (1:length(x))){
    temp <- unlist(x[[i]]$accuracy$accuracy90[1])
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

### Best models based on PI and accuracy for each station
DamningenList_PI <- orderListPI(DamningenList)
DamningenList_Accuracy <- orderListAccuracy(DamningenList)
DamhusaenList_PI <- orderListPI(DamhusaenList)
DamhusaenList_Accuracy <- orderListAccuracy(DamhusaenList)


## Move lists to dataframe
getData <- function(L, top){
  df1 <- data.frame(matrix(NA, nrow = top, ncol = 7))
  df2 <- data.frame(matrix(NA, nrow = top, ncol = 7))
  colnames(df1) <- c("ofc", "order", "reg.nr", "reg.lag", "PI30", "PI60", "PI90")
  colnames(df2) <- c("ofc", "order", "reg.nr", "reg.lag", "A30", "A60", "A90")
  nsig <- 2
  
  for (i in (1:top)){
    
    df1$ofc[i] <- unlist(L[[i]]$ofc)
    df2$ofc[i] <- unlist(L[[i]]$ofc)
    
    order <- paste(L[[i]]$order[1], ", ", L[[i]]$order[2], ", ", L[[i]]$order[3], sep ="")
    df1$order[i] <- order
    df2$order[i] <- order
    
    reg.nr <- unlist(L[[i]]$reg.nr)
    reg.lag <-  unlist(L[[i]]$reg.lag)
    
    if(!is.null(reg.nr)){
      df1$reg.nr[i] <- round(reg.nr)
      df2$reg.nr[i] <- round(reg.nr)
    }
    if(!is.null(reg.lag)){
      df1$reg.lag[i] <- round(reg.lag)
      df2$reg.lag[i] <- round(reg.lag)
    }
    
    df1$PI30[i] <- signif(unlist(L[[i]]$PI[1]), digits = nsig)
    df1$PI60[i] <- signif(unlist(L[[i]]$PI[2]), digits = nsig)
    df1$PI90[i] <- signif(unlist(L[[i]]$PI[3]), digits = nsig)
    
    df2$A30[i] <- signif(unlist(L[[i]]$accuracy$accuracy30[1]), digits = nsig)
    df2$A60[i] <- signif(unlist(L[[i]]$accuracy$accuracy60[1]), digits = nsig)
    df2$A90[i] <- signif(unlist(L[[i]]$accuracy$accuracy90[1]), digits = nsig)
  }
  
  return(list(df1, df2))
}

top <- 10
DamningenDF_PI <- getData(DamningenList_PI, top = top)
DamningenDF_Accuracy <- getData(DamningenList_Accuracy, top = top)
DamhusaenDF_PI <- getData(DamhusaenList_PI, top = top)
DamhusaenDF_Accuracy <- getData(DamhusaenList_Accuracy, top = top)

#### Definition;
# DamningenDF_PI_PI : PI table of the top performing models on Dammningen station based on PI
# DamningenDF_PI_Accuracy : Accuracy table of the top performing models on Dammningen station based on PI

Damningen_PI_PI <- DamningenDF_PI[[1]]
Damningen_PI_Accuracy <- DamningenDF_PI[[2]]

Damningen_Accuracy_PI <- DamningenDF_Accuracy[[1]]
Damningen_Accuracy_Accuracy <- DamningenDF_Accuracy[[2]]

Damhusaen_PI_PI <- DamhusaenDF_PI[[1]]
Damhusaen_PI_Accuracy <- DamhusaenDF_PI[[2]]

Damhusaen_Accuracy_PI <- DamhusaenDF_Accuracy[[1]]
Damhusaen_Accuracy_Accuracy <- DamhusaenDF_Accuracy[[2]]

###### Look
# View(Damningen_PI_PI)
# View(Damningen_PI_Accuracy)
# View(Damningen_Accuracy_PI)
# View(Damningen_Accuracy_Accuracy)
# 
# View(Damhusaen_PI_PI)
# View(Damhusaen_PI_Accuracy)
# View(Damhusaen_Accuracy_PI)
# View(Damhusaen_Accuracy_Accuracy)



# save(Damningen_PI_PI, file = "Evaluation/DDS/best_models/Tables/Damningen_PI_PI.Rdata")
# save(Damningen_PI_Accuracy, file = "Evaluation/DDS/best_models/Tables/Damningen_PI_Accuracy.Rdata")
# save(Damningen_Accuracy_PI, file = "Evaluation/DDS/best_models/Tables/Damningen_Accuracy_PI.Rdata")
# save(Damningen_Accuracy_Accuracy, file = "Evaluation/DDS/best_models/Tables/Damningen_Accuracy_Accuracy.Rdata")
# save(Damhusaen_PI_PI, file = "Evaluation/DDS/best_models/Tables/Damhusaen_PI_PI.Rdata")
# save(Damhusaen_PI_Accuracy, file = "Evaluation/DDS/best_models/Tables/Damhusaen_PI_Accuracy.Rdata")
# save(Damhusaen_Accuracy_PI, file = "Evaluation/DDS/best_models/Tables/Damhusaen_Accuracy_PI.Rdata")
# save(Damhusaen_Accuracy_Accuracy, file = "Evaluation/DDS/best_models/Tables/Damhusaen_Accuracy_Accuracy.Rdata")
# 
# 
#save(DamningenList_PI, file = "Evaluation/DDS/best_models/Lists/DamningenList_PI.Rdata")
#save(DamningenList_Accuracy, file = "Evaluation/DDS/best_models/Lists/DamningenList_Accuracy.Rdata")
#save(DamhusaenList_PI, file = "Evaluation/DDS/best_models/Lists/DamhusaenList_PI.Rdata")
#save(DamhusaenList_Accuracy, file = "Evaluation/DDS/best_models/Lists/DamhusaenList_Accuracy.Rdata")


### Print tables if desired
digits <- as.vector(c(0,0,0,0,0,2,2,2,2,2,2), mode = "numeric")


#### Fix tables together


####

damningenPI <- cbind(Damningen_PI_PI, Damningen_PI_Accuracy[,5:ncol(Damningen_PI_Accuracy)])
write.table(damningenPI, file = "Evaluation/DDS/Table_DamningenPI.txt", sep = ";", quote = FALSE, row.names = F)
#xtable(damningenPI, digits = digits)

damningenAccuracy <-  cbind(Damningen_Accuracy_PI, Damningen_Accuracy_Accuracy[,5:ncol(Damningen_Accuracy_Accuracy)])
write.table(damningenAccuracy, file = "Evaluation/DDS/Table_DamningenAccuracy.txt", sep = ";", quote = FALSE, row.names = F)
#xtable(damningenAccuracy, type = "latex", digits = digits)


DamhusaenPI <- cbind(Damhusaen_PI_PI, Damhusaen_PI_Accuracy[,5:ncol(Damhusaen_PI_Accuracy)])
write.table(DamhusaenPI, file = "Evaluation/DDS/Table_DamhusaenPI.txt", sep = ";", quote = FALSE, row.names = F)
#xtable(DamhusaenPI, type = "latex", digits = digits)

DamhusaenAccuracy <-  cbind(Damhusaen_Accuracy_PI, Damhusaen_Accuracy_Accuracy[,5:ncol(Damhusaen_Accuracy_Accuracy)])
write.table(DamhusaenAccuracy, file = "Evaluation/DDS/Table_DamhusaenAccuracy.txt", sep = ";", quote = FALSE, row.names = F)
#xtable(DamhusaenAccuracy, type = "latex", digits = digits)



# xtable(Damningen_PI_PI, type = "latex", digits = digits1)
# xtable(Damningen_PI_Accuracy, type = "latex", digits = digits2)
# 
# xtable(Damningen_Accuracy_PI, type = "latex", digits = digits1)
# xtable(Damningen_Accuracy_Accuracy, type = "latex", digits = digits2)
# 
# xtable(Damhusaen_PI_PI, type = "latex", digits = digits1)
# xtable(Damhusaen_PI_Accuracy, type = "latex", digits = digits2)
# 
# xtable(Damhusaen_Accuracy_PI, type = "latex", digits = digits1)
# xtable(Damhusaen_Accuracy_Accuracy, type = "latex", digits = digits2)


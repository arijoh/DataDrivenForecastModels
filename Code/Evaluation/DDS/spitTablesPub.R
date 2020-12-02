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
ARIMA_ss_S1 <- labelList(ARIMA_ss_S1, ofc_ = "SS", model_ = "ARIMA")
ARIMAX_ss_S1 <- labelList(ARIMAX_ss_S1, ofc_ = "SS", model_ = "ARIMAX")
ARIMA_ms_S1 <- labelList(ARIMA_ms_S1, ofc_ = "MS", model_ = "ARIMA")
ARIMAX_ms_S1 <- labelList(ARIMAX_ms_S1, ofc_ = "MS", model_ = "ARIMAX")
DamningenList <- c(ARIMA_ss_S1, ARIMAX_ss_S1, ARIMA_ms_S1, ARIMAX_ms_S1)

ARIMA_ss_S2 <- labelList(ARIMA_ss_S2, ofc_ = "SS", model_ = "ARIMA")
ARIMAX_ss_S2 <- labelList(ARIMAX_ss_S2, ofc_ = "SS", model_ = "ARIMAX")
ARIMA_ms_S2 <- labelList(ARIMA_ms_S2, ofc_ = "MS", model_ = "ARIMA")
ARIMAX_ms_S2 <- labelList(ARIMAX_ms_S2, ofc_ = "MS", model_ = "ARIMAX")
DamhusaenList <- c(ARIMA_ss_S2, ARIMAX_ss_S2, ARIMA_ms_S2, ARIMAX_ms_S2)




orderListPI <- function(x, fh){
  if ((fh == 1) || (fh==2) || (fh == 3) || (fh == "AVG")){
    values <- vector()
    for (i in (1:length(x))){
      if (fh == "AVG"){temp <- unlist((as.numeric(x[[i]]$PI[1]) + as.numeric(x[[i]]$PI[2]) + as.numeric(x[[i]]$PI[3]))/3)}
      else {temp <- unlist(x[[i]]$PI[fh])}
      if (is.null(temp) || (is.nan(temp)) || (length(temp) == 0)){
        values[i] <- NA
      }
      else{
        values[i] <- as.numeric(temp) 
      }
    }
    orderedList <- x[order(values, decreasing = TRUE)]
    return(orderedList)
  }
  else{
    print("Pass in forecasting horizon 1, 2, 3, or AVG")
  }
}


orderListAccuracy <- function(x, fh){
  if ((fh == 1) || (fh==2) || (fh == 3) || (fh == "AVG")){
      values <- vector()
    for (i in (1:length(x))){
      if (fh == "AVG"){temp <- unlist((as.numeric(x[[i]]$accuracy$accuracy30$accuracy_correct) + as.numeric(x[[i]]$accuracy$accuracy60$accuracy_correct) + as.numeric(x[[i]]$accuracy$accuracy90$accuracy_correct))/3)}
      else {temp <- unlist(x[[i]]$accuracy[fh][[1]]$accuracy_correct)}
      if (is.null(temp) || (is.nan(temp)) || (length(temp) == 0)){
        values[i] <- NA
      }
      else{
        values[i] <- as.numeric(temp) 
      }
    }
    orderedList <- x[order(values, decreasing = TRUE)]
    return(orderedList)
  }
  else{
    print("Pass in forecasting horizon 1, 2, 3, or AVG")
  }
}


## PI
# DamningenList
DamningenOrdered30_PI <- orderListPI(DamningenList, fh = 1)
DamningenOrdered60_PI <- orderListPI(DamningenList, fh = 2)
DamningenOrdered90_PI <- orderListPI(DamningenList, fh = 3)
DamningenOrderedAVG_PI <- orderListPI(DamningenList, fh = "AVG")
# DamhusaenList
DamhusaenOrdered30_PI <- orderListPI(DamhusaenList, fh = 1)
DamhusaenOrdered60_PI <- orderListPI(DamhusaenList, fh = 2)
DamhusaenOrdered90_PI <- orderListPI(DamhusaenList, fh = 3)
DamhusaenOrderedAVG_PI <- orderListPI(DamhusaenList, fh = "AVG")

## Accuracy
# DamningenList
DamningenOrdered30_Accuracy <- orderListAccuracy(DamningenList, fh = 1)
DamningenOrdered60_Accuracy <- orderListAccuracy(DamningenList, fh = 2)
DamningenOrdered90_Accuracy <- orderListAccuracy(DamningenList, fh = 3)
DamningenOrderedAVG_Accuracy <- orderListAccuracy(DamningenList, fh = "AVG")
# DamhusaenList
DamhusaenOrdered30_Accuracy <- orderListAccuracy(DamhusaenList, fh = 1)
DamhusaenOrdered60_Accuracy <- orderListAccuracy(DamhusaenList, fh = 2)
DamhusaenOrdered90_Accuracy <- orderListAccuracy(DamhusaenList, fh = 3)
DamhusaenOrderedAVG_Accuracy <- orderListAccuracy(DamhusaenList, fh = "AVG")


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
# Get Damningen tables
DamningenOrdered30_PI_PI <- getData(DamningenOrdered30_PI, top = top)[[1]]
DamningenOrdered30_PI_Accuracy <- getData(DamningenOrdered30_PI, top = top)[[2]]
DamningenOrdered30_Accuracy_PI <- getData(DamningenOrdered30_Accuracy, top = top)[[1]]
DamningenOrdered30_Accuracy_Accuracy <- getData(DamningenOrdered30_Accuracy, top = top)[[2]]

DamningenOrdered60_PI_PI <- getData(DamningenOrdered60_PI, top = top)[[1]]
DamningenOrdered60_PI_Accuracy <- getData(DamningenOrdered60_PI, top = top)[[2]]
DamningenOrdered60_Accuracy_PI <- getData(DamningenOrdered60_Accuracy, top = top)[[1]]
DamningenOrdered60_Accuracy_Accuracy <- getData(DamningenOrdered60_Accuracy, top = top)[[2]]

DamningenOrdered90_PI_PI <- getData(DamningenOrdered90_PI, top = top)[[1]]
DamningenOrdered90_PI_Accuracy <- getData(DamningenOrdered90_PI, top = top)[[2]]
DamningenOrdered90_Accuracy_PI <- getData(DamningenOrdered90_Accuracy, top = top)[[1]]
DamningenOrdered90_Accuracy_Accuracy <- getData(DamningenOrdered90_Accuracy, top = top)[[2]]

DamningenOrderedAVG_PI_PI <- getData(DamningenOrderedAVG_PI, top = top)[[1]]
DamningenOrderedAVG_PI_Accuracy <- getData(DamningenOrderedAVG_PI, top = top)[[2]]
DamningenOrderedAVG_Accuracy_PI <- getData(DamningenOrderedAVG_Accuracy, top = top)[[1]]
DamningenOrderedAVG_Accuracy_Accuracy <- getData(DamningenOrderedAVG_Accuracy, top = top)[[2]]

# Get Damhusaen tables
DamhusaenOrdered30_PI_PI <- getData(DamhusaenOrdered30_PI, top = top)[[1]]
DamhusaenOrdered30_PI_Accuracy <- getData(DamhusaenOrdered30_PI, top = top)[[2]]
DamhusaenOrdered30_Accuracy_PI <- getData(DamhusaenOrdered30_Accuracy, top = top)[[1]]
DamhusaenOrdered30_Accuracy_Accuracy <- getData(DamhusaenOrdered30_Accuracy, top = top)[[2]]

DamhusaenOrdered60_PI_PI <- getData(DamhusaenOrdered60_PI, top = top)[[1]]
DamhusaenOrdered60_PI_Accuracy <- getData(DamhusaenOrdered60_PI, top = top)[[2]]
DamhusaenOrdered60_Accuracy_PI <- getData(DamhusaenOrdered60_Accuracy, top = top)[[1]]
DamhusaenOrdered60_Accuracy_Accuracy <- getData(DamhusaenOrdered60_Accuracy, top = top)[[2]]

DamhusaenOrdered90_PI_PI <- getData(DamhusaenOrdered90_PI, top = top)[[1]]
DamhusaenOrdered90_PI_Accuracy <- getData(DamhusaenOrdered90_PI, top = top)[[2]]
DamhusaenOrdered90_Accuracy_PI <- getData(DamhusaenOrdered90_Accuracy, top = top)[[1]]
DamhusaenOrdered90_Accuracy_Accuracy <- getData(DamhusaenOrdered90_Accuracy, top = top)[[2]]

DamhusaenOrderedAVG_PI_PI <- getData(DamhusaenOrderedAVG_PI, top = top)[[1]]
DamhusaenOrderedAVG_PI_Accuracy <- getData(DamhusaenOrderedAVG_PI, top = top)[[2]]
DamhusaenOrderedAVG_Accuracy_PI <- getData(DamhusaenOrderedAVG_Accuracy, top = top)[[1]]
DamhusaenOrderedAVG_Accuracy_Accuracy <- getData(DamhusaenOrderedAVG_Accuracy, top = top)[[2]]





#### Write tables
# Damningen
Damningen30_PI <- cbind(DamningenOrdered30_PI_PI, DamningenOrdered30_PI_Accuracy[,5:7])
Damningen30_Accuracy <- cbind(DamningenOrdered30_Accuracy_PI, DamningenOrdered30_Accuracy_Accuracy[,5:7])

Damningen60_PI <- cbind(DamningenOrdered60_PI_PI, DamningenOrdered60_PI_Accuracy[,5:7])
Damningen60_Accuracy <- cbind(DamningenOrdered60_Accuracy_PI, DamningenOrdered60_Accuracy_Accuracy[,5:7])

Damningen90_PI <- cbind(DamningenOrdered90_PI_PI, DamningenOrdered90_PI_Accuracy[,5:7])
Damningen90_Accuracy <- cbind(DamningenOrdered90_Accuracy_PI, DamningenOrdered90_Accuracy_Accuracy[,5:7])

DamningenAVG_PI <- cbind(DamningenOrderedAVG_PI_PI, DamningenOrderedAVG_PI_Accuracy[,5:7])
DamningenAVG_Accuracy <- cbind(DamningenOrderedAVG_Accuracy_PI, DamningenOrderedAVG_Accuracy_Accuracy[,5:7])

# Damhusaen
Damhusaen30_PI <- cbind(DamhusaenOrdered30_PI_PI, DamhusaenOrdered30_PI_Accuracy[,5:7])
Damhusaen30_Accuracy <- cbind(DamhusaenOrdered30_Accuracy_PI, DamhusaenOrdered30_Accuracy_Accuracy[,5:7])

Damhusaen60_PI <- cbind(DamhusaenOrdered60_PI_PI, DamhusaenOrdered60_PI_Accuracy[,5:7])
Damhusaen60_Accuracy <- cbind(DamhusaenOrdered60_Accuracy_PI, DamhusaenOrdered60_Accuracy_Accuracy[,5:7])

Damhusaen90_PI <- cbind(DamhusaenOrdered90_PI_PI, DamhusaenOrdered90_PI_Accuracy[,5:7])
Damhusaen90_Accuracy <- cbind(DamhusaenOrdered90_Accuracy_PI, DamhusaenOrdered90_Accuracy_Accuracy[,5:7])

DamhusaenAVG_PI <- cbind(DamhusaenOrderedAVG_PI_PI, DamhusaenOrderedAVG_PI_Accuracy[,5:7])
DamhusaenAVG_Accuracy <- cbind(DamhusaenOrderedAVG_Accuracy_PI, DamhusaenOrderedAVG_Accuracy_Accuracy[,5:7])

# Save tables
# Damningen
write.table(Damningen30_PI, file = "Evaluation/DDS/Damningen30_PI.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damningen60_PI, file = "Evaluation/DDS/Damningen60_PI.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damningen90_PI, file = "Evaluation/DDS/Damningen90_PI.txt", sep = ";", quote = FALSE, row.names = F)
write.table(DamningenAVG_PI, file = "Evaluation/DDS/DamningenAVG_PI.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damningen30_Accuracy, file = "Evaluation/DDS/Damningen30_Accuracy.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damningen60_Accuracy, file = "Evaluation/DDS/Damningen60_Accuracy.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damningen90_Accuracy, file = "Evaluation/DDS/Damningen90_Accuracy.txt", sep = ";", quote = FALSE, row.names = F)
write.table(DamningenAVG_Accuracy, file = "Evaluation/DDS/DamningenAVG_Accuracy.txt", sep = ";", quote = FALSE, row.names = F)


# Damhusaen
write.table(Damhusaen30_PI, file = "Evaluation/DDS/Damhusaen30_PI.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damhusaen60_PI, file = "Evaluation/DDS/Damhusaen60_PI.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damhusaen90_PI, file = "Evaluation/DDS/Damhusaen90_PI.txt", sep = ";", quote = FALSE, row.names = F)
write.table(DamhusaenAVG_PI, file = "Evaluation/DDS/DamhusaenAVG_PI.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damhusaen30_Accuracy, file = "Evaluation/DDS/Damhusaen30_Accuracy.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damhusaen60_Accuracy, file = "Evaluation/DDS/Damhusaen60_Accuracy.txt", sep = ";", quote = FALSE, row.names = F)
write.table(Damhusaen90_Accuracy, file = "Evaluation/DDS/Damhusaen90_Accuracy.txt", sep = ";", quote = FALSE, row.names = F)
write.table(DamhusaenAVG_Accuracy, file = "Evaluation/DDS/DamhusaenAVG_Accuracy.txt", sep = ";", quote = FALSE, row.names = F)

# Remove variables
rm(list = ls())









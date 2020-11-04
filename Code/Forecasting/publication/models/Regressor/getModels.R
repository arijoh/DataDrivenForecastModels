
## Load all models

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
DamningenList_ARIMA <- c(ARIMA_ss_S1, ARIMA_ms_S1)
DamningenList_ARIMAX <- c(ARIMAX_ss_S1, ARIMAX_ms_S1)

ARIMA_ss_S2 <- labelList(ARIMA_ss_S2, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S2 <- labelList(ARIMAX_ss_S2, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S2 <- labelList(ARIMA_ms_S2, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S2 <- labelList(ARIMAX_ms_S2, ofc_ = "multi-step", model_ = "ARIMAX")
DamhusaenList_ARIMA <- c(ARIMA_ss_S2, ARIMA_ms_S2)
DamhusaenList_ARIMAX <- c(ARIMAX_ss_S2, ARIMAX_ms_S2)

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

# orderListAccuracy <- function(x){
#   values <- vector()
#   for (i in (1:length(x))){
#     temp <- unlist(x[[i]]$accuracy$accuracy90[1])
#     if (is.null(temp) || (is.nan(temp))){
#       values[i] <- NA
#     }
#     else{
#       values[i] <- as.numeric(temp) 
#     }
#   }
#   orderedList <- x[order(values, decreasing = TRUE)]
#   return(orderedList)
# }

### Best models based on PI and accuracy for each station
DamningenList_ARIMA <- orderListPI(DamningenList_ARIMA)
DamningenList_ARIMAX <- orderListPI(DamningenList_ARIMAX)
DamhusaenList_ARIMA <- orderListPI(DamhusaenList_ARIMA)
DamhusaenList_ARIMAX <- orderListPI(DamhusaenList_ARIMAX)



## Select best 10 models based on criteria
DamningenList_ARIMA <- DamningenList_ARIMA[1:10]
DamningenList_ARIMAX <- DamningenList_ARIMAX[1:10]
DamhusaenList_ARIMA <- DamhusaenList_ARIMA[1:10]
DamhusaenList_ARIMAX <- DamhusaenList_ARIMAX[1:10]


## Save 10 models as a list
save(DamningenList_ARIMA, file = "Forecasting/publication/models/Regressor/Damningen_ARIMA.Rdata")
save(DamningenList_ARIMAX, file = "Forecasting/publication/models/Regressor/Damningen_ARIMAX.Rdata")
save(DamhusaenList_ARIMA, file = "Forecasting/publication/models/Regressor/Damhusaen_ARIMA.Rdata")
save(DamhusaenList_ARIMAX, file = "Forecasting/publication/models/Regressor/Damhusaen_ARIMAX.Rdata")




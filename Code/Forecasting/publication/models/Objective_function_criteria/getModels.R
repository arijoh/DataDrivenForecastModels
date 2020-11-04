
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
DamningenList_ss <- c(ARIMA_ss_S1, ARIMAX_ss_S1)
DamningenList_ms <- c(ARIMA_ms_S1, ARIMAX_ms_S1)

ARIMA_ss_S2 <- labelList(ARIMA_ss_S2, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S2 <- labelList(ARIMAX_ss_S2, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S2 <- labelList(ARIMA_ms_S2, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S2 <- labelList(ARIMAX_ms_S2, ofc_ = "multi-step", model_ = "ARIMAX")
DamhusaenList_ss <- c(ARIMA_ss_S2, ARIMAX_ss_S2)
DamhusaenList_ms <- c(ARIMA_ms_S2, ARIMAX_ms_S2)


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
DamningenList_ss <- orderListPI(DamningenList_ss)
DamningenList_ms <- orderListPI(DamningenList_ms)
DamhusaenList_ss <- orderListPI(DamhusaenList_ss)
DamhusaenList_ms <- orderListPI(DamhusaenList_ms)



## Select best 10 models based on criteria
DamningenList_ss <- DamningenList_ss[1:10]
DamningenList_ms <- DamningenList_ms[1:10]
DamhusaenList_ss <- DamhusaenList_ss[1:10]
DamhusaenList_ms <- DamhusaenList_ms[1:10]


## Save 10 models as a list
save(DamningenList_ss, file = "Forecasting/publication/models/Objective_function_criteria/Damningen_ss.Rdata")
save(DamningenList_ms, file = "Forecasting/publication/models/Objective_function_criteria/Damningen_ms.Rdata")
save(DamhusaenList_ss, file = "Forecasting/publication/models/Objective_function_criteria/Damhusaen_ss.Rdata")
save(DamhusaenList_ms, file = "Forecasting/publication/models/Objective_function_criteria/Damhusaen_ms.Rdata")



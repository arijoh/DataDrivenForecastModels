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
DamningenList_ARIMA <- c(ARIMA_ss_S1, ARIMA_ms_S1)
DamningenList_ARIMAX <- c(ARIMAX_ss_S1, ARIMAX_ms_S1)


ARIMA_ss_S2 <- labelList(ARIMA_ss_S2, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S2 <- labelList(ARIMAX_ss_S2, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S2 <- labelList(ARIMA_ms_S2, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S2 <- labelList(ARIMAX_ms_S2, ofc_ = "multi-step", model_ = "ARIMAX")
DamhusaenList_ARIMA <- c(ARIMA_ss_S2, ARIMA_ms_S2)
DamhusaenList_ARIMAX <- c(ARIMAX_ss_S2, ARIMAX_ms_S2)

##
orderListPI <- function(x, fh){
  values <- vector()
  for (i in (1:length(x))){
    temp <- unlist(x[[i]]$PI[fh])
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
DamningenList_ARIMA_30 <- orderListPI(DamningenList_ARIMA, fh = 1)
DamningenList_ARIMA_60 <- orderListPI(DamningenList_ARIMA, fh = 2)
DamningenList_ARIMA_90 <- orderListPI(DamningenList_ARIMA, fh = 3)

DamningenList_ARIMAX_30 <- orderListPI(DamningenList_ARIMAX, fh = 1)
DamningenList_ARIMAX_60 <- orderListPI(DamningenList_ARIMAX, fh = 2)
DamningenList_ARIMAX_90 <- orderListPI(DamningenList_ARIMAX, fh = 3)

DamhusaenList_ARIMA_30 <- orderListPI(DamhusaenList_ARIMA, fh = 1)
DamhusaenList_ARIMA_60 <- orderListPI(DamhusaenList_ARIMA, fh = 2)
DamhusaenList_ARIMA_90 <- orderListPI(DamhusaenList_ARIMA, fh = 3)

DamhusaenList_ARIMAX_30 <- orderListPI(DamhusaenList_ARIMAX, fh = 1)
DamhusaenList_ARIMAX_60 <- orderListPI(DamhusaenList_ARIMAX, fh = 2)
DamhusaenList_ARIMAX_90 <- orderListPI(DamhusaenList_ARIMAX, fh = 3)



#### Construct table
## ARIMA
table_ARIMA <- as.data.frame(matrix(NA, nrow = 2, ncol = 3))
rownames(table_ARIMA) <- c("Damningen", "Damhusaen")
colnames(table_ARIMA) <- c("30 min", "60 min", "90 min")
table_ARIMA

table_ARIMA[1,1] <- DamningenList_ARIMA_30[[1]]$PI$PI30
table_ARIMA[1,2] <- DamningenList_ARIMA_60[[1]]$PI$PI60
table_ARIMA[1,3] <- DamningenList_ARIMA_90[[1]]$PI$PI90

table_ARIMA[2,1] <- DamhusaenList_ARIMA_30[[1]]$PI$PI30
table_ARIMA[2,2] <- DamhusaenList_ARIMA_60[[1]]$PI$PI60
table_ARIMA[2,3] <- DamhusaenList_ARIMA_90[[1]]$PI$PI90

table_ARIMA

## ARIMAX
table_ARIMAX <- as.data.frame(matrix(NA, nrow = 2, ncol = 3))
rownames(table_ARIMAX) <- c("Damningen", "Damhusaen")
colnames(table_ARIMAX) <- c("30 min", "60 min", "90 min")
table_ARIMAX

table_ARIMAX[1,1] <- DamningenList_ARIMAX_30[[1]]$PI$PI30
table_ARIMAX[1,2] <- DamningenList_ARIMAX_60[[1]]$PI$PI60
table_ARIMAX[1,3] <- DamningenList_ARIMAX_90[[1]]$PI$PI90

table_ARIMAX[2,1] <- DamhusaenList_ARIMAX_30[[1]]$PI$PI30
table_ARIMAX[2,2] <- DamhusaenList_ARIMAX_60[[1]]$PI$PI60
table_ARIMAX[2,3] <- DamhusaenList_ARIMAX_90[[1]]$PI$PI90

table_ARIMAX



library(tidyverse)

dt_ARIMA <- table_ARIMA %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
head(dt_ARIMA)


dt_ARIMAX <- table_ARIMAX %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
head(dt_ARIMAX)

dt_ARIMA$Type <- "ARIMA"
dt_ARIMAX$Type <- "ARIMAX"

dt <- rbind(dt_ARIMA, dt_ARIMAX)
dt

library(ggplot2)


plot <- ggplot(dt, aes(x = colname, y = Type, fill =  value))+
  geom_tile()+
  facet_grid(cols = vars(rowname))+
  geom_text(aes(label = round(value, 2)))+
  scale_fill_gradient(low = "white",high = "#808080", na.value = "red")+
  theme_bw()

plot


## clean up prior code....
## Make plot more beautiful.....



#### Similar thing for single vs multistep ??






























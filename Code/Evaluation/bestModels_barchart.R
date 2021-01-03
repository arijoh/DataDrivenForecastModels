library(tidyverse)
library(ggplot2)
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
ARIMA_ss_S1 <-labelList(ARIMA_ss_S1, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S1 <- labelList(ARIMAX_ss_S1, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S1 <- labelList(ARIMA_ms_S1, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S1 <- labelList(ARIMAX_ms_S1, ofc_ = "multi-step", model_ = "ARIMAX")
DamningenList <- c(ARIMA_ss_S1, ARIMAX_ss_S1, ARIMA_ms_S1, ARIMAX_ms_S1)


ARIMA_ss_S2 <- labelList(ARIMA_ss_S2, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S2 <- labelList(ARIMAX_ss_S2, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S2 <- labelList(ARIMA_ms_S2, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S2 <- labelList(ARIMAX_ms_S2, ofc_ = "multi-step", model_ = "ARIMAX")
DamhusaenList <- c(ARIMA_ss_S2, ARIMAX_ss_S2, ARIMA_ms_S2, ARIMAX_ms_S2)



pullData <- function(List){
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
  orderListAccuracy <- function(x, fh){
    values <- vector()
    for (i in (1:length(x))){
      temp <- unlist(x[[i]]$accuracy[[fh]]$accuracy_correct)
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
  orderListAVGPI <- function(x){
    values <- vector()
    for (i in (1:length(x))){
      temp <- unlist((as.numeric(x[[i]]$PI[1]) + as.numeric(x[[i]]$PI[2]) + as.numeric(x[[i]]$PI[3]))/3)
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
  
  PI30 <- orderListPI(List, 1)## Best model based on PI30
  PI60 <- orderListPI(List, 2) ## Best model based on PI30
  PI90 <- orderListPI(List, 3) ## Best model based on PI30
  PIAVG <- orderListAVGPI(List) ## Best model based on PI30
  ACC30 <- orderListAccuracy(List, 1)
  ACC60 <- orderListAccuracy(List, 2)
  ACC90 <- orderListAccuracy(List, 3)
  
  countOFC <- function(List){
    count_ss <- 0
    count_ms <- 0
    for (i in (1:10)){
      if (List[[i]]$ofc == "single-step"){
        count_ss <- count_ss + 1
      }else
        if (List[[i]]$ofc == "multi-step"){
          count_ms <- count_ms + 1
        }
    }
    return(c(count_ss, count_ms))
  }
  PI30_count <- countOFC(PI30)
  PI60_count <- countOFC(PI60)
  PI90_count <- countOFC(PI90)
  PIAVG_count <- countOFC(PIAVG)
  ACC30_count <- countOFC(ACC30)
  ACC60_count <- countOFC(ACC60)
  ACC90_count <- countOFC(ACC90)
  
  df <- data.frame(matrix(NA, nrow = 7, ncol = 2))
  colnames(df) <- c("Single-step (SSE)", "Multi-step (SC)")
  rownames(df) <- c("PI30", "PI60","PI90", "PIAVG", "A30", "A60", "A90")

  df[1, 1] <- PI30_count[1]
  df[1, 2] <- PI30_count[2]
  
  df[2, 1] <- PI60_count[1]
  df[2, 2] <- PI60_count[2]
  
  df[3, 1] <- PI90_count[1]
  df[3, 2] <- PI90_count[2]

  df[4, 1] <- PIAVG_count[1]
  df[4, 2] <- PIAVG_count[2]
  
  df[5, 1] <- ACC30_count[1]
  df[5, 2] <- ACC30_count[2]
  
  df[6, 1] <- ACC60_count[1]
  df[6, 2] <- ACC60_count[2]

  df[7, 1] <- ACC90_count[1]
  df[7, 2] <- ACC90_count[2]
  
  return(df)
}


DamningenData <- pullData(DamningenList)
DamhusaenData <- pullData(DamhusaenList)


DamningenData$Station <- "Dæmningen"
DamhusaenData$Station <- "Damhusåen"


DamningenData$`Forecasting horizon` <- c("30 min", "60 min", "90 min", 'Avg', "30 min", "60 min", "90 min")
DamningenData$`Error metric` <- c('PI', 'PI', 'PI', 'PI', 'CSI', 'CSI', 'CSI')

DamhusaenData$`Forecasting horizon` <- c("30 min", "60 min", "90 min", 'Avg', "30 min", "60 min", "90 min")
DamhusaenData$`Error metric` <- c('PI', 'PI', 'PI', 'PI', 'CSI', 'CSI', 'CSI')

df <- rbind(DamningenData, DamhusaenData)
df


library(reshape)
df <- melt(df)

integer_breaks = function(range) {
  bmax = ceiling(range[2])
  bmin = floor(range[1])
  if (bmax - bmin < 7) return(bmin:bmax)
  pretty(range)
}

library(scales)
plot <- ggplot(df, aes(x = interaction(`Forecasting horizon`, `Error metric`), y = value, fill = variable))+
  geom_bar(stat="identity", position=position_dodge())+
  ylab("Count")+xlab("Selected on")+
  facet_grid(cols = vars(Station))+
  ggtitle("Objective function criteria of the 10 best performing models")+
  labs(fill = "Objectice function criteria")+
  scale_fill_manual(values = c('grey', '#636363'))+
  scale_y_continuous(breaks = integer_breaks)+
  theme_bw()+
  theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
        panel.grid.minor = element_blank(),
        panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
        plot.title = element_text(size = 10), text = element_text(size=10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom")
plot

tiff(file = "../Figures/Results/DDS/barplot_ofc.tiff", height = 75, width = 140, unit = "mm", res = 500)
plot
dev.off()
 









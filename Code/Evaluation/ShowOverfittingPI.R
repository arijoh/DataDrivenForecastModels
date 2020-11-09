


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
  
  PI30 <- orderListPI(List, 1)[[1]]$PI ## Best model based on PI30
  PI60 <- orderListPI(List, 2)[[1]]$PI ## Best model based on PI30
  PI90 <- orderListPI(List, 3)[[1]]$PI ## Best model based on PI30
  PIAVG <- orderListAVGPI(List)[[1]]$PI ## Best model based on PI30
  
  df <- data.frame(matrix(NA, nrow = 4, ncol = 3))
  colnames(df) <- c("30 minutes", "60 minutes", "90 minutes")
  rownames(df) <- c("PI30", "PI60", "PI90", "PIAVG")
  
  df[1, 1] <- as.numeric(PI30$PI30)
  df[1, 2] <- as.numeric(PI30$PI60)
  df[1, 3] <- as.numeric(PI30$PI90)
  
  df[2, 1] <- as.numeric(PI60$PI30)
  df[2, 2] <- as.numeric(PI60$PI60)
  df[2, 3] <- as.numeric(PI60$PI90)
  
  df[3, 1] <- as.numeric(PI90$PI30)
  df[3, 2] <- as.numeric(PI90$PI60)
  df[3, 3] <- as.numeric(PI90$PI90)
  
  df[4, 1] <- as.numeric(PIAVG$PI30)
  df[4, 2] <- as.numeric(PIAVG$PI60)
  df[4, 3] <- as.numeric(PIAVG$PI90)
  
  return(df)
}




DamningenData <- pullData(DamningenList)
DamhusaenData <- pullData(DamhusaenList)

DamningenData$Station <- "Dæmningen"
DamhusaenData$Station <- "Damhusåen"
DamningenData$Type <- rownames(DamningenData) 
DamhusaenData$Type <- rownames(DamhusaenData) 
df <- rbind(DamningenData, DamhusaenData)


library(reshape2)
df <- melt(df)
df


plot <- ggplot(df)+
        geom_line(aes(x = variable, y = value, group = Type, colour = Type))+
        geom_point(aes(x = variable, y = value, colour = Type, shape = Type))+
        facet_grid(cols = vars(Station))+
        scale_colour_manual(name = 'Selected on', values = c("#636363", "#636363", "#636363", "black"))+
        scale_shape_manual(name = 'Selected on', values = c(0, 1, 2, 16))+
        xlab("Forecasting horizon")+ylab("Persistence Index (PI)")+
        ylim(c(-0.2, 1))+
        ggtitle("Perfomance of models selected on different PI skill-score measures")+
        theme_bw()+
        theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
              panel.grid.minor = element_blank(),
              panel.ontop = FALSE, panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
              plot.title = element_text(size = 12), text = element_text(size=12),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              legend.position = "right")

plot



pdf(file = "../Figures/Results/DDS/overfitPI.pdf", height = 3, width = 7)
plot
dev.off()
 













library(tidyverse)
library(ggplot2)
library(ggpubr)
library(reshape2)

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



pullData <- function(){
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
  

    
  ### Best models based on PI ordered by differetn forecasting horizon
  DamningenList_ARIMA <- orderListAVGPI(DamningenList_ARIMA)
  DamningenList_ARIMAX <- orderListAVGPI(DamningenList_ARIMAX)
  DamhusaenList_ARIMA<- orderListAVGPI(DamhusaenList_ARIMA)
  DamhusaenList_ARIMAX <- orderListAVGPI(DamhusaenList_ARIMAX)
  
  
  #### Construct tables
  table_Damningen <- as.data.frame(matrix(NA, nrow = 5, ncol = 2))
  colnames(table_Damningen) <- c("ARIMA", "ARIMAX")
  table_Damningen$Station <- 'Dæmningen'
  table_Damningen$Nr <- 1:5
  table_Damhusaen <- as.data.frame(matrix(NA, nrow = 5, ncol = 2))
  colnames(table_Damhusaen) <- c("ARIMA", "ARIMAX")
  table_Damhusaen$Station <- 'Damhusåen'
  table_Damhusaen$Nr <- 1:5
  
  table_Damningen[1,1] <- mean(sapply(DamningenList_ARIMA[[1]]$PI, mean))
  table_Damningen[2,1] <- mean(sapply(DamningenList_ARIMA[[2]]$PI, mean))
  table_Damningen[3,1] <- mean(sapply(DamningenList_ARIMA[[3]]$PI, mean))
  table_Damningen[4,1] <- mean(sapply(DamningenList_ARIMA[[4]]$PI, mean))
  table_Damningen[5,1] <- mean(sapply(DamningenList_ARIMA[[5]]$PI, mean))
  
  table_Damningen[1,2] <- mean(sapply(DamningenList_ARIMAX[[1]]$PI, mean))
  table_Damningen[2,2] <- mean(sapply(DamningenList_ARIMAX[[2]]$PI, mean))
  table_Damningen[3,2] <- mean(sapply(DamningenList_ARIMAX[[3]]$PI, mean))
  table_Damningen[4,2] <- mean(sapply(DamningenList_ARIMAX[[4]]$PI, mean))
  table_Damningen[5,2] <- mean(sapply(DamningenList_ARIMAX[[5]]$PI, mean))
  
  table_Damhusaen[1,1] <- mean(sapply(DamhusaenList_ARIMA[[1]]$PI, mean))
  table_Damhusaen[2,1] <- mean(sapply(DamhusaenList_ARIMA[[2]]$PI, mean))
  table_Damhusaen[3,1] <- mean(sapply(DamhusaenList_ARIMA[[3]]$PI, mean))
  table_Damhusaen[4,1] <- mean(sapply(DamhusaenList_ARIMA[[4]]$PI, mean))
  table_Damhusaen[5,1] <- mean(sapply(DamhusaenList_ARIMA[[5]]$PI, mean))
  
  table_Damhusaen[1,2] <- mean(sapply(DamhusaenList_ARIMAX[[1]]$PI, mean))
  table_Damhusaen[2,2] <- mean(sapply(DamhusaenList_ARIMAX[[2]]$PI, mean))
  table_Damhusaen[3,2] <- mean(sapply(DamhusaenList_ARIMAX[[3]]$PI, mean))
  table_Damhusaen[4,2] <- mean(sapply(DamhusaenList_ARIMAX[[4]]$PI, mean))
  table_Damhusaen[5,2] <- mean(sapply(DamhusaenList_ARIMAX[[5]]$PI, mean))
  
  
  data <- rbind(table_Damningen, table_Damhusaen)
  data <- melt(data, id.vars = c("Nr", "Station"))
  
  return(data)
}


df <- pullData()
df

plot <- ggplot(df, aes(x = interaction(Nr), y = value, fill =  variable))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_grid(cols = vars(Station))+
  scale_x_discrete(labels=c("1","2","3", "4","5"))+
  ylim(0,1)+
  scale_fill_manual(values = c("#adadad", "#666666"))+
  theme_bw()+
  ylab("PIAVG") + xlab("Model Nr") + labs(fill = "Model type")+
  ggtitle("Comparison top-5 models with and without regressors based on PIAVG")+
  theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
        panel.grid.minor = element_blank(),
        panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
        plot.title = element_text(size = 12), text = element_text(size=12),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.position = "right")
plot


pdf(file = "../Figures/Results/DDS/Barchart_regressors.pdf", height = 3, width = 7)
plot
dev.off()
  

















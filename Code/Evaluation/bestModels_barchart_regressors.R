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

constructTable <- function(station, errorMeasure){
  #Construct table
  table <- as.data.frame(matrix(NA, nrow = 5, ncol = 2))
  colnames(table) <- c("ARIMA", "ARIMAX")
  table$Station <- station
  table$Nr <- 1:5
  
  if (station == "Dæmningen"){
    ListARIMA <- DamningenList_ARIMA
    ListARIMAX <- DamningenList_ARIMAX
  } else
  if (station == "Damhusåen"){
  ListARIMA <- DamhusaenList_ARIMA
  ListARIMAX <- DamhusaenList_ARIMAX
  }else{table <- NULL}
  
  

  #Fill table
  if (errorMeasure == "PI"){
    table[1,1] <- mean(sapply(ListARIMA[[1]]$PI, mean))
    table[2,1] <- mean(sapply(ListARIMA[[2]]$PI, mean))
    table[3,1] <- mean(sapply(ListARIMA[[3]]$PI, mean))
    table[4,1] <- mean(sapply(ListARIMA[[4]]$PI, mean))
    table[5,1] <- mean(sapply(ListARIMA[[5]]$PI, mean))
    
    table[1,2] <- mean(sapply(ListARIMAX[[1]]$PI, mean))
    table[2,2] <- mean(sapply(ListARIMAX[[2]]$PI, mean))
    table[3,2] <- mean(sapply(ListARIMAX[[3]]$PI, mean))
    table[4,2] <- mean(sapply(ListARIMAX[[4]]$PI, mean))
    table[5,2] <- mean(sapply(ListARIMAX[[5]]$PI, mean))
  }else
  if (errorMeasure == "Accuracy"){
    
    table[1,1] <- mean(c(ListARIMA[[1]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMA[[1]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMA[[1]]$accuracy$accuracy90$accuracy_correct))
    table[2,1] <- mean(c(ListARIMA[[2]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMA[[2]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMA[[2]]$accuracy$accuracy90$accuracy_correct))
    table[3,1] <- mean(c(ListARIMA[[3]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMA[[3]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMA[[3]]$accuracy$accuracy90$accuracy_correct))
    table[4,1] <- mean(c(ListARIMA[[4]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMA[[4]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMA[[4]]$accuracy$accuracy90$accuracy_correct))
    table[5,1] <- mean(c(ListARIMA[[5]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMA[[5]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMA[[5]]$accuracy$accuracy90$accuracy_correct))
    
    table[1,2] <- mean(c(ListARIMAX[[1]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMAX[[1]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMAX[[1]]$accuracy$accuracy90$accuracy_correct))
    table[2,2] <- mean(c(ListARIMAX[[2]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMAX[[2]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMAX[[2]]$accuracy$accuracy90$accuracy_correct))
    table[3,2] <- mean(c(ListARIMAX[[3]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMAX[[3]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMAX[[3]]$accuracy$accuracy90$accuracy_correct))
    table[4,2] <- mean(c(ListARIMAX[[4]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMAX[[4]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMAX[[4]]$accuracy$accuracy90$accuracy_correct))
    table[5,2] <- mean(c(ListARIMAX[[5]]$accuracy$accuracy30$accuracy_correct,
                         ListARIMAX[[5]]$accuracy$accuracy60$accuracy_correct,
                         ListARIMAX[[5]]$accuracy$accuracy90$accuracy_correct))
  }else{table <- NULL}

  return(table)
}



tableDamningenPI <- constructTable(station = "Dæmningen", errorMeasure = "PI")
tableDamningenAccuracy <- constructTable(station = "Dæmningen", errorMeasure = "Accuracy")

tableDamhusaenPI <- constructTable(station = "Damhusåen", errorMeasure = "PI")
tableDamhusaenAccuracy <- constructTable(station = "Damhusåen", errorMeasure = "Accuracy")



data_PI <- rbind(tableDamningenPI, tableDamhusaenPI)
data_Accuracy <- rbind(tableDamningenAccuracy, tableDamhusaenAccuracy)


df_PI <- melt(data_PI, id.vars = c("Nr", "Station"))
df_Accuracy <- melt(data_Accuracy, id.vars = c("Nr", "Station"))




plot_PI <- ggplot(df_PI, aes(x = interaction(Nr), y = value, fill =  variable))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_grid(cols = vars(Station))+
  scale_x_discrete(labels=c("1","2","3", "4","5"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.0))+
  scale_fill_manual(values = c("#adadad", "#666666"))+
  theme_bw()+
  ylab("AVG-PI") + xlab("Model Nr") + labs(fill = "Model type")+
  theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
        panel.grid.minor = element_blank(),
        panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
        plot.title = element_text(size = 9), text = element_text(size=9),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        axis.title = element_text(size = 9),
        legend.position = "right")
plot_PI

plot_Accuracy <- ggplot(df_Accuracy, aes(x = interaction(Nr), y = value, fill =  variable))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_grid(cols = vars(Station))+
  scale_x_discrete(labels=c("1","2","3", "4","5"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.0))+
  scale_fill_manual(values = c("#adadad", "#666666"))+
  theme_bw()+
  ylab("AVG-CSI") + xlab("Model Nr") + labs(fill = "Model type")+
  theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
        panel.grid.minor = element_blank(),
        panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"), 
        plot.title = element_text(size = 9), text = element_text(size=9),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.position = "right")
plot_Accuracy

plot <- annotate_figure(ggarrange(plot_PI, plot_Accuracy, ncol = 2, nrow = 1, common.legend = T, legend = "bottom"),
                        text_grob("Comparison top-5 models with-and without regressors based on AVG-PI", size = 10))
plot


tiff(file = "../Figures/Results/DDS/Barchart_regressors.tiff", width = 140, height = 60, units = "mm", res = 300)
plot
dev.off()



















###### FOR DDS


library(reshape2)
library(ggplot2)


## DDS read-in
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

rm(ARIMA_ss_S1, ARIMAX_ss_S1, ARIMA_ms_S1, ARIMAX_ms_S1,
   ARIMA_ss_S2, ARIMAX_ss_S2, ARIMA_ms_S2, ARIMAX_ms_S2, files)



fillData_PI <- function(data){
  df <- as.data.frame(matrix(nrow = length(data), ncol = 3))
  colnames(df) <- c("30 minutes", "60 minutes", "90 minutes")
  for (i in (1:length(data))){
    PI30 <- as.numeric(unlist(data[[i]]$PI$PI30))
    PI60 <- as.numeric(unlist(data[[i]]$PI$PI60))
    PI90 <- as.numeric(unlist(data[[i]]$PI$PI90))
    
    if ((length(PI30) != 0) || (length(PI60) != 0) || (length(PI90) != 0)){
      df$`30 minutes`[i] <- PI30
      df$`60 minutes`[i] <- PI60
      df$`90 minutes`[i] <- PI90
    }
  }
  return(df)
}


fillData_Accuracy <- function(data){
  df <- as.data.frame(matrix(nrow = length(data), ncol = 3))
  colnames(df) <- c("30 minutes", "60 minutes", "90 minutes")
  for (i in (1:length(data))){
    A30 <- as.numeric(unlist(data[[i]]$accuracy$accuracy30$accuracy_correct))
    A60 <- as.numeric(unlist(data[[i]]$accuracy$accuracy60$accuracy_correct))
    A90 <- as.numeric(unlist(data[[i]]$accuracy$accuracy90$accuracy_correct))
    
    if ((length(A30) != 0) || (length(A60) != 0) || (length(A90) != 0)){
      df$`30 minutes`[i] <- A30
      df$`60 minutes`[i] <- A60
      df$`90 minutes`[i] <- A90
    }
  }
  return(df)
}






Damningen_ss_PI <- fillData_PI(DamningenList_ss)
Damningen_ms_PI <- fillData_PI(DamningenList_ms)
Damhusaen_ss_PI <- fillData_PI(DamhusaenList_ss)
Damhusaen_ms_PI <- fillData_PI(DamhusaenList_ms)


Damningen_ss_Accuracy <- fillData_Accuracy(DamningenList_ss)
Damningen_ms_Accuracy <- fillData_Accuracy(DamningenList_ms)
Damhusaen_ss_Accuracy <- fillData_Accuracy(DamhusaenList_ss)
Damhusaen_ms_Accuracy <- fillData_Accuracy(DamhusaenList_ms)


findBestScore <- function(df){
  findBestScore_ <- function(df, FH){
    pos <- which(df[,FH] == max(df[,FH], na.rm = T))
    return(df[pos,])
  }
  
  label <- c("30 min",
             "60 min",
             "90 min")
  data_return <- as.data.frame(matrix(NA, nrow = 3, ncol = 3))
  colnames(data_return) <- colnames(df)
  for (i in (1:3)){
    data_return[i,] <- findBestScore_(df, i)
    data_return$sort[i] <- label[i]
  }
  return(data_return)
}



#### Get best models
Damningen_ss_PI <- findBestScore(Damningen_ss_PI)
Damningen_ms_PI <- findBestScore(Damningen_ms_PI)

Damhusaen_ss_PI <- findBestScore(Damhusaen_ss_PI)
Damhusaen_ms_PI <- findBestScore(Damhusaen_ms_PI)

Damningen_ss_Accuracy <- findBestScore(Damningen_ss_Accuracy)
Damningen_ms_Accuracy <- findBestScore(Damningen_ms_Accuracy)

Damhusaen_ss_Accuracy <- findBestScore(Damhusaen_ss_Accuracy)
Damhusaen_ms_Accuracy <- findBestScore(Damhusaen_ms_Accuracy)




makedf <- function(S1_ss, S1_ms, S2_ss, S2_ms){
  S1_ss$Optimization <- rep("Single-step")
  S1_ms$Optimization <- rep("Multi-step")
  S1 <- rbind(S1_ss, S1_ms)
  
  S2_ss$Optimization <- rep("Single-step")
  S2_ms$Optimization <- rep("Multi-step")
  S2 <- rbind(S2_ss, S2_ms)
  
  S1$Station <- rep("Dæmningen")
  S2$Station <- rep('Damhusåen')
  
  df <- rbind(S1, S2)
  return(melt(df))
}



dataPI <- makedf(S1_ss = Damningen_ss_PI, 
                 S1_ms = Damningen_ms_PI, 
                 S2_ss = Damhusaen_ss_PI,
                 S2_ms = Damhusaen_ms_PI)
dataAccuracy <- makedf(S1_ss = Damningen_ss_Accuracy, 
                       S1_ms = Damningen_ms_Accuracy, 
                       S2_ss = Damhusaen_ss_Accuracy,
                       S2_ms = Damhusaen_ms_Accuracy)


dataPI$Type <- rep("PI")
dataAccuracy$Type <- rep("Accuracy")

data <- rbind(dataPI, dataAccuracy)




plot <- ggplot(data, aes(x = sort, y = value, shape = Optimization, colour = variable, group = interaction(sort, variable)))+
  geom_point(position=position_dodge(0.75), size = 3)+
  scale_shape_manual(values = c(19, 17))+
  geom_line(position=position_dodge(0.75), size=1, colour = "black", alpha = 0.3) +
  scale_colour_manual(values = c("#86BA90", "#4A8FE7", "#F2AF29"))+
  labs(fill = "Models", colour = "Model forecasting horizons")+
  ggtitle("Best performing models based on\nAccuracy/PI for 30 min, 60 min, and 90 min forecasting horizon")+
  facet_grid(cols = vars(Station), rows = vars(Type), scales = "free_y", switch = "y")+
  xlab("Best perfoming model based on different forecasting horizons")+ylab("")+
  scale_y_continuous(minor_breaks = seq(-15 , 1, 0.1), breaks = seq(-15, 1, 0.2))+
  theme_minimal()+
  theme(plot.title = element_text(size = 12), 
        text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.5),
        legend.position = "bottom", legend.box="vertical", legend.margin=margin(), strip.placement = "outside")


plot






ggsave(filename = "../Figures/Results/DDS/Evaluation/best_models_ofc.pdf", width = 8, height = 5)
plot
dev.off()













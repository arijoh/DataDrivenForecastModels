library(ggplot2)
library(reshape2)

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
DamningenList_DDS <- c(ARIMA_ss_S1, ARIMAX_ss_S1, ARIMA_ms_S1, ARIMAX_ms_S1)

ARIMA_ss_S2 <- labelList(ARIMA_ss_S2, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S2 <- labelList(ARIMAX_ss_S2, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S2 <- labelList(ARIMA_ms_S2, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S2 <- labelList(ARIMAX_ms_S2, ofc_ = "multi-step", model_ = "ARIMAX")
DamhusaenList_DDS <- c(ARIMA_ss_S2, ARIMAX_ss_S2, ARIMA_ms_S2, ARIMAX_ms_S2)




# Neleer-Mead read-in
## Singlestep
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)
## Multistep
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)


## take together all models for station
ARIMA_ss_S1 <- labelList(ARIMA_ss_S1, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S1 <- labelList(ARIMAX_ss_S1, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S1 <- labelList(ARIMA_ms_S1, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S1 <- labelList(ARIMAX_ms_S1, ofc_ = "multi-step", model_ = "ARIMAX")
DamningenList_NM <- c(ARIMA_ss_S1, ARIMAX_ss_S1, ARIMA_ms_S1, ARIMAX_ms_S1)

ARIMA_ss_S2 <- labelList(ARIMA_ss_S2, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S2 <- labelList(ARIMAX_ss_S2, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S2 <- labelList(ARIMA_ms_S2, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S2 <- labelList(ARIMAX_ms_S2, ofc_ = "multi-step", model_ = "ARIMAX")
DamhusaenList_NM <- c(ARIMA_ss_S2, ARIMAX_ss_S2, ARIMA_ms_S2, ARIMAX_ms_S2)


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








DamningenList_NM_PI <- fillData_PI(DamningenList_NM)
DamningenList_NM_Accuracy <- fillData_Accuracy(DamningenList_NM)

DamhusaenList_NM_PI <- fillData_PI(DamhusaenList_NM)
DamhusaenList_NM_Accuracy <- fillData_Accuracy(DamhusaenList_NM)

DamningenList_DDS_PI <- fillData_PI(DamningenList_DDS)
DamningenList_DDS_Accuracy <- fillData_Accuracy(DamningenList_DDS)

DamhusaenList_DDS_PI <- fillData_PI(DamhusaenList_DDS)
DamhusaenList_DDS_Accuracy <- fillData_Accuracy(DamhusaenList_DDS)


DamningenList_NM_PI$Station <- rep("Dæmningen")
DamhusaenList_NM_PI$Station <- rep("Damhusåen")
DamningenList_NM_PI$Optimization <- rep("Nelder-Mead")
DamhusaenList_NM_PI$Optimization <- rep("Nelder-Mead")
DamningenList_DDS_PI$Station <- rep("Dæmningen")
DamhusaenList_DDS_PI$Station <- rep("Damhusåen")
DamningenList_DDS_PI$Optimization <- rep("DDS")
DamhusaenList_DDS_PI$Optimization <- rep("DDS")
PI <- rbind(DamningenList_NM_PI, 
            DamhusaenList_NM_PI,
            DamningenList_DDS_PI,
            DamhusaenList_DDS_PI)

DamningenList_NM_Accuracy$Station <- rep("Dæmningen")
DamhusaenList_NM_Accuracy$Station <- rep("Damhusåen")
DamningenList_NM_Accuracy$Optimization <- rep("Nelder-Mead")
DamhusaenList_NM_Accuracy$Optimization <- rep("Nelder-Mead")
DamningenList_DDS_Accuracy$Station <- rep("Dæmningen")
DamhusaenList_DDS_Accuracy$Station <- rep("Damhusåen")
DamningenList_DDS_Accuracy$Optimization <- rep("DDS")
DamhusaenList_DDS_Accuracy$Optimization <- rep("DDS")
Accuracy <- rbind(DamningenList_NM_Accuracy,
                  DamhusaenList_NM_Accuracy,
                  DamningenList_DDS_Accuracy,
                  DamhusaenList_DDS_Accuracy)



rm(DamningenList_NM_PI, DamhusaenList_NM_PI, DamningenList_DDS_PI, DamhusaenList_DDS_PI,
   DamningenList_NM_Accuracy, DamhusaenList_NM_Accuracy, DamningenList_DDS_Accuracy, DamhusaenList_DDS_Accuracy)



PI_melt <- melt(PI)

plotViolin_PI <- function(data, title, zoom_to){
  p1 <- ggplot(data, aes(x = variable, y = value, fill = Optimization)) +
    geom_violin(position=position_dodge(0.75), scale = "width", width = 0.65) +
    scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
    geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
    stat_summary(fun=mean, geom="point", shape=19, size=1.25, color = "blue", position=position_dodge(0.75))+
    stat_summary(fun=median, geom="point", shape=19, size=1.25, color = 'red', position=position_dodge(0.75))+
    labs(fill = "Models")+
    ggtitle(title)+
    facet_grid(cols = vars(Station))+
    xlab("")+ylab("PI")+
    theme_minimal()+
    theme(plot.title = element_text(size = 14), 
          text = element_text(size=14),
          axis.text.x = element_blank(),
          legend.position = "bottom")+
    scale_y_continuous(minor_breaks = seq(-80 , 1, 10), breaks = seq(-80, 1, 20))+
    coord_cartesian(ylim=c(-80,1))
  
  p2 <- ggplot(data, aes(x = variable, y = value, fill = Optimization)) +
    geom_violin(position=position_dodge(0.75), scale = "width", width = 0.65) +
    scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
    geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
    stat_summary(fun=mean, geom="point", shape=19, size=1.25, color = "blue", position=position_dodge(0.75))+
    stat_summary(fun=median, geom="point", shape=19, size=1.25, color = 'red', position=position_dodge(0.75))+
    labs(fill = "Models")+
    ggtitle("Y-axis adjusted")+
    facet_grid(cols = vars(Station))+
    xlab("Forecast horizon")+ylab("PI")+
    theme_minimal()+
    theme(plot.title = element_text(size = 14), 
          text = element_text(size=14),
          strip.text.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom")+
    scale_y_continuous(minor_breaks = seq(-1 , 1, 0.1), breaks = seq(-1, 1, 0.2))+
    coord_cartesian(ylim=c(0,0.6))
  
  
  plot <- ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "bottom")
  return(plot)
}

PIplot <- plotViolin_PI(PI_melt, title = "PI distributions of Nelder-Mead and DDS\nfor different forecasting horizons")





Accuracy_melt <- melt(Accuracy)

plotViolin_Accuracy <- function(data, title, zoom_to){
  p1 <- ggplot(data, aes(x = interaction(variable), y = value, fill = Optimization)) +
    geom_violin(position=position_dodge(0.75), scale = "width", width = 0.65) +
    scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
    geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
    stat_summary(fun=mean, geom="point", shape=19, size=1.25, color = "blue", position=position_dodge(0.75))+
    stat_summary(fun=median, geom="point", shape=19, size=1.25, color = 'red', position=position_dodge(0.75))+
    labs(fill = "Models")+
    ggtitle(title)+
    facet_grid(cols = vars(Station))+
    xlab("Forecast horizon")+ylab("Accuracy")+
    theme_minimal()+
    theme(plot.title = element_text(size = 12), 
          text = element_text(size=12),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom")+
    scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.2))+
    coord_cartesian(ylim=c(0,1))

  return(p1)
}

Aplot <- plotViolin_Accuracy(Accuracy_melt, title = "Accuracy distributions of Nelder-Mead and DDS\nfor different forecasting horizons", zoom_to = -1)




ggsave(filename = "../Figures/Results/Comparison/Evaluation/PI_distributions_optim.pdf", width = 8, height =5)
PIplot
dev.off()

ggsave(filename = "../Figures/Results/Comparison/Evaluation/Accuracy_distributions_optim.pdf",  width = 8, heigh = 4)
Aplot
dev.off()









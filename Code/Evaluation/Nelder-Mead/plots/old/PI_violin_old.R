library(ggplot2)
library(ggpubr)
library(dplyr)
library(reshape2)

####### Everything is Nelder-Mead

####################### Multistep Nelder-Mead
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

####################### Single-step Nelder-Mead
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)







fillData <- function(data, model_type, station){
  df <- as.data.frame(matrix(nrow = length(data), ncol = 5))
  colnames(df) <- c("Type", "Station", "PI30", "PI60", "PI90")
  for (i in (1:length(data))){
    df$Type[i] <- model_type
    df$Station[i] <- station
    PI30 <- as.numeric(unlist(data[[i]]$PI$PI30))
    PI60 <- as.numeric(unlist(data[[i]]$PI$PI60))
    PI90 <- as.numeric(unlist(data[[i]]$PI$PI90))
    
    if ((length(PI30) != 0) || (length(PI60) != 0) || (length(PI90) != 0)){
      df$PI30[i] <- PI30
      df$PI60[i] <- PI60
      df$PI90[i] <- PI90
    }
  }
  return(df)
}


## Multi-step
ARIMA_ms_S1 <- fillData(ARIMA_ms_S1, model_type = "ARIMA\nmulti-step", station = "Dæmmningen")
ARIMA_ms_S2 <- fillData(ARIMA_ms_S2, model_type = "ARIMA\nmulti-step", station = "Damhusåen")
ARIMAX_ms_S1 <- fillData(ARIMAX_ms_S1, model_type = "ARIMAX\nmulti-step", station = "Dæmmningen")
ARIMAX_ms_S2 <- fillData(ARIMAX_ms_S2, model_type = "ARIMAX\nmulti-step", station = "Damhusåen")
## Single-step
ARIMA_ss_S1 <- fillData(ARIMA_ss_S1, model_type = "ARIMA\nsingle-step", station = "Dæmmningen")
ARIMA_ss_S2 <- fillData(ARIMA_ss_S2, model_type = "ARIMA\nsingle-step", station = "Damhusåen")
ARIMAX_ss_S1 <- fillData(ARIMAX_ss_S1, model_type = "ARIMAX\nsingle-step", station = "Dæmmningen")
ARIMAX_ss_S2 <- fillData(ARIMAX_ss_S2, model_type = "ARIMAX\nsingle-step", station = "Damhusåen")

data <- rbind(ARIMA_ms_S1, ARIMA_ms_S2, ARIMAX_ms_S1, ARIMAX_ms_S2,
              ARIMA_ss_S1, ARIMA_ss_S2, ARIMAX_ss_S1, ARIMAX_ss_S2)

df <- melt(data)


###########################################  Function to plot
plotViolin <- function(data, title){
  plot <- ggplot(data, aes(x = Type, y = value, fill = variable)) +
    geom_violin(position=position_dodge(0.75), scale = "width", width = 0.65) +
     scale_fill_manual(values=c("#CAD593", "#A1C349", "#758E29"),
                       labels = c("30 minutes", "60 minutes", "90 minutes"))+
    geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
    stat_summary(fun=mean, geom="point", shape=19, size=1.25, color = "blue", position=position_dodge(0.75))+
    stat_summary(fun=median, geom="point", shape=19, size=1.25, color = 'red', position=position_dodge(0.75))+
    labs(fill = "Forecasting horizon")+
    ggtitle(title)+
    facet_grid(rows = vars(Station))+
    xlab("Models")+ylab("Persistence index")+
    theme_minimal()+
    theme(plot.title = element_text(size = 20), 
          text = element_text(size=16),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom")+
    scale_y_continuous(minor_breaks = seq(-15 , 1, 1), breaks = seq(-15, 1, 5))+
    coord_cartesian(ylim=c(-15,1))
  return(plot)
}


plot <- plotViolin(df, title = "Persistence index distribution for different model types")
plot


svg(filename = "../Figures/Results/Nelder-Mead/PI_violinplot_new.svg", width = 8, height = 4)
plot
dev.off()


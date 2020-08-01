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

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

####################### Single-step Nelder-Mead
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)




fillData_PI <- function(data, model_type, station){
  df <- as.data.frame(matrix(nrow = length(data), ncol = 5))
  colnames(df) <- c("Type", "Station", "30 minutes", "60 minutes", "90 minutes")
  for (i in (1:length(data))){
    df$Type[i] <- model_type
    df$Station[i] <- station
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


fillData_Accuracy <- function(data, model_type, station){
  df <- as.data.frame(matrix(nrow = length(data), ncol = 5))
  colnames(df) <- c("Type", "Station", "30 minutes", "60 minutes", "90 minutes")
  for (i in (1:length(data))){
    df$Type[i] <- model_type
    df$Station[i] <- station
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


##### PI fill
## Multi-step
ARIMA_ms_S1_temp <- fillData_PI(ARIMA_ms_S1, model_type = "ARIMA\nmulti-step", station = "Dæmningen")
ARIMA_ms_S2_temp <- fillData_PI(ARIMA_ms_S2, model_type = "ARIMA\nmulti-step", station = "Damhusåen")
ARIMAX_ms_S1_temp <- fillData_PI(ARIMAX_ms_S1, model_type = "ARIMAX\nmulti-step", station = "Dæmningen")
ARIMAX_ms_S2_temp <- fillData_PI(ARIMAX_ms_S2, model_type = "ARIMAX\nmulti-step", station = "Damhusåen")
## Single-step
ARIMA_ss_S1_temp <- fillData_PI(ARIMA_ss_S1, model_type = "ARIMA\nsingle-step", station = "Dæmningen")
ARIMA_ss_S2_temp <- fillData_PI(ARIMA_ss_S2, model_type = "ARIMA\nsingle-step", station = "Damhusåen")
ARIMAX_ss_S1_temp <- fillData_PI(ARIMAX_ss_S1, model_type = "ARIMAX\nsingle-step", station = "Dæmningen")
ARIMAX_ss_S2_temp <- fillData_PI(ARIMAX_ss_S2, model_type = "ARIMAX\nsingle-step", station = "Damhusåen")

data <- rbind(ARIMA_ms_S1_temp, ARIMA_ms_S2_temp, ARIMAX_ms_S1_temp, ARIMAX_ms_S2_temp,
              ARIMA_ss_S1_temp, ARIMA_ss_S2_temp, ARIMAX_ss_S1_temp, ARIMAX_ss_S2_temp)

data$errormetric <- rep("PI")

df_PI <- melt(data)

###### Accuracy fill
## Multi-step
ARIMA_ms_S1_temp <- fillData_Accuracy(ARIMA_ms_S1, model_type = "ARIMA\nmulti-step", station = "Dæmningen")
ARIMA_ms_S2_temp <- fillData_Accuracy(ARIMA_ms_S2, model_type = "ARIMA\nmulti-step", station = "Damhusåen")
ARIMAX_ms_S1_temp <- fillData_Accuracy(ARIMAX_ms_S1, model_type = "ARIMAX\nmulti-step", station = "Dæmningen")
ARIMAX_ms_S2_temp <- fillData_Accuracy(ARIMAX_ms_S2, model_type = "ARIMAX\nmulti-step", station = "Damhusåen")
## Single-step
ARIMA_ss_S1_temp <- fillData_Accuracy(ARIMA_ss_S1, model_type = "ARIMA\nsingle-step", station = "Dæmningen")
ARIMA_ss_S2_temp <- fillData_Accuracy(ARIMA_ss_S2, model_type = "ARIMA\nsingle-step", station = "Damhusåen")
ARIMAX_ss_S1_temp <- fillData_Accuracy(ARIMAX_ss_S1, model_type = "ARIMAX\nsingle-step", station = "Dæmningen")
ARIMAX_ss_S2_temp <- fillData_Accuracy(ARIMAX_ss_S2, model_type = "ARIMAX\nsingle-step", station = "Damhusåen")


data <- rbind(ARIMA_ms_S1_temp,
              ARIMA_ms_S2_temp,
              ARIMAX_ms_S1_temp,
              ARIMAX_ms_S2_temp,
              ARIMA_ss_S1_temp,
              ARIMA_ss_S2_temp,
              ARIMAX_ss_S1_temp,
              ARIMAX_ss_S2_temp)

data$errormetric <- rep("Accuracy")

###
df_Accuracy <- melt(data)


df <- rbind(df_PI,
            df_Accuracy)

df$Type <- factor(df$Type, levels = c("ARIMA\nsingle-step", "ARIMA\nmulti-step", "ARIMAX\nsingle-step", "ARIMAX\nmulti-step"))


plot_PI <- ggplot(df_PI, aes(x = variable, y = value,
                             fill = factor(Type, levels = c("ARIMA\nsingle-step", "ARIMA\nmulti-step", "ARIMAX\nsingle-step", "ARIMAX\nmulti-step"))), group = interaction(Type, variable)) +
  geom_violin(position=position_dodge(0.75), scale = "width", width = 0.65)+
  scale_fill_manual(values=c("#70A288", "#DAB785", "#D5896F", "#62929E"))+
  geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
  stat_summary(fun=mean, geom="point", shape=19, size=1.25, color = "blue", position=position_dodge(0.75))+
  stat_summary(fun=median, geom="point", shape=19, size=1.25, color = 'red', position=position_dodge(0.75))+
  labs(fill = "Models")+
  facet_grid(cols = vars(Station))+
  xlab("Forecasting horizon")+ylab("PI")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20), 
        text = element_text(size=16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom")+
  scale_y_continuous(minor_breaks = seq(-3 , 1, 0.25), breaks = seq(-3, 1, 0.5), limits = c(-3, 1))

plot_Accuracy <- ggplot(df_Accuracy, aes(x = variable, y = value,
                                         fill = factor(Type, levels = c("ARIMA\nsingle-step", "ARIMA\nmulti-step", "ARIMAX\nsingle-step", "ARIMAX\nmulti-step"))), group = interaction(Type, variable)) +
  geom_violin(position=position_dodge(0.75), scale = "width", width = 0.65)+
  scale_fill_manual(values=c("#70A288", "#DAB785", "#D5896F", "#62929E"))+
  geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
  stat_summary(fun=mean, geom="point", shape=19, size=1.25, color = "blue", position=position_dodge(0.75))+
  stat_summary(fun=median, geom="point", shape=19, size=1.25, color = 'red', position=position_dodge(0.75))+
  labs(fill = "Models")+
  facet_grid(cols = vars(Station))+
  xlab("Forecasting horizon")+ylab("Accuracy")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20), 
        axis.title.x=element_blank(),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
        strip.text.x = element_blank(),
        legend.position = "bottom")+
  scale_y_continuous(minor_breaks = seq(0 , 1, 0.1), breaks = seq(0, 1, 0.2), limits = c(0, 1))

plot <- annotate_figure(ggarrange(plot_PI, plot_Accuracy, nrow = 2, common.legend = T, legend = "bottom"),
                        top = text_grob("Perfomance of Model Types", size = 20))
plot





ggsave(filename = "../Figures/Results/Nelder-Mead/Evaluation/EvaluationDistributionOFC.pdf", width = 8, height = 5)
plot
dev.off()





library(ggplot2)
library(reshape)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

getValues <- function(L){
  data <- data.frame(matrix(NA, nrow = length(L), ncol = 2))
  colnames(data) <- c("reg.lag", "objval")
  for (i in (1:length(L))){
    temp <- unlist(L[[i]]$reg.lag)
    if (!is.null(temp)){
      data$reg.lag[i] <- temp
    }else{data$reg.lag[i] <- 0}
    temp <- unlist(L[[i]]$value)
    if (!is.null(temp)){
      data$objval[i] <- temp
    }
    
  }
  #data <- melt(data)
  return(data)
}


data_ARIMA_ss_S1 <- getValues(L = ARIMA_ss_S1)
data <- as.data.frame(data_ARIMA_ss_S1)
data$station <- rep("Dammning")

data_ARIMA_ms_S1 <- getValues(L = ARIMA_ms_S1)
data_temp <- as.data.frame(data_ARIMA_ms_S1)
data_temp$station <- rep("Dammning")
data <- rbind(data, data_temp)

data_ARIMAX_ss_S1 <- getValues(L = ARIMAX_ss_S1)
data_temp <- as.data.frame(data_ARIMAX_ss_S1)
data_temp$station <- rep("Dammning")
data <- rbind(data, data_temp)

data_ARIMAX_ms_S1 <- getValues(L = ARIMAX_ms_S1)
data_temp <- as.data.frame(data_ARIMAX_ms_S1)
data_temp$station <- rep("Dammning")
data <- rbind(data, data_temp)

data_ARIMA_ss_S2 <- getValues(L = ARIMA_ss_S2)
data_temp <- as.data.frame(data_ARIMA_ss_S2)
data_temp$station <- rep("Damhusaen")
data <- rbind(data, data_temp)

data_ARIMA_ms_S2 <- getValues(L = ARIMA_ms_S2)
data_temp <- as.data.frame(data_ARIMA_ms_S2)
data_temp$station <- rep("Damhusaen")
data <- rbind(data, data_temp)

data_ARIMAX_ss_S2 <- getValues(L = ARIMAX_ss_S2)
data_temp <- as.data.frame(data_ARIMAX_ss_S2)
data_temp$station <- rep("Damhusaen")
data <- rbind(data, data_temp)

data_ARIMAX_ms_S2 <- getValues(L = ARIMAX_ms_S2)
data_temp <- as.data.frame(data_ARIMAX_ms_S2)
data_temp$station <- rep("Damhusaen")
data <- rbind(data, data_temp)

data$reg.lag <- factor(data$reg.lag, c("0", "2", "4", "5", "6",  "8", "10", "15"))
data$station <- factor(data$station)

plot_nm <- ggplot(data, aes(x = reg.lag, y = objval, fill = station, group = interaction(station, reg.lag)))+
  geom_violin()+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  geom_boxplot(width=0.1, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.9))+
  stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue", position=position_dodge(0.9))+
  stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red', position=position_dodge(0.9))+
  ggtitle("Nelder-Mead")+
  xlab("Lag of external regressor") + ylab("Obj.fun")+
  theme_minimal()+
  theme(plot.title = element_text(size = 16), 
        text = element_text(size=16),
        legend.position = "none")+
  coord_cartesian(ylim=c(0,15))+
  facet_grid(cols = vars(station))
plot_nm


# svg(filename="../Figures/Results/Nelder-Mead/reglag_vs_objfun.svg", width = 8, heigh = 4)
# plot
# dev.off()








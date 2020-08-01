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
  data <- data.frame(matrix(NA, nrow = length(L), ncol =3))
  colnames(data) <- c("PI30", "PI60", "PI90")
  for (i in (1:length(L))){
    temp <- unlist(L[[i]]$PI)
    if (!is.null(temp)){
      data[i,] <- temp
    }
  }
  data <- melt(data)
  return(data)
}


data_ARIMA_ss_S1 <- getValues(L = ARIMA_ss_S1)
data <- as.data.frame(data_ARIMA_ss_S1)
data$type <- rep("ARIMA\nsingle-step")
data$station <- rep("Dammning")

data_ARIMA_ms_S1 <- getValues(L = ARIMA_ms_S1)
data_temp <- as.data.frame(data_ARIMA_ms_S1)
data_temp$type <- rep("ARIMA\nmulti-step")
data_temp$station <- rep("Dammning")
data <- rbind(data, data_temp)

data_ARIMAX_ss_S1 <- getValues(L = ARIMAX_ss_S1)
data_temp <- as.data.frame(data_ARIMAX_ss_S1)
data_temp$type <- rep("ARIMAX\nsingle-step")
data_temp$station <- rep("Dammning")
data <- rbind(data, data_temp)

data_ARIMAX_ms_S1 <- getValues(L = ARIMAX_ms_S1)
data_temp <- as.data.frame(data_ARIMAX_ms_S1)
data_temp$type <- rep("ARIMAX\nmulti-step")
data_temp$station <- rep("Dammning")
data <- rbind(data, data_temp)

data_ARIMA_ss_S2 <- getValues(L = ARIMA_ss_S2)
data_temp <- as.data.frame(data_ARIMA_ss_S2)
data_temp$type <- rep("ARIMA\nsingle-step")
data_temp$station <- rep("Damhusaen")
data <- rbind(data, data_temp)

data_ARIMA_ms_S2 <- getValues(L = ARIMA_ms_S2)
data_temp <- as.data.frame(data_ARIMA_ms_S2)
data_temp$type <- rep("ARIMA\nmulti-step")
data_temp$station <- rep("Damhusaen")
data <- rbind(data, data_temp)

data_ARIMAX_ss_S2 <- getValues(L = ARIMAX_ss_S2)
data_temp <- as.data.frame(data_ARIMAX_ss_S2)
data_temp$type <- rep("ARIMAX\nsingle-step")
data_temp$station <- rep("Damhusaen")
data <- rbind(data, data_temp)

data_ARIMAX_ms_S2 <- getValues(L = ARIMAX_ms_S2)
data_temp <- as.data.frame(data_ARIMAX_ms_S2)
data_temp$type <- rep("ARIMAX\nmulti-step")
data_temp$station <- rep("Damhusaen")
data <- rbind(data, data_temp)


plot <- ggplot(data, aes(x = type, y = (value), fill = variable, group = interaction(type, variable))) +
  geom_violin(position=position_dodge(0.75))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_boxplot(width=0.15, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
  stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue", position=position_dodge(0.75))+
  stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red', position=position_dodge(0.75))+
  scale_y_continuous(limits=c(-10,1), breaks = seq(-10, 1, 1))+
  labs(title="PI distributions for all models ",x="", y = "PI")+
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 20),
        text = element_text(size=18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  coord_cartesian(ylim=c(-6,1))+
  facet_grid(cols = vars(station))
plot

# svg(filename="../Figures/Results/Nelder-Mead/PI_violinplot.svg", width = 8, heigh = 4)
# plot
# dev.off()








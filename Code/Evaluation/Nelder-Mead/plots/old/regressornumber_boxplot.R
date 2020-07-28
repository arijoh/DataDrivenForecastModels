library(ggplot2)
library(reshape)


# THIS CODE ONLY TAKES PI90 FOR STATION 1, DAMMNING - MIGHT NEED TO HAVE DAMHUSAEN ALSO


files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)


getValues <- function(L){
  data <- data.frame(matrix(0, nrow = length(L), ncol = 3))
  colnames(data) <- c("PI", "reg.nr", "reg.lag")
  for (i in (1:length(L))){
    PI <- as.numeric(unlist(L[[i]]$PI[3]))
    reg.nr <- as.numeric(unlist(L[[i]]$reg.nr))
    reg.lag <- as.numeric(unlist(L[[i]]$reg.lag))
    
    if (length(PI) != 0){ data$PI[i] <- PI } else{  data$PI[i] <- 0}
    if (length(reg.nr) != 0){ data$reg.nr[i] <- reg.nr } else{  data$reg.nr[i] <- 0}
    if (length(reg.lag) != 0){ data$reg.lag[i] <- reg.lag } else{  data$reg.lag[i] <- 0}
  }
  return(data)
}


data_noreg <- getValues(ARIMA_ms_S1)
data_reg <- getValues(ARIMAX_ms_S1)

data <- rbind(data_noreg, data_reg)

data$reg.nr <- factor(data$reg.nr)
data$reg.lag <- factor(data$reg.lag)

plot <- ggplot(data, aes(x =reg.nr, y = PI, fill =reg.lag, group = interaction(reg.nr, reg.lag)))+
  geom_violin(position=position_dodge(0.75))+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_boxplot(width=0.15, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
  stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue", position=position_dodge(0.75))+
  stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red', position=position_dodge(0.75))+
  scale_y_continuous(limits=c(-6, 1), breaks=seq(-6,1,1))+
  labs(title="PI90 for different number of regressors",x="Number of regressors", y = "PI90")+
  theme_minimal()
plot


# svg(filename = "../Figures/Results/PI90_vs_regnum_reglag.svg", width = 8, heigh = 4)
# plot
# dev.off()













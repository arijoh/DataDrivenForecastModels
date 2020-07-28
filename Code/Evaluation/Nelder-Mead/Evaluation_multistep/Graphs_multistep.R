library(ggplot2)
library(ggpubr)
library(dplyr)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S1 <- lapply(paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S2 <- lapply(paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S1 <- lapply(paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S2 <- lapply(paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

data_ARIMA_S1 <- as.data.frame(matrix(nrow = length(ARIMA_S1), ncol =4))
data_ARIMA_S2 <- as.data.frame(matrix(nrow = length(ARIMA_S2), ncol =4))
data_ARIMAX_S1 <- as.data.frame(matrix(nrow = length(ARIMAX_S1), ncol =4))
data_ARIMAX_S2 <- as.data.frame(matrix(nrow = length(ARIMAX_S2), ncol =4))
colnames(data_ARIMA_S1) <- c("n", "Hours", "Value", "Conv")
colnames(data_ARIMA_S2) <- c("n", "Hours", "Value", "Conv")
colnames(data_ARIMAX_S1) <- c("n", "Hours", "Value", "Conv")
colnames(data_ARIMAX_S2) <- c("n", "Hours", "Value", "Conv")

for (i in (1:length(ARIMA_S1))){
  data_ARIMA_S1[i,1] <- length(unlist(ARIMA_S1[[i]]$par))
  data_ARIMA_S1[i,2] <- as.numeric(unlist(ARIMA_S1[[i]]$Time), units="hours")
  if (!is.null(unlist(ARIMA_S1[[i]]$value))){
    data_ARIMA_S1[i,3] <- unlist(ARIMA_S1[[i]]$value)
  } else{ data_ARIMA_S1[i,3] <- NA}
  if (!is.null(unlist(ARIMA_S1[[i]]$convergence))){
    data_ARIMA_S1[i,4] <- unlist(ARIMA_S1[[i]]$convergence)
  } else{ data_ARIMA_S1[i,4] <- NA}
}
for (i in (1:length(ARIMA_S2))){
  data_ARIMA_S2[i,1] <- length(unlist(ARIMA_S2[[i]]$par))
  data_ARIMA_S2[i,2] <- as.numeric(unlist(ARIMA_S2[[i]]$Time), units="hours")
  if (!is.null(unlist(ARIMA_S2[[i]]$value))){
    data_ARIMA_S2[i,3] <- unlist(ARIMA_S2[[i]]$value)
  } else{ data_ARIMA_S2[i,3] <- NA}
  if (!is.null(unlist(ARIMA_S2[[i]]$convergence))){
    data_ARIMA_S2[i,4] <- unlist(ARIMA_S2[[i]]$convergence)
  } else{ data_ARIMA_S2[i,4] <- NA}
}
for (i in (1:length(ARIMAX_S1))){
  data_ARIMAX_S1[i,1] <- length(unlist(ARIMAX_S1[[i]]$par))
  data_ARIMAX_S1[i,2] <- as.numeric(unlist(ARIMAX_S1[[i]]$Time), units="hours")
  if (!is.null(unlist(ARIMAX_S1[[i]]$value))){
    data_ARIMAX_S1[i,3] <- unlist(ARIMAX_S1[[i]]$value)
  } else{ data_ARIMAX_S1[i,3] <- NA}
  if (!is.null(unlist(ARIMAX_S1[[i]]$convergence))){
    data_ARIMAX_S1[i,4] <- unlist(ARIMAX_S1[[i]]$convergence)
  } else{ data_ARIMAX_S1[i,4] <- NA}
}
for (i in (1:length(ARIMAX_S2))){
  data_ARIMAX_S2[i,1] <- length(unlist(ARIMAX_S2[[i]]$par))
  data_ARIMAX_S2[i,2] <- as.numeric(unlist(ARIMAX_S1[[i]]$Time), units="hours")
  if (!is.null(unlist(ARIMAX_S2[[i]]$value))){
    data_ARIMAX_S2[i,3] <- unlist(ARIMAX_S2[[i]]$value)
  } else{ data_ARIMAX_S2[i,3] <- NA}
  if (!is.null(unlist(ARIMAX_S2[[i]]$convergence))){
    data_ARIMAX_S2[i,4] <- unlist(ARIMAX_S2[[i]]$convergence)
  } else{ data_ARIMAX_S2[i,4] <- NA}
}

## Histogramover model complexity
hist_ARIMA_S1 <- ggplot(data_ARIMA_S1, aes(x=n))+
  geom_histogram(binwidth=1, fill=I("blue"), col=I("black"), alpha=I(.5)) +
  ggtitle("ARIMA \n Damning")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20), text = element_text(size=20))

hist_ARIMA_S2 <- ggplot(data_ARIMA_S2, aes(x=n))+
  geom_histogram(binwidth=1, fill=I("blue"), col=I("black"), alpha=I(.5)) +
  ggtitle("ARIMA \n Damhusåen")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20), text = element_text(size=20))

hist_ARIMAX_S1 <- ggplot(data_ARIMAX_S1, aes(x=n))+
  geom_histogram(binwidth=1, fill=I("blue"), col=I("black"), alpha=I(.5)) +
  ggtitle("ARIMAX \n Damning")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20), text = element_text(size=20))

hist_ARIMAX_S2 <- ggplot(data_ARIMAX_S2, aes(x=n))+
  geom_histogram(binwidth=1, fill=I("blue"), col=I("black"), alpha=I(.5)) +
  ggtitle("ARIMAX \n Damhusåen")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20), text = element_text(size=20))

coefficient_distr <- annotate_figure(ggarrange(hist_ARIMA_S1, hist_ARIMA_S2, hist_ARIMAX_S1, hist_ARIMAX_S1),
                top = text_grob("Number of optimized parameters", size= 22))

# svg(filename="../Figures/Results/coeff_dists.svg")
# coefficient_distr
# dev.off()


# Basic barplot
data_agr_ARIMA_S1 <- aggregate(data_ARIMA_S1, list(data_ARIMA_S1$n), mean)[,-1]
data_agr_ARIMA_S2 <- aggregate(data_ARIMA_S2, list(data_ARIMA_S2$n), mean)[,-1]
data_agr_ARIMAX_S1 <- aggregate(data_ARIMAX_S1, list(data_ARIMAX_S1$n), mean)[,-1]
data_agr_ARIMAX_S2 <- aggregate(data_ARIMAX_S2, list(data_ARIMAX_S2$n), mean)[,-1]

time_ARIMA_S1 <-ggplot(data=data_agr_ARIMA_S1, aes(x=n, y=Hours)) +
  geom_bar(stat="identity", fill=I("blue"), col=I("black"), alpha=I(.5))+
  ggtitle("ARIMA \n Damning")+
  theme_minimal()+
  theme(plot.title = element_text(size = 24), text = element_text(size=24))

time_ARIMA_S2 <-ggplot(data=data_agr_ARIMA_S2, aes(x=n, y=Hours)) +
  geom_bar(stat="identity", fill=I("blue"), col=I("black"), alpha=I(.5))+
  ggtitle("ARIMA \n Damhusåen")+
  theme_minimal()+
  theme(plot.title = element_text(size = 24), text = element_text(size=24))

time_ARIMAX_S1 <-ggplot(data=data_agr_ARIMAX_S1, aes(x=n, y=Hours)) +
  geom_bar(stat="identity", fill=I("blue"), col=I("black"), alpha=I(.5))+
  ggtitle("ARIMAX \n Damning")+
  theme_minimal()+
  theme(plot.title = element_text(size = 24), text = element_text(size=24))

time_ARIMAX_S2 <-ggplot(data=data_agr_ARIMAX_S2, aes(x=n, y=Hours)) +
  geom_bar(stat="identity", fill=I("blue"), col=I("black"), alpha=I(.5))+
  ggtitle("ARIMAX \n Damhusåen")+
  theme_minimal()+
  theme(plot.title = element_text(size = 24), text = element_text(size=24))

comptime_multistep <- annotate_figure(ggarrange(time_ARIMA_S1, time_ARIMA_S2, time_ARIMAX_S1, time_ARIMAX_S2), 
                top = text_grob("Computation time", size = 30))

# svg(filename="../Figures/Results/comptime_multistep.svg")
# comptime_multistep
# dev.off()

nr_ARIMA <- 162
nr_ARIMAX <- 2430


#### Convergence..
conv_ARIMA_S1 <- count(data_ARIMA_S1, Conv)
if (sum(conv_ARIMA_S1$n) < nr_ARIMA){ conv_ARIMA_S1$n[3] <- conv_ARIMA_S1$n[3] +  nr_ARIMA - sum(conv_ARIMA_S1$n)}
conv_ARIMA_S1$type <- rep('ARIMA \n Damning', 3)
conv_ARIMA_S2 <- count(data_ARIMA_S2, Conv)
if (sum(conv_ARIMA_S2$n) < nr_ARIMA){ conv_ARIMA_S2$n[3] <- conv_ARIMA_S2$n[3] +  nr_ARIMA - sum(conv_ARIMA_S2$n)}
conv_ARIMA_S2$type <- rep('ARIMA \n Damhusåen', 3)
conv_ARIMAX_S1 <- count(data_ARIMAX_S1, Conv)
if (sum(conv_ARIMAX_S1$n) < nr_ARIMAX){ conv_ARIMAX_S1$n[3] <- conv_ARIMAX_S1$n[3] +  nr_ARIMAX - sum(conv_ARIMAX_S1$n)}
conv_ARIMAX_S1$type <- rep('ARIMAX \n Damning', 3)
conv_ARIMAX_S2 <- count(data_ARIMAX_S2, Conv)
if (sum(conv_ARIMAX_S2$n) < nr_ARIMAX){ conv_ARIMAX_S2$n[3] <- conv_ARIMAX_S2$n[3] +  nr_ARIMAX - sum(conv_ARIMAX_S2$n)}
conv_ARIMAX_S2$type <- rep('ARIMAX \n Damhusåen', 3)
data <- rbind(conv_ARIMA_S1, conv_ARIMA_S2, conv_ARIMAX_S1, conv_ARIMAX_S2)

# normalize data to get %
data$n[1:3] <- (data$n[1:3]/(sum(data$n[1:3])))*100
data$n[4:6] <- (data$n[4:6]/(sum(data$n[4:6])))*100
data$n[7:9] <- (data$n[7:9]/(sum(data$n[7:9])))*100
data$n[10:12] <- (data$n[10:12]/(sum(data$n[10:12])))*100
data

data$Conv[which(data$Conv == 0)] <- "Converged"
data$Conv[which(data$Conv == 1)] <- "Not converged"

##### ADD MISSED MODELS HERE AS WELL
conv_plot_2 <- ggplot(data = data, aes(fill=as.character(Conv), y=n, x=type)) + 
  geom_bar(position="stack", stat="identity", alpha=I(.9))+
  scale_fill_manual(values = c("Converged" = "darkgoldenrod2",
                               "Not converged" = "steelblue"), 
                    na.value = "firebrick4", name= "Convergence")+
  ggtitle("Convergences for \nmulti-step optimizations")+
  theme_minimal()+
  theme(plot.title = element_text(size = 16), text = element_text(size=16))+
  ylab("%") + xlab("")  + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

# svg(filename = "../Figures/Results/conv_multistep.svg")
# conv_plot_2
# dev.off()

### Combine single and multistep figure

conv_plot <- ggarrange(conv_plot_1, conv_plot_2, common.legend = T, legend="bottom")
svg(filename = "../Figures/Results/Nelder-Mead/conv_both.svg", width = 8, height = 5)
conv_plot
dev.off()


### Dammning
data_ARIMA_S1$type <- rep("ARIMA", nrow(data_ARIMA_S1))
data_ARIMAX_S1$type <- rep("ARIMAX", nrow(data_ARIMAX_S1))
data_station1 <- rbind(data_ARIMA_S1, data_ARIMAX_S1)
data_station1$n <- as.factor(data_station1$n)


### Damnusaen 
data_ARIMA_S2$type <- rep("ARIMA", nrow(data_ARIMA_S2))
data_ARIMAX_S2$type <- rep("ARIMAX", nrow(data_ARIMAX_S2))
data_station2 <- rbind(data_ARIMA_S2, data_ARIMAX_S2)
data_station2$n <- as.factor(data_station2$n)


## PLots
p1_ms <- ggplot(data_station1, aes(x = n, y = Value, fill = type, group = interaction(n, type)))+
  geom_violin()+
  geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black",  position=position_dodge(0.9))+
  stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue",  position=position_dodge(0.9))+
  stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red',  position=position_dodge(0.9))+
  ggtitle("Multi-step")+theme_minimal()+
  theme(plot.title = element_text(size = 18), 
        text = element_text(size=16))+
  labs(fill = "Model type calibration")+
  xlab("Nr. of hyperparameters")+ylab("Sum-of-square")+
  coord_cartesian(ylim=c(0,10))


p2_ms <- ggplot(data_station2, aes(x = n, y = Value, fill = type, group = interaction(n, type)))+
  geom_violin()+
  geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black",  position=position_dodge(0.9))+
  stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue",  position=position_dodge(0.9))+
  stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red',  position=position_dodge(0.9))+
  ggtitle("Multi-step calibration")+theme_minimal()+
  theme(plot.title = element_text(size = 18), 
        text = element_text(size=16))+
  labs(fill = "Model type")+
  xlab("Nr. of hyperparameters")+ylab("Sum-of-square")+
  coord_cartesian(ylim=c(0,15))


# p_ms <- annotate_figure(ggarrange(p1_ms, p2_ms, nrow = 2, ncol = 1, common.legend = T, legend = "bottom"), 
#                      top = text_grob("Objective values for multi-step optimization", size = 20))
# p_ms


# svg(filename = "../Figures/Results/objective_n_multistep.svg")
# p_ms
# dev.off()



# 
# poviolin_ARIMA_S1 <- ggplot(data_ARIMA_S1, aes(x = n, y = Value)) +
#   geom_violin(fill = "#E69F00") + 
#   geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black")+
#   stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue")+
#   stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red')+
#   ggtitle("ARIMA \n Damning")+
#   theme_minimal()+
#   theme(plot.title = element_text(size = 10))+
#   theme(plot.title = element_text(size = 24), text = element_text(size=24))
# 
# 
# violin_ARIMAX_S1 <- ggplot(data_ARIMAX_S1, aes(x = n, y = Value)) +
#   geom_violin(fill = "#E69F00") + 
#   geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black")+
#   stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue")+
#   stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red')+
#   ggtitle("ARIMAX \n Damning")+
#   theme_minimal()+
#   theme(plot.title = element_text(size = 10))+
#   theme(plot.title = element_text(size = 24), text = element_text(size=24))
# 
# violin_ARIMA_S1
# violin_ARIMAX_S1
# 
# 
# 
# violin_ARIMA_S2 <- ggplot(data_ARIMA_S2, aes(x = n, y = Value)) +
#   geom_violin(fill = "#E69F00") + 
#   geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black")+
#   stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue")+
#   stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red')+
#   ggtitle("ARIMA \n Damhusåen")+
#   theme_minimal()+
#   theme(plot.title = element_text(size = 10))+
#   theme(plot.title = element_text(size = 24), text = element_text(size=24))
# 
# 
# 
# 
# 
# 
# 
# violin_ARIMAX_S2 <- ggplot(data_ARIMAX_S2, aes(x = n, y = Value)) +
#   geom_violin(fill = "#E69F00") + 
#   geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black")+
#   stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue")+
#   stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red')+
#   ggtitle("ARIMAX \n Damhusåen")+
#   theme_minimal()+
#   theme(plot.title = element_text(size = 10))+
#   theme(plot.title = element_text(size = 24), text = element_text(size=24))
# 
# 
# 
# # annotate_figure(ggarrange(violin_ARIMA_S1, violin_ARIMA_S2, violin_ARIMAX_S1, violin_ARIMAX_S2),
# #                 top = text_grob("Objective function distribution for different number of coefficients", size = 15))
# 
# violin_objfun_multistep <- annotate_figure(ggarrange(violin_ARIMA_S1 + coord_cartesian(ylim=c(0,15)),
#                           violin_ARIMA_S2 + coord_cartesian(ylim=c(0,15)),
#                           violin_ARIMAX_S1 + coord_cartesian(ylim=c(0,15)),
#                           violin_ARIMAX_S2 + coord_cartesian(ylim=c(0,15))),
#                 top = text_grob("Objective function distribution for different number of coefficients", size = 30))
# 
# # svg(filename = "../Figures/Results/violin_objfun_multistep.svg", height=10, width=15)
# # violin_objfun_multistep
# # dev.off()




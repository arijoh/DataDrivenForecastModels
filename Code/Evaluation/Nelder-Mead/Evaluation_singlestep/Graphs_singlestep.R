library(ggplot2)
library(ggpubr)
library(dplyr)



files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S1 <- lapply(paste("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S2 <- lapply(paste("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S1 <- lapply(paste("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S2 <- lapply(paste("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

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
  data_ARIMAX_S2[i,2] <- as.numeric(unlist(ARIMAX_S2[[i]]$Time), units="hours")
  if (!is.null(unlist(ARIMAX_S2[[i]]$value))){
    data_ARIMAX_S2[i,3] <- unlist(ARIMAX_S2[[i]]$value)
  } else{ data_ARIMAX_S2[i,3] <- NA}
  if (!is.null(unlist(ARIMAX_S2[[i]]$convergence))){
    data_ARIMAX_S2[i,4] <- unlist(ARIMAX_S2[[i]]$convergence)
  } else{ data_ARIMAX_S2[i,4] <- NA}
}


## Same for multi-singlesteps
## Histogramover model complexity

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

comp_time <- annotate_figure(ggarrange(time_ARIMA_S1, time_ARIMA_S2, time_ARIMAX_S1, time_ARIMAX_S2), 
                top = text_grob("Computation time", size = 30))
# svg(filename="../Figures/Results/comptime_singlestep.svg")
# comp_time
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


conv_plot_1 <- ggplot(data = data, aes(fill=as.character(Conv), y=n, x=type)) + 
  geom_bar(position="stack", stat="identity", alpha=I(.9))+
  scale_fill_manual(values = c("Converged" = "darkgoldenrod2",
                               "Not converged" = "steelblue"), 
                    na.value = "firebrick4", name= "Convergence")+
  ggtitle("Convergences for \nsingle-step optimizations")+
  theme_minimal()+
  theme(plot.title = element_text(size = 16), text = element_text(size=16))+
  ylab("%") + xlab("")  + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

# svg(filename = "../Figures/Results/conv_singlestep.svg")
# conv_plot_1
# dev.off()



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
p1_ss <- ggplot(data_station1, aes(x = n, y = Value, fill = type, group = interaction(n, type)))+
  geom_violin()+
  geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black",  position=position_dodge(0.9))+
  stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue",  position=position_dodge(0.9))+
  stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red',  position=position_dodge(0.9))+
  ggtitle("Single-step calibration")+theme_minimal()+
  theme(plot.title = element_text(size = 18), 
        text = element_text(size=16))+
  labs(fill = "Model type")+
  xlab("Nr. of hyperparameters")+ylab("Sum-of-square")+
  coord_cartesian(ylim=c(0,10))


p2_ss <- ggplot(data_station2, aes(x = n, y = Value, fill = type, group = interaction(n, type)))+
  geom_violin()+
  geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black",  position=position_dodge(0.9))+
  stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue",  position=position_dodge(0.9))+
  stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red',  position=position_dodge(0.9))+
  ggtitle("Single-step calibration")+theme_minimal()+
  theme(plot.title = element_text(size = 18), 
        text = element_text(size=16))+
  labs(fill = "Model type")+
  xlab("Nr. of hyperparameters")+ylab("Sum-of-square")+
  coord_cartesian(ylim=c(0,15))



### Plot togather single-multistep for stations
p_Dammning <- annotate_figure(ggarrange(p1_ss, p1_ms, nrow = 2, ncol = 1, common.legend = T, legend = "bottom"), 
                        top = text_grob("Sum-of-squares distributions for Dæmningen", size = 20))
p_Dammning


p_Damhusaen <- annotate_figure(ggarrange(p2_ss, p2_ms, nrow = 2, ncol = 1, common.legend = T, legend = "bottom"), 
                              top = text_grob("Sum-of-squares distributions for Damhusåen", size = 20))
p_Damhusaen



# svg(filename = "../Figures/Results/violin_Dammning.svg")
# p_Dammning
# dev.off()
# 
# svg(filename = "../Figures/Results/violin_Damhusaen.svg")
# p_Damhusaen
# dev.off()


# svg(filename = "../Figures/Results/objective_n_singlestep.svg")
# p_ss
# dev.off()



# 
# #### To violin plot out of this
# data_ARIMA_S1$n <- as.factor(data_ARIMA_S1$n)
# #data_ARIMA_S1$type <- rep("ARIMA dammning", nrow(data_ARIMA_S1))
# data_ARIMA_S2$n <- as.factor(data_ARIMA_S2$n)
# #data_ARIMA_S2$type <- rep("ARIMA damhusaen", nrow(data_ARIMA_S2))
# data_ARIMAX_S1$n <- as.factor(data_ARIMAX_S1$n)
# #data_ARIMAX_S1$type <- rep("ARIMAX dammning", nrow(data_ARIMAX_S1))
# data_ARIMAX_S2$n <- as.factor(data_ARIMAX_S2$n)
# #data_ARIMAX_S2$type <- rep("ARIMAX damhusaen", nrow(data_ARIMAX_S2))
# 
# 
# 
# # data <- rbind(data_ARIMA_S1, data_ARIMA_S2, data_ARIMAX_S1, data_ARIMAX_S2)
# # ggplot(data, aes(x = n, y = Value, fill = type))+ #, group = interaction(type, variable)
# #   geom_violin() +
# #   scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "green"))+
# #   #geom_boxplot(width=0.15, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
# #   #stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue", position=position_dodge(0.5))+
# #   #stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red', position=position_dodge(0.5))+
# #   coord_cartesian(ylim=c(0,15))
# 
# 
# 
# 
# 
# violin_ARIMA_S1 <- ggplot(data_ARIMA_S1, aes(x = n, y = Value)) +
#   geom_violin(fill = "#E69F00") +
#   geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black")+
#   stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue")+
#   stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red')+
#   ggtitle("ARIMA \n Damning")+
#   theme_minimal()+
#   theme(plot.title = element_text(size = 24), text = element_text(size=24))
# 
# 
# violin_ARIMA_S2 <- ggplot(data_ARIMA_S2, aes(x = n, y = Value)) +
#   geom_violin(fill = "#E69F00") +
#   geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black")+
#   stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue")+
#   stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red')+
#   ggtitle("ARIMA \n Damhusåen")+
#   theme_minimal()+
#   theme(plot.title = element_text(size = 24), text = element_text(size=24))
# 
# 
# 
# violin_ARIMAX_S1 <- ggplot(data_ARIMAX_S1, aes(x = n, y = Value)) +
#   geom_violin(fill = "#E69F00") +
#   geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black")+
#   stat_summary(fun=mean, geom="point", shape=1, size=1, color = "blue")+
#   stat_summary(fun=median, geom="point", shape=1, size=1, color = 'red')+
#   ggtitle("ARIMAX \n Damning")+
#   theme_minimal()+
#   theme(plot.title = element_text(size = 24), text = element_text(size=24))
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
#   theme(plot.title = element_text(size = 24), text = element_text(size=24))
# 
# 
# 
# # annotate_figure(ggarrange(violin_ARIMA_S1, violin_ARIMA_S2, violin_ARIMAX_S1, violin_ARIMAX_S2),
# #                 top = text_grob("Objective function distribution for different number of coefficients", size = 30))
# 
# violin_objfun_singlestep <- annotate_figure(ggarrange(violin_ARIMA_S1 + coord_cartesian(ylim=c(0,15)),
#                           violin_ARIMA_S2 + coord_cartesian(ylim=c(0,15)),
#                           violin_ARIMAX_S1 + coord_cartesian(ylim=c(0,15)),
#                           violin_ARIMAX_S2 + coord_cartesian(ylim=c(0,15))),
#                 top = text_grob("Objective function distribution for different number of coefficients", size = 30))
# 
# 
# svg(filename = "../Figures/Results/violin_objfun_singlestep.svg", height=10, width=15)
# violin_objfun_singlestep
# dev.off()



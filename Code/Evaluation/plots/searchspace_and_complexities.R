library(ggplot2)
library(ggpubr)
library(dplyr)


####################### Multistep Nelder-Mead
files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S1_ms_nm <- lapply(paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S2_ms_nm <- lapply(paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S1_ms_nm <- lapply(paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S2_ms_nm <- lapply(paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

####################### Single-step Nelder-Mead
files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S1_ss_nm <- lapply(paste("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S2_ss_nm <- lapply(paste("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S1_ss_nm <- lapply(paste("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S2_ss_nm <- lapply(paste("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)


###################### Multi-step DDS
files <- list.files(path = "Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S1_ms_dds <- lapply(paste("Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S2_ms_dds <- lapply(paste("Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S1_ms_dds <- lapply(paste("Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S2_ms_dds <- lapply(paste("Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)


###################### Single-step DDS
files <- list.files(path = "Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S1_ss_dds <- lapply(paste("Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S2_ss_dds <- lapply(paste("Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S1_ss_dds <- lapply(paste("Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S2_ss_dds <- lapply(paste("Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

########################################### Data preparation



##### ONLY CHECK ORDERS WHERE D = 0
getInfo <- function(L){
  df <- as.data.frame(matrix(nrow = length(L), ncol =4))
  colnames(df) <- c("n", "Hours", "Value", "Conv")
  for (i in (1:length(L))){
    df[i,1] <- length(unlist(L[[i]]$par))
    df[i,2] <- as.numeric(unlist(L[[i]]$Time), units="hours")
    if (!is.null(unlist(L[[i]]$value))){
      df[i,3] <- unlist(L[[i]]$value)
    } else{ df[i,3] <- NA}
    if (!is.null(unlist(L[[i]]$convergence))){
      df[i,4] <- unlist(L[[i]]$convergence)
    } else{ df[i,4] <- NA}
  }

  return(df)
}

ARIMA_S1_ms_nm <- getInfo(ARIMA_S1_ms_nm)
ARIMA_S2_ms_nm  <- getInfo(ARIMA_S2_ms_nm)
ARIMAX_S1_ms_nm  <- getInfo(ARIMAX_S1_ms_nm)
ARIMAX_S2_ms_nm  <- getInfo(ARIMAX_S2_ms_nm)
ARIMA_S1_ss_nm  <- getInfo(ARIMA_S1_ss_nm)
ARIMA_S2_ss_nm <- getInfo(ARIMA_S2_ss_nm)
ARIMAX_S1_ss_nm  <- getInfo(ARIMAX_S1_ss_nm)
ARIMAX_S2_ss_nm  <- getInfo(ARIMAX_S2_ss_nm)
ARIMA_S1_ms_dds  <- getInfo(ARIMA_S1_ms_dds)
ARIMA_S2_ms_dds <- getInfo(ARIMA_S2_ms_dds)
ARIMAX_S1_ms_dds  <- getInfo(ARIMAX_S1_ms_dds)
ARIMAX_S2_ms_dds  <- getInfo(ARIMAX_S2_ms_dds)
ARIMA_S1_ss_dds <- getInfo(ARIMA_S1_ss_dds)
ARIMA_S2_ss_dds  <- getInfo(ARIMA_S2_ss_dds)
ARIMAX_S1_ss_dds <- getInfo(ARIMAX_S1_ss_dds)
ARIMAX_S2_ss_dds <- getInfo(ARIMAX_S2_ss_dds)



####################### Model complexity
## What to plot:::
#the number of coefficients are all the same for different optimization methods or different objective function criteria
#Thus, we just take Dammning ss Nelder-Mead here
#ARIMA_S1_ss_nm
#ARIMAX_S1_ss_nm
hist_ARIMA <- ggplot(ARIMA_S1_ss_nm, aes(x=n))+
  geom_histogram(binwidth=1, fill=I("blue"), col=I("black"), alpha=I(.5)) +
  ggtitle("ARIMA")+
  xlab("Total nr.") + ylab("Count")+
  scale_y_continuous(minor_breaks = seq(0 , 200, 25), breaks = seq(0, 200, 25), limits = c(-0.5, 200))+
  scale_x_continuous(limits = c(-0.5, 23.5))+
  theme_minimal()+
  theme(plot.title = element_text(size = 16), text = element_text(size=16))

hist_ARIMAX <- ggplot(ARIMAX_S1_ss_nm, aes(x=n))+
  geom_histogram(binwidth=1, fill=I("blue"), col=I("black"), alpha=I(.5)) +
  ggtitle("ARIMAX")+
  xlab("Total nr.") + ylab("Count")+
  scale_y_continuous(minor_breaks = seq(0 , 200, 25), breaks = seq(0, 200, 25), limits = c(-0.5, 200))+
  scale_x_continuous(limits = c(-0.5, 23.5))+
  theme_minimal()+
  theme(plot.title = element_text(size = 16), text = element_text(size=16))

hyperparameter_distribution <- annotate_figure(ggarrange(hist_ARIMA, hist_ARIMAX),
                                     top = text_grob("Hyperparameter distribution", size= 18))
# hyperparameter_distribution
ggsave(filename="../Figures/Results/hyperparameter_distribution.pdf", width = 7, heigh = 3)
hyperparameter_distribution
dev.off()


















############ Computation time
### aggregate data
aggregateData <- function(data, op){
  data <- aggregate(data, list(data$n), mean)[,-1]
  #data = data #when not aggragate
  data$optimization <- op
  return(data)
}
## station 1 single-step
data_aggr_ARIMA_S1_ss_nm <- aggregateData(data = ARIMA_S1_ss_nm, op = "Nelder-Mead")
data_aggr_ARIMA_S1_ss_dds <- aggregateData(data = ARIMA_S1_ss_dds, op = "DDS")
data_aggr_ARIMA_S1_ss <- rbind(data_aggr_ARIMA_S1_ss_nm, data_aggr_ARIMA_S1_ss_dds)
data_aggr_ARIMAX_S1_ss_nm  <- aggregateData(ARIMAX_S1_ss_nm, op = "Nelder-Mead")
data_aggr_ARIMAX_S1_ss_dds <- aggregateData(ARIMAX_S1_ss_dds, op = "DDS")
data_aggr_ARIMAX_S1_ss <- rbind(data_aggr_ARIMAX_S1_ss_nm, data_aggr_ARIMAX_S1_ss_dds)

# station 2 single-step
data_aggr_ARIMA_S2_ss_nm <- aggregateData(data = ARIMA_S2_ss_nm, op = "Nelder-Mead")
data_aggr_ARIMA_S2_ss_dds <- aggregateData(ARIMA_S2_ss_dds, op = "DDS")
data_aggr_ARIMA_S2_ss <- rbind(data_aggr_ARIMA_S2_ss_nm, data_aggr_ARIMA_S2_ss_dds)
data_aggr_ARIMAX_S2_ss_nm  <- aggregateData(ARIMAX_S2_ss_nm, op = "Nelder-Mead")
data_aggr_ARIMAX_S2_ss_dds <- aggregateData(ARIMAX_S2_ss_dds, op = "DDS")
data_aggr_ARIMAX_S2_ss <- rbind(data_aggr_ARIMAX_S2_ss_nm, data_aggr_ARIMAX_S2_ss_dds)

## station1 multi-step
data_aggr_ARIMA_S1_ms_nm <- aggregateData(data = ARIMA_S1_ms_nm, op = "Nelder-Mead")
data_aggr_ARIMA_S1_ms_dds <- aggregateData(ARIMA_S1_ms_dds, op = "DDS")
data_aggr_ARIMA_S1_ms <- rbind(data_aggr_ARIMA_S1_ms_nm, data_aggr_ARIMA_S1_ms_dds)
data_aggr_ARIMAX_S1_ms_nm  <- aggregateData(ARIMAX_S1_ms_nm, op = "Nelder-Mead")
data_aggr_ARIMAX_S1_ms_dds <- aggregateData(ARIMAX_S1_ms_dds, op = "DDS")
data_aggr_ARIMAX_S1_ms <- rbind(data_aggr_ARIMAX_S1_ms_nm, data_aggr_ARIMAX_S1_ms_dds)

## station 2 multi-step
data_aggr_ARIMA_S2_ms_nm <- aggregateData(data = ARIMA_S2_ms_nm, op = "Nelder-Mead")
data_aggr_ARIMA_S2_ms_dds <- aggregateData(ARIMA_S2_ms_dds, op = "DDS")
data_aggr_ARIMA_S2_ms <- rbind(data_aggr_ARIMA_S2_ms_nm, data_aggr_ARIMA_S2_ms_dds)
data_aggr_ARIMAX_S2_ms_nm  <- aggregateData(ARIMAX_S2_ms_nm, op = "Nelder-Mead")
data_aggr_ARIMAX_S2_ms_dds <- aggregateData(ARIMAX_S2_ms_dds, op = "DDS")
data_aggr_ARIMAX_S2_ms <- rbind(data_aggr_ARIMAX_S2_ms_nm, data_aggr_ARIMAX_S2_ms_dds)



plotComputationCost <- function(data, title){
  plot <-ggplot(data=data, aes(x=n, y=Hours)) +
    geom_bar(stat="identity", fill=I("blue"), col=I("black"), alpha=(.5))+
    ggtitle(title)+
    facet_grid(cols = vars(optimization))+
    scale_x_continuous(limits = c(-0.5, 23.5))+
    theme_minimal()+
    theme(plot.title = element_text(size = 16), text = element_text(size=12))
  return(plot)
}


compCost_station1_ARIMA_ss <- plotComputationCost(data_aggr_ARIMA_S1_ss, title = "ARIMA")
compCost_station1_ARIMAX_ss <- plotComputationCost(data_aggr_ARIMAX_S1_ss, title = "ARIMAX")
compCost_station2_ARIMA_ss <-  plotComputationCost(data_aggr_ARIMA_S2_ss, title = "ARIMA")
compCost_station2_ARIMAX_ss <-  plotComputationCost(data_aggr_ARIMAX_S2_ss, title = "ARIMAX")
compCost_station1_ARIMA_ms <- plotComputationCost(data_aggr_ARIMA_S1_ms, title = "ARIMA")
compCost_station1_ARIMAX_ms <- plotComputationCost(data_aggr_ARIMAX_S1_ms, title = "ARIMAX")
compCost_station2_ARIMA_ms <- plotComputationCost(data_aggr_ARIMA_S2_ms, title = "ARIMA")
compCost_station2_ARIMAX_ms <- plotComputationCost(data_aggr_ARIMAX_S2_ms, title = "ARIMAX")


ComputationCost_station1_ss <- annotate_figure(ggarrange(compCost_station1_ARIMA_ss, compCost_station1_ARIMAX_ss, nrow = 2), 
                                        top = text_grob("Computing time\nDæmmningen, single-step models", size = 16))
ComputationCost_station2_ss <- annotate_figure(ggarrange(compCost_station2_ARIMA_ss, compCost_station2_ARIMAX_ss, nrow = 2), 
                                        top = text_grob("Computing time\nDamhusåen, single-step models", size = 16))
ComputationCost_station1_ms <- annotate_figure(ggarrange(compCost_station1_ARIMA_ms, compCost_station1_ARIMAX_ms, nrow = 2), 
                                        top = text_grob("Computing time\nDæmmningen, multi-step models", size = 16))
ComputationCost_station2_ms <- annotate_figure(ggarrange(compCost_station2_ARIMA_ms, compCost_station2_ARIMAX_ms, nrow = 2), 
                                        top = text_grob("Computing time\nDamhusåen, multi-step models", size = 16))

ComputationCost_station1_ss
ComputationCost_station2_ss
ComputationCost_station1_ms
ComputationCost_station2_ms

ggsave(filename="../Figures/Results/ComputationTime/ComputationCost_station1_ss.pdf", height = 5 , width = 8)
ComputationCost_station1_ss
dev.off()

ggsave(filename="../Figures/Results/ComputationTime/ComputationCost_station2_ss.pdf", height = 5 , width = 8)
ComputationCost_station2_ss
dev.off()

ggsave(filename="../Figures/Results/ComputationTime/ComputationCost_station1_ms.pdf", height = 5 , width = 8)
ComputationCost_station1_ms
dev.off()

ggsave(filename="../Figures/Results/ComputationTime/ComputationCost_station2_ms.pdf", height = 5 , width = 8)
ComputationCost_station2_ms
dev.off()





####   DISTRIBUTIONS
factorData <- function(data, op){
  data$n <- as.factor(data$n)
  data$optimization <- op
  return(data)
}

## station 1 single-step
data_ARIMA_S1_ss_nm <- factorData(data = ARIMA_S1_ss_nm, op = "Nelder-Mead")
data_ARIMA_S1_ss_dds <- factorData(data = ARIMA_S1_ss_dds, op = "DDS")
data_ARIMA_S1_ss <- rbind(data_ARIMA_S1_ss_nm, data_ARIMA_S1_ss_dds)
data_ARIMAX_S1_ss_nm  <- factorData(ARIMAX_S1_ss_nm, op = "Nelder-Mead")
data_ARIMAX_S1_ss_dds <- factorData(ARIMAX_S1_ss_dds, op = "DDS")
data_ARIMAX_S1_ss <- rbind(data_ARIMAX_S1_ss_nm, data_ARIMAX_S1_ss_dds)

# station 2 single-step
data_ARIMA_S2_ss_nm <- factorData(data = ARIMA_S2_ss_nm, op = "Nelder-Mead")
data_ARIMA_S2_ss_dds <- factorData(ARIMA_S2_ss_dds, op = "DDS")
data_ARIMA_S2_ss <- rbind(data_ARIMA_S2_ss_nm, data_ARIMA_S2_ss_dds)
data_ARIMAX_S2_ss_nm  <- factorData(ARIMAX_S2_ss_nm, op = "Nelder-Mead")
data_ARIMAX_S2_ss_dds <- factorData(ARIMAX_S2_ss_dds, op = "DDS")
data_ARIMAX_S2_ss <- rbind(data_ARIMAX_S2_ss_nm, data_ARIMAX_S2_ss_dds)

## station1 multi-step
data_ARIMA_S1_ms_nm <- factorData(data = ARIMA_S1_ms_nm, op = "Nelder-Mead")
data_ARIMA_S1_ms_dds <- factorData(ARIMA_S1_ms_dds, op = "DDS")
data_ARIMA_S1_ms <- rbind(data_ARIMA_S1_ms_nm, data_ARIMA_S1_ms_dds)
data_ARIMAX_S1_ms_nm  <- factorData(ARIMAX_S1_ms_nm, op = "Nelder-Mead")
data_ARIMAX_S1_ms_dds <- factorData(ARIMAX_S1_ms_dds, op = "DDS")
data_ARIMAX_S1_ms <- rbind(data_ARIMAX_S1_ms_nm, data_ARIMAX_S1_ms_dds)

## station 2 multi-step
data_ARIMA_S2_ms_nm <- factorData(data = ARIMA_S2_ms_nm, op = "Nelder-Mead")
data_ARIMA_S2_ms_dds <- factorData(ARIMA_S2_ms_dds, op = "DDS")
data_ARIMA_S2_ms <- rbind(data_ARIMA_S2_ms_nm, data_ARIMA_S2_ms_dds)
data_ARIMAX_S2_ms_nm  <- factorData(ARIMAX_S2_ms_nm, op = "Nelder-Mead")
data_ARIMAX_S2_ms_dds <- factorData(ARIMAX_S2_ms_dds, op = "DDS")
data_ARIMAX_S2_ms <- rbind(data_ARIMAX_S2_ms_nm, data_ARIMAX_S2_ms_dds)


costDistribution <- function(data, title){
  plot <-ggplot(data=data, aes(x=n, y=Hours)) +
    geom_violin(scale = "width", width = 0.565, fill=I("blue"), col=I("black"), alpha=I(.5))+
    #ggtitle(title)+
    facet_grid(cols = vars(optimization))+
    #scale_x_discrete(breaks = seq(0, 25, 5))+
    theme_minimal()+
    theme(plot.title = element_text(size = 16), text = element_text(size=16))
  return(plot)
}



compCost_station1_ARIMA_ss <- costDistribution(data_ARIMA_S1_ss, title = "ARIMA")
compCost_station1_ARIMAX_ss <- costDistribution(data_ARIMAX_S1_ss, title = "ARIMAX")
compCost_station2_ARIMA_ss <-  costDistribution(data_ARIMA_S2_ss, title = "ARIMA")
compCost_station2_ARIMAX_ss <-  costDistribution(data_ARIMAX_S2_ss, title = "ARIMAX")
compCost_station1_ARIMA_ms <- costDistribution(data_ARIMA_S1_ms, title = "ARIMA")
compCost_station1_ARIMAX_ms <- costDistribution(data_ARIMAX_S1_ms, title = "ARIMAX")
compCost_station2_ARIMA_ms <- costDistribution(data_ARIMA_S2_ms, title = "ARIMA")
compCost_station2_ARIMAX_ms <- costDistribution(data_ARIMAX_S2_ms, title = "ARIMAX")


ComputationCost_station1_ss <- annotate_figure(ggarrange(compCost_station1_ARIMA_ss, compCost_station1_ARIMAX_ss, nrow = 2), 
                                               top = text_grob("Computing time\nDæmmningen, single-step models", size = 16))
ComputationCost_station2_ss <- annotate_figure(ggarrange(compCost_station2_ARIMA_ss, compCost_station2_ARIMAX_ss, nrow = 2), 
                                               top = text_grob("Computing time\nDamhusåen, single-step models", size = 16))
ComputationCost_station1_ms <- annotate_figure(ggarrange(compCost_station1_ARIMA_ms, compCost_station1_ARIMAX_ms, nrow = 2), 
                                               top = text_grob("Computing time\nDæmmningen, multi-step models", size = 16))
ComputationCost_station2_ms <- annotate_figure(ggarrange(compCost_station2_ARIMA_ms, compCost_station2_ARIMAX_ms, nrow = 2), 
                                               top = text_grob("Computing time\nDamhusåen, multi-step models", size = 16))

ComputationCost_station1_ss
ComputationCost_station2_ss
ComputationCost_station1_ms
ComputationCost_station2_ms


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


fillData <- function(NM, DDS, model){
  fillData_ <- function(data, optim_){
    #optim_ is ARIMA or ARIMAX
    df <- as.data.frame(matrix(nrow = length(data), ncol =4))
    colnames(df) <- c("n", "Hours", "Value", "Conv")
    for (i in (1:length(data))){
      df[i,1] <- (length(unlist(data[[i]]$par)))
      df[i,2] <- as.numeric(unlist(data[[i]]$Time), units="hours")
      if (!is.null(unlist(data[[i]]$value))){
        df[i,3] <- unlist(data[[i]]$value)
      } else{ df[i,3] <- NA}
      if (!is.null(unlist(data[[i]]$convergence))){
        df[i,4] <- unlist(data[[i]]$convergence)
      } else{ df[i,4] <- NA}
    }
    df$n <- as.factor(df$n)
    df$optim <- optim_
    return(df)
  }
  data1 <- fillData_(NM, optim_ = "Nelder-Mead")
  data2 <- fillData_(DDS, optim_ = "DDS")
  data <- rbind(data1, data2)
  data$Model <- rep(model)
  return(data)
}


## Station 1, sigle-step
data_ARIMA_S1_ss <- fillData(NM = ARIMA_S1_ss_nm, DDS = ARIMA_S1_ss_dds, model = "ARIMA")
data_ARIMAX_S1_ss <- fillData(NM = ARIMAX_S1_ss_nm, DDS = ARIMAX_S1_ss_dds, model = "ARIMAX")
data_S1_ss <- rbind(data_ARIMA_S1_ss, data_ARIMAX_S1_ss)

## Station 2, single step
data_ARIMA_S2_ss <- fillData(NM = ARIMA_S2_ss_nm, DDS = ARIMA_S2_ss_dds, model = "ARIMA")
data_ARIMAX_S2_ss <- fillData(NM = ARIMAX_S2_ss_nm, DDS = ARIMAX_S2_ss_dds, model = "ARIMAX")
data_S2_ss <- rbind(data_ARIMA_S2_ss, data_ARIMAX_S2_ss)

## Station 1, multi-step
data_ARIMA_S1_ms <- fillData(NM = ARIMA_S1_ms_nm, DDS = ARIMA_S1_ms_dds, model = "ARIMA")
data_ARIMAX_S1_ms <- fillData(NM = ARIMAX_S1_ms_nm, DDS = ARIMAX_S1_ms_dds, model = "ARIMAX")
data_S1_ms <- rbind(data_ARIMA_S1_ms, data_ARIMAX_S1_ms)

## Station 2, multi step
data_ARIMA_S2_ms <- fillData(NM = ARIMA_S2_ms_nm, DDS = ARIMA_S2_ms_dds, model = "ARIMA")
data_ARIMAX_S2_ms <- fillData(NM = ARIMAX_S2_ms_nm, DDS = ARIMAX_S2_ms_dds, model = "ARIMAX")
data_S2_ms <- rbind(data_ARIMA_S2_ms, data_ARIMAX_S2_ms)


rm(ARIMA_S1_ms_nm,
   ARIMA_S2_ms_nm,
   ARIMAX_S1_ms_nm,
   ARIMAX_S2_ms_nm,
   ARIMA_S1_ss_nm,
   ARIMA_S2_ss_nm, 
   ARIMAX_S1_ss_nm,
   ARIMAX_S2_ss_nm,
   ARIMA_S1_ms_dds,
   ARIMA_S2_ms_dds,
   ARIMAX_S1_ms_dds,
   ARIMAX_S2_ms_dds,
   ARIMA_S1_ss_dds,
   ARIMA_S2_ss_dds,
   ARIMAX_S1_ss_dds,
   ARIMAX_S2_ss_dds
)


###########################################  Function to plot
plotViolin <- function(data, zoom_to, title){
  p1 <- ggplot(data, aes(x = n, y = Value, fill = optim)) +
    geom_violin(position=position_dodge(0.75), scale = "width", width = 0.65) +
    scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
    geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
    stat_summary(fun=mean, geom="point", shape=19, size=1.25, color = "blue", position=position_dodge(0.75))+
    stat_summary(fun=median, geom="point", shape=19, size=1.25, color = 'red', position=position_dodge(0.75))+
    labs(fill = "Optimization")+
    ggtitle(title)+
    xlab("Nr. of coefficients")+ylab("Obj.fun")+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          plot.title = element_text(size = 18), text = element_text(size=16))+
    facet_grid(rows = vars(Model))+ ### Take this if station splitted vertically
    scale_x_discrete(limits=as.character(seq(1,23)))+
    coord_cartesian(ylim=c(0,50), xlim = c(1,23))
  
  p2 <- p1 + coord_cartesian(ylim=c(0,zoom_to), xlim = c(1,23)) + ggtitle("Y-axis adjusted")
  
  plot <- ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "bottom")
  return(plot)
}

## Rows
plot_S1_ss <- plotViolin(data_S1_ss, zoom_to = 2.5, title = "Dæmningen\nSingle-step objective function criteria")
plot_S1_ms <- plotViolin(data_S1_ms, zoom_to = 10, title = "Dæmningen\nMulti-step objective function criteria")

plot_S2_ss <- plotViolin(data_S2_ss, zoom_to = 10, title = "Damhusåen\nSingle-step objective function criteria")
plot_S2_ms <- plotViolin(data_S2_ms, zoom_to = 10, title = "Damhusåen\nMulti-step objective function criteria")


ggsave(filename = "../Figures/Results/Comparison/ObjectiveFunction/singlestepDammningen.pdf", width = 11, height =8)
plot_S1_ss
dev.off()

ggsave(filename = "../Figures/Results/Comparison/ObjectiveFunction/singlestepDamhusaen.pdf", width = 11, height = 8)
plot_S2_ss
dev.off()

ggsave(filename = "../Figures/Results/Comparison/ObjectiveFunction/multistepDammningen.pdf", width = 11, height = 8)
plot_S1_ms
dev.off()

ggsave(filename = "../Figures/Results/Comparison/ObjectiveFunction/multistepDamhusaen.pdf", width = 11, height = 8)
plot_S2_ms
dev.off()


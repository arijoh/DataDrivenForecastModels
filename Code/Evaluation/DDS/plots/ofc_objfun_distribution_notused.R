library(ggplot2)
library(ggpubr)
library(dplyr)

###################### Multi-step DDS
files <- list.files(path = "Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S1_ms <- lapply(paste("Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S2_ms <- lapply(paste("Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S1_ms <- lapply(paste("Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S2_ms <- lapply(paste("Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)


###################### Single-step DDS
files <- list.files(path = "Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S1_ss <- lapply(paste("Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S2_ss <- lapply(paste("Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S1_ss <- lapply(paste("Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S2_ss <- lapply(paste("Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

########################################### Data preparation


fillData <- function(ss, ms, model){
  fillData_ <- function(data, ofc_){
    #ofc is single-step or multi-step
    df <- as.data.frame(matrix(nrow = length(data), ncol =3))
    colnames(df) <- c("n", "Hours", "Value")
    for (i in (1:length(data))){
      df[i,1] <- (length(unlist(data[[i]]$par)))
      df[i,2] <- as.numeric(unlist(data[[i]]$Time), units="hours")
      if (!is.null(unlist(data[[i]]$value))){
        df[i,3] <- unlist(data[[i]]$value)
      } else{ df[i,3] <- NA}
    }
    df$n <- as.factor(df$n)
    df$ofc <- ofc_
    return(df)
  }
  data1 <- fillData_(ss, ofc = "Single-step")
  data2 <- fillData_(ms, ofc = "Multi-step")
  data <- rbind(data1, data2)
  data$Model <- rep(model)
  return(data)
}


## Station 1
data_ARIMA_S1 <- fillData(ss = ARIMA_S1_ss, ms = ARIMA_S1_ms, model = "ARIMA")
data_ARIMAX_S1 <- fillData(ss = ARIMAX_S1_ss, ms = ARIMAX_S1_ms, model = "ARIMAX")
data_S1 <- rbind(data_ARIMA_S1, data_ARIMAX_S1)


## Station 2
data_ARIMA_S2 <- fillData(ss = ARIMA_S2_ss, ms = ARIMA_S2_ms, model = "ARIMA")
data_ARIMAX_S2 <- fillData(ss = ARIMAX_S2_ss, ms = ARIMAX_S2_ms, model = "ARIMAX")
data_S2 <- rbind(data_ARIMA_S2, data_ARIMAX_S2)





###########################################  Function to plot
plotViolin <- function(data, title){
  plot <- ggplot(data, aes(x = n, y = Value, fill = ofc)) +
    geom_violin(position=position_dodge(0.75), scale = "width", width = 0.65) +
    scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
    geom_boxplot(width=0, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.75))+
    stat_summary(fun=mean, geom="point", shape=19, size=1.25, color = "blue", position=position_dodge(0.75))+
    stat_summary(fun=median, geom="point", shape=19, size=1.25, color = 'red', position=position_dodge(0.75))+
    labs(fill = "Objective function criteria")+
    ggtitle(title)+
    xlab("Nr. of coefficients")+ylab("Obj.fun")+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          plot.title = element_text(size = 18), text = element_text(size=16))+
    facet_grid(rows = vars(Model))+ ### Take this if station splitted vertically
    scale_x_discrete(limits=as.character(seq(1,23)))+
    coord_cartesian(ylim=c(0,50), xlim = c(1,23))
  
  return(plot)
}

plotViolin_zoom <- function(p, zoom_to, title){
  plot <- p + coord_cartesian(ylim=c(0,zoom_to), xlim = c(1,23)) + ggtitle(title)
  return(plot)
}



## Rows
plot_S1 <- plotViolin(data_S1, title = "Dæmningen")
plot_S1_zoom <- plotViolin_zoom(plot_S1, zoom_to = 10, title = "Dæmningen")


plot_S2 <- plotViolin(data_S2, title = "Damhusåen")
plot_S2_zoom <- plotViolin_zoom(plot_S2, zoom_to = 15, title = "Damhusåen")


plot_S1
plot_S2
plot_S1_zoom
plot_S2_zoom

plot_all <- annotate_figure(ggarrange(plot_S1, plot_S2, plot_S1_zoom, plot_S2_zoom, ncol = 2, nrow = 2, common.legend = T, legend = "bottom"),
                        top = text_grob("Distribution of mininized objective function vale", size = 18))
plot_all

plot <- annotate_figure(ggarrange(plot_S1, plot_S2, nrow = 2, common.legend = T, legend = "bottom"),
                            top = text_grob("Distribution of mininized objective function vale", size = 18))
plot

plot_zoom <- annotate_figure(ggarrange(plot_S1_zoom, plot_S2_zoom, nrow = 2, common.legend = T, legend = "bottom"),
                        top = text_grob("Distribution of mininized objective function vale", size = 18))
plot_zoom

# svg(filename = "../Figures/Results/DDS/ObjectiveFunction/ObjectiveFunctionDistribution_bothstations.svg", width = 11, height =8)
# plot
# dev.off()

svg(filename = "../Figures/Results/DDS/ObjectiveFunction/ObjectiveFunctionDistribution.svg", width = 11, height =8)
plot
dev.off()

svg(filename = "../Figures/Results/DDS/ObjectiveFunction/ObjectiveFunctionDistribution_zoom.svg", width = 11, height =8)
plot_zoom
dev.off()


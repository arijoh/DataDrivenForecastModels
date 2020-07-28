library(ggplot2)
library(reshape)
library(hrbrthemes)
library(viridis)


### ARIMA models
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

### ARIMAX models
files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S1 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_singlestep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S2 <- lapply(paste("Evaluation/Nelder-Mead/Evaluation_multistep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)




getValues <- function(ARIMA, ARIMAX){
  getValues_ <- function(L){
    data <- data.frame(matrix(NA, nrow = length(L), ncol = 3))
    colnames(data) <- c("reg.lag", "reg.nr", "objval")
    for (i in (1:length(L))){
      lag <- unlist(L[[i]]$reg.lag)
      if (!is.null(lag)){
        data$reg.lag[i] <- lag
      }else{data$reg.lag[i] <- 0}
      
      n <- unlist(L[[i]]$reg.nr)
      if (!is.null(n)){
        data$reg.nr[i] <- n
      }else{data$reg.nr[i] <- 0}
      
      objfun <- unlist(L[[i]]$value)
      if (!is.null(objfun)){
        data$objval[i] <- objfun
      }
    }
    return(data)
  }
  data_ARIMA <- getValues_(ARIMA)
  data_ARIMAX <- getValues_(ARIMAX)
  data <- rbind(data_ARIMA, data_ARIMAX)
}


data_ss_S1 <- getValues(ARIMA = ARIMA_ss_S1, ARIMAX = ARIMAX_ss_S1)
data_ss_S2 <- getValues(ARIMA = ARIMA_ss_S2, ARIMAX = ARIMAX_ss_S2)
data_ms_S1 <- getValues(ARIMA = ARIMA_ms_S1, ARIMAX = ARIMAX_ms_S1) 
data_ms_S2 <- getValues(ARIMA = ARIMA_ms_S2, ARIMAX = ARIMAX_ms_S2) 

# validate if aggregate() works correctly
# data_ss_S1[which((data_ss_S1$reg.lag == 6) & (data_ss_S1$reg.nr == 10)),]
# colMeans(data_ss_S1[which((data_ss_S1$reg.lag == 6) & (data_ss_S1$reg.nr == 10)),], na.rm = T)

data_ss_S1 <- aggregate(.~reg.lag+reg.nr, data_ss_S1, mean)
data_ss_S2 <- aggregate(.~reg.lag+reg.nr, data_ss_S2, mean)
data_ms_S1 <- aggregate(.~reg.lag+reg.nr, data_ms_S1, mean)
data_ms_S2 <- aggregate(.~reg.lag+reg.nr, data_ms_S2, mean)


plotHeatmap <- function(data, title){
  data$reg.lag <- as.factor(data$reg.lag)
  data$reg.nr <- as.factor(data$reg.nr)
  plot <- ggplot(data, aes(x = reg.lag, y = reg.nr, fill= objval)) + 
    geom_tile() +
    geom_text(aes(label = round(objval, 2)))+
    ggtitle(title)+
    scale_fill_viridis(discrete=FALSE) +
    theme_ipsum(grid = F, base_size = 8*2, axis_title_size = 8*2,  axis_text_size = 8*2, plot_margin = margin(5,5,5,5))
  return(plot)
}

plot_ss_S1 <- plotHeatmap(data_ss_S1, title = "Dæmningen\nSingle-step ofc")
plot_ss_S2 <- plotHeatmap(data_ss_S2, title = "Damhusåen\nSingle-step ofc")
plot_ms_S1 <- plotHeatmap(data_ms_S1, title = "Dæmningen\nMulti-step ofc")
plot_ms_S2 <- plotHeatmap(data_ms_S2, title = "Damhusåen\nMulti-step ofc")


ggarrange(plot_ss_S1,
          plot_ss_S2,
          plot_ms_S1,
          plot_ms_S2, ncol = 2, nrow = 2, common.legend = T, legend = "bottom")







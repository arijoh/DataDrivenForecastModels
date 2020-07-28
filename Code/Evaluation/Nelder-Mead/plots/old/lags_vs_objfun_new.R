library(ggplot2)
library(reshape)


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



getValues <- function(L, model, station){
  ### First calculate total amount of regerssors for creating the data frame
  if (model == "ARIMA"){
    size <- length(L)
  }else
    if (model == "ARIMAX"){
      size <- 0
      for (i in (1:length(L))){
        regressors <- unlist(L[[i]]$reg.nr)   # get vector with number of regressors
        size = size + regressors
      }
    }
  
  data <- data.frame(matrix(NA, nrow = size, ncol = 2))
  colnames(data) <- c("lag", "fval")
  counter <- 1
  for (i in (1:length(L))){
    # get stuff
    lag <- unlist(L[[i]]$reg.lag) # get lag
    nreg <- seq(unlist(L[[i]]$reg.nr))   # get vector with number of regressors
    fval <- unlist(L[[i]]$value)
    if (!is.null(fval)){
      if (is.null(lag)){
        ### ARIMA model
        data$lag[counter] <- 0
        data$fval[counter] <- fval
        counter = counter + 1
      } else{
        #ARIMAX model
        for (j in (nreg)){
          #print(lag+j)
          lag_temp <- lag + j
          data$lag[counter] <- lag_temp
          data$fval[counter] <- fval
          counter = counter + 1
        }
      }
    }
  }
  data$station <- rep(station)
  return(data)
}






data_ARIMA_ss_S1 <- getValues(L = ARIMA_ss_S1, model = "ARIMA", station = "Dæmmningen")
data_ARIMA_ms_S1 <- getValues(L = ARIMA_ms_S1, model = "ARIMA", station = "Dæmmningen")
data_ARIMA_ss_S2 <- getValues(L = ARIMA_ss_S2, model = "ARIMA", station = "Damhusåen")
data_ARIMA_ms_S2 <- getValues(L = ARIMA_ms_S2, model = "ARIMA", station = "Damhusåen")


data_ARIMAX_ss_S1 <- getValues(L = ARIMAX_ss_S1, model = "ARIMAX", station = "Dæmmningen")
data_ARIMAX_ms_S1 <- getValues(L = ARIMAX_ms_S1, model = "ARIMAX", station = "Dæmmningen")
data_ARIMAX_ss_S2 <- getValues(L = ARIMAX_ss_S2, model = "ARIMAX", station = "Damhusåen")
data_ARIMAX_ms_S2 <- getValues(L = ARIMAX_ms_S2, model = "ARIMAX", station = "Damhusåen")


data <- rbind(data_ARIMA_ss_S1,
              data_ARIMA_ms_S1,
              data_ARIMA_ss_S2,
              data_ARIMA_ms_S2,
              data_ARIMAX_ss_S1,
              data_ARIMAX_ms_S1,
              data_ARIMAX_ss_S2,
              data_ARIMAX_ms_S2)



data$lag <- as.factor(data$lag)
data$station <- as.factor(data$station)


plot <- ggplot(data, aes(x = lag, y = fval, fill = station))+
  geom_violin(scale = "width", width = 0.65)+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  geom_boxplot(width=0.1, outlier.size = 0.3, outlier.color = "black", position=position_dodge(0.9))+
  stat_summary(fun=mean, geom="point", shape=19, size=1.5, color = "blue", position=position_dodge(0.9))+
  stat_summary(fun=median, geom="point", shape=19, size=1.5, color = 'red', position=position_dodge(0.9))+
  ggtitle("Distribution of objective function values for lag of external regressor")+
  xlab("Lag of external regressor") + ylab("Obj.fun")+
  scale_y_continuous(minor_breaks = seq(0 , 15, 1), breaks = seq(0, 15, 5))+
  theme_minimal()+
  theme(plot.title = element_text(size = 16), 
        text = element_text(size=16),
        legend.position = "none")+
  coord_cartesian(ylim=c(0,15))+
  scale_x_discrete(limits = 1:25)+
  facet_grid(rows = vars(station))
plot


svg(filename="../Figures/Results/Nelder-Mead/better_plots?/reglag_vs_objfun_2.svg", width = 8, height = 5)
plot
dev.off()




library(ggplot2)
library(reshape)
library(hrbrthemes)
library(viridis)
library(ggpubr)


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




getValues_f <- function(ARIMA, ARIMAX){
  getValues_ <- function(L){
    data <- data.frame(matrix(NA, nrow = length(L), ncol = 3))
    colnames(data) <- c("reg.lag", "reg.nr", "f")
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
        data$f[i] <- objfun
      }

    }
    return(data)
  }
  data_ARIMA <- getValues_(ARIMA)
  data_ARIMAX <- getValues_(ARIMAX)
  data <- rbind(data_ARIMA, data_ARIMAX)
}


getValues_PI <- function(ARIMA, ARIMAX){
  getValues_ <- function(L){
    data <- data.frame(matrix(NA, nrow = length(L), ncol = 5))
    colnames(data) <- c("reg.lag", "reg.nr", "PI30", "PI60", "PI90")
    for (i in (1:length(L))){
      lag <- unlist(L[[i]]$reg.lag)
      if (!is.null(lag)){
        data$reg.lag[i] <- lag
      }else{data$reg.lag[i] <- 0}
      
      n <- unlist(L[[i]]$reg.nr)
      if (!is.null(n)){
        data$reg.nr[i] <- n
      }else{data$reg.nr[i] <- 0}

      PI30 <- unlist(L[[i]]$PI[1])
      PI60 <- unlist(L[[i]]$PI[2])  
      PI90 <- unlist(L[[i]]$PI[3])
      
      if (!is.null(PI30)){
        data$PI30[i] <- PI30
        data$PI60[i] <- PI60
        data$PI90[i] <- PI90
      }
      
    }
    return(data)
  }
  data_ARIMA <- getValues_(ARIMA)
  data_ARIMAX <- getValues_(ARIMAX)
  data <- rbind(data_ARIMA, data_ARIMAX)
}


getValues_A <- function(ARIMA, ARIMAX){
  getValues_ <- function(L){
    data <- data.frame(matrix(NA, nrow = length(L), ncol = 5))
    colnames(data) <- c("reg.lag", "reg.nr", "A30", "A60", "A90")
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
        data$f[i] <- objfun
      }

      A30 <- unlist(L[[i]]$accuracy$accuracy30[1])
      A60 <- unlist(L[[i]]$accuracy$accuracy60[1])  
      A90 <- unlist(L[[i]]$accuracy$accuracy90[1])

      if (!is.null(A30)){
        data$A30[i] <- A30
        data$A60[i] <- A60
        data$A90[i] <- A90
      }
    }
    return(data)
  }
  data_ARIMA <- getValues_(ARIMA)
  data_ARIMAX <- getValues_(ARIMAX)
  data <- rbind(data_ARIMA, data_ARIMAX)
}

data_ss_S1 <- getValues_f(ARIMA = ARIMA_ss_S1, ARIMAX = ARIMAX_ss_S1)
data_ss_S2 <- getValues_f(ARIMA = ARIMA_ss_S2, ARIMAX = ARIMAX_ss_S2)
data_ms_S1 <- getValues_f(ARIMA = ARIMA_ms_S1, ARIMAX = ARIMAX_ms_S1) 
data_ms_S2 <- getValues_f(ARIMA = ARIMA_ms_S2, ARIMAX = ARIMAX_ms_S2) 

# validate if aggregate() works correctly
# data_ss_S1[which((data_ss_S1$reg.lag == 6) & (data_ss_S1$reg.nr == 10)),]
# colMeans(data_ss_S1[which((data_ss_S1$reg.lag == 6) & (data_ss_S1$reg.nr == 10)),], na.rm = T)


data_ss_S1 <- aggregate(.~reg.lag+reg.nr, data_ss_S1, mean)
data_ss_S2 <- aggregate(.~reg.lag+reg.nr, data_ss_S2, mean)
data_ms_S1 <- aggregate(.~reg.lag+reg.nr, data_ms_S1, mean)
data_ms_S2 <- aggregate(.~reg.lag+reg.nr, data_ms_S2, mean)

data_ss_S1$Station <- rep("Dæmningen")
data_ss_S2$Station <- rep("Damhusåen")
data_ms_S1$Station <- rep("Dæmningen")
data_ms_S2$Station <- rep("Damhusåen")
data_ss_S1$ofc <- rep("Single-step")
data_ss_S2$ofc <- rep("Single-step")
data_ms_S1$ofc <- rep("Multi-step")
data_ms_S2$ofc <- rep("Multi-step")







library(wesanderson)
pal <- wes_palette("IsleofDogs2", 100, type = "continuous")


data_ss_S1$reg.lag <- as.factor(data_ss_S1$reg.lag)
data_ss_S1$reg.nr <- as.factor(data_ss_S1$reg.nr)

data_ss_S2$reg.lag <- as.factor(data_ss_S2$reg.lag)
data_ss_S2$reg.nr <- as.factor(data_ss_S2$reg.nr)

data_ms_S1$reg.lag <- as.factor(data_ms_S1$reg.lag)
data_ms_S1$reg.nr <- as.factor(data_ms_S1$reg.nr)

data_ms_S2$reg.lag <- as.factor(data_ms_S2$reg.lag)
data_ms_S2$reg.nr <- as.factor(data_ms_S2$reg.nr)



ss_s1 <- ggplot(data_ss_S1, aes(x = reg.lag, y = reg.nr, fill= (f))) +
  geom_tile() +
  geom_text(aes(label = round(f, 2)))+
  scale_fill_distiller(palette = "Spectral")+
  ggtitle("Single-step Dæmningen")+
  labs(fill="SS")+
  theme_ipsum(grid = F, base_size = 8*2, axis_title_size = 8*2,  axis_text_size = 8*2, plot_title_size = 12, plot_margin = margin(5,5,5,5), base_family = "")
ss_s2 <- ggplot(data_ss_S2, aes(x = reg.lag, y = reg.nr, fill= (f))) +
  geom_tile() +
  geom_text(aes(label = round(f, 2)))+
  ggtitle("Single-step Damhusåen")+
  labs(fill="SS")+
  scale_fill_distiller(palette = "Spectral")+
  theme_ipsum(grid = F, base_size = 8*2, axis_title_size = 8*2,  axis_text_size = 8*2, plot_title_size = 12, plot_margin = margin(5,5,5,5), base_family = "")
ms_s1 <- ggplot(data_ms_S1, aes(x = reg.lag, y = reg.nr, fill= (f))) +
  geom_tile() +
  geom_text(aes(label = round(f, 2)))+
  scale_fill_distiller(palette = "Spectral")+
  ggtitle("Multi-step Dæmningen")+
  labs(fill="SC")+
  theme_ipsum(grid = F, base_size = 8*2, axis_title_size = 8*2,  axis_text_size = 8*2, plot_title_size = 12, plot_margin = margin(5,5,5,5), base_family = "")
ms_s2 <- ggplot(data_ms_S2, aes(x = reg.lag, y = reg.nr, fill= (f))) +
  geom_tile() +
  geom_text(aes(label = round(f, 2)))+
  ggtitle("Multi-step Damhusåen")+
  labs(fill="SC")+
  scale_fill_distiller(palette = "Spectral")+
  theme_ipsum(grid = F, base_size = 8*2, axis_title_size = 8*2,  axis_text_size = 8*2, plot_title_size = 12, plot_margin = margin(5,5,5,5), base_family = "")



plot <- ggarrange(ss_s1, ss_s2, ms_s1, ms_s2, nrow = 2, ncol = 2)
plot


ggsave(filename = "../Figures/Results/Nelder-Mead/objfun_vs_regressors_NM.pdf", width = 8, height = 5)
plot
dev.off()


#### Old figure which combines legend
# 
# data <- rbind(data_ss_S1,
#               data_ss_S2,
#               data_ms_S1,
#               data_ms_S2)
# 
# library(wesanderson)
# 
# # Gradient color
# pal <- wes_palette("IsleofDogs2", 100, type = "continuous")
# 
# data$reg.lag <- as.factor(data$reg.lag)
# data$reg.nr <- as.factor(data$reg.nr)
# 
# 
# data$Station <- factor(data$Station, levels = c("Dæmningen", "Damhusåen"))
# data$ofc <- factor(data$ofc, levels = c("Single-step", "Multi-step"))
# head(data)
# plot <- ggplot(data, aes(x = reg.lag, y = reg.nr, fill= (f))) +
#   geom_tile() +
#   geom_text(aes(label = round(f, 2)))+
#   facet_grid(rows = vars(Station), cols = vars(ofc))+
#   ggtitle("Objective function value vs regressors")+
#   scale_fill_distiller(palette = "Spectral")+
#   theme_ipsum(grid = F, base_size = 8*2, axis_title_size = 8*2,  axis_text_size = 8*2, plot_title_size = 12, plot_margin = margin(5,5,5,5), base_family = "")
# 
# plot









ggsave(filename = "../Figures/Results/Nelder-Mead/objfun_vs_regressors.pdf", width = 8, height = 5)
plot
dev.off()















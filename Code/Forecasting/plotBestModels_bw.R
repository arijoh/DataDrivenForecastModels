library(xts)
library(dygraphs)
library(ggplot2)
library(scales)
library(ggpubr)

Sys.setenv(TZ = "GMT")

#### Plot best models for ARIMA

#### Load best performing models
load("Evaluation/DDS/best_models/Lists/DamningenList_PI.Rdata")
load("Evaluation/DDS/best_models/Lists/DamhusaenList_PI.Rdata")

dammningList <- DamningenList_PI
damhusaenList <- DamhusaenList_PI
dammningList_ <- sapply(X=(1:length(dammningList)), FUN = function(i){ dammningList[[i]]$model == "ARIMA" })
dammningList <- DamningenList_PI[dammningList_]
damhusaenList_ <- sapply(X=(1:length(damhusaenList)), FUN = function(i){ damhusaenList[[i]]$model == "ARIMA" })
damhusaenList <- damhusaenList[damhusaenList_]

source("Forecasting/plotModel_bw.r")
#### Load stations
d <- read.csv("../Data/Validation_data/d_validation.txt", header = TRUE, sep = "\t")
s1 <- read.csv("../Data/Validation_data/s1_validation.txt", header = TRUE, sep = "\t")
s2 <- read.csv("../Data/Validation_data/s2_validation.txt", header = TRUE, sep = "\t")

timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)
timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)
timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)
Regressor <- coredata(d_xts)
precipitation_norm <- 6.64898
station1_norm <- 9866.368
station2_norm <- 26776.54

stations <- cbind(s1_xts, s2_xts)
colnames(stations) <- c("Damning", "Damhusaen")
colnames(d_xts) <- "Precipitation"

data <- cbind(d_xts, stations)
data$Precipitation[which(is.na(data$Precipitation))] <- 0
time <- index(data)



from <- "2018-03-09 00:10:00"
to <- "2018-03-09 12:00"
model <- 1
plot_ARIMA <- plotModel(model = model, from = from, to = to, type = "ARIMA") 
plot_ARIMA




for (model in (1:5)){
  plot <- plotModel(model = model, from = from, to = to, type = "ARIMA")
  filename <- paste("../Figures/Results/DDS/Best_model_forecasts/PI/BW_ARIMA_", model, "_PI.pdf", sep = "")
  ggsave(filename = filename, height= 5, width = 8)
  print(plot)
  dev.off()
}

rm(list = ls())

















#### Plot best models for ARIMAX

#### Load best performing models
load("Evaluation/DDS/best_models/Lists/DamningenList_PI.Rdata")
load("Evaluation/DDS/best_models/Lists/DamhusaenList_PI.Rdata")

dammningList <- DamningenList_PI
damhusaenList <- DamhusaenList_PI
dammningList_ <- sapply(X=(1:length(dammningList)), FUN = function(i){ dammningList[[i]]$model == "ARIMAX" })
dammningList <- DamningenList_PI[dammningList_]
damhusaenList_ <- sapply(X=(1:length(damhusaenList)), FUN = function(i){ damhusaenList[[i]]$model == "ARIMAX" })
damhusaenList <- damhusaenList[damhusaenList_]

source("Forecasting/plotModel_bw.r")
#### Load stations
d <- read.csv("../Data/Validation_data/d_validation.txt", header = TRUE, sep = "\t")
s1 <- read.csv("../Data/Validation_data/s1_validation.txt", header = TRUE, sep = "\t")
s2 <- read.csv("../Data/Validation_data/s2_validation.txt", header = TRUE, sep = "\t")

timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)
timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)
timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)
Regressor <- coredata(d_xts)
precipitation_norm <- 6.64898
station1_norm <- 9866.368
station2_norm <- 26776.54

stations <- cbind(s1_xts, s2_xts)
colnames(stations) <- c("Damning", "Damhusaen")
colnames(d_xts) <- "Precipitation"

data <- cbind(d_xts, stations)
data$Precipitation[which(is.na(data$Precipitation))] <- 0
time <- index(data)



from <- "2018-03-09 00:10:00"
to <- "2018-03-09 12:00"
model <- 1
plot_ARIMAX <- plotModel(model = model, from = from, to = to, type = "ARIMAX") 
plot_ARIMAX

for (model in (1:5)){
  plot <- plotModel(model = model, from = from, to = to, type = "ARIMAX")
  filename <- paste("../Figures/Results/DDS/Best_model_forecasts/PI/BW_ARIMAX_", model, "_PI.pdf", sep = "")
  ggsave(filename = filename, height= 5, width = 8)
  print(plot)
  dev.off()
}







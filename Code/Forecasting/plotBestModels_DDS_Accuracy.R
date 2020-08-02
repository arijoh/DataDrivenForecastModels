library(xts)
library(dygraphs)
library(ggplot2)
library(scales)
library(ggpubr)


### NOT FULLY DONE, THE FIGURES ARE NOT VERY GOOD BECAUSE THE FORECASTS ARE CRAZY
### I THINK IT MIGHT BE ENOUGH TO JUST MENTION THAT ACCURACY SORTED EVALUATION DOES NOT GIVE GOOD MODELS
### MOSE (IF NOT ALL) OF THE MODELS ARE D=0, MAKING THEM GO CRAZY



Sys.setenv(TZ = "GMT")

#### Load best performing models
load("Evaluation/DDS/best_models/Lists/DamningenList_Accuracy.Rdata")
load("Evaluation/DDS/best_models/Lists/DamhusaenList_Accuracy.Rdata")

dammningList <- DamningenList_Accuracy
damhusaenList <- DamhusaenList_Accuracy


source("Forecasting/plotModel.r")
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


###### Here we can inspect dynamically what we want to check for.
# dygraph(data, main = "Precipitation and runoff") %>%
#   dyAxis("y", label = "Runoff [m3/h]") %>%
#   dyAxis("y2", label = "Rainfall [mm/min]", independentTicks = TRUE, valueRange = c(10, 0)) %>%
#   dySeries("Damning", axis = 'y', color = "#10C312") %>% 
#   dySeries("Damhusaen", axis = 'y', color = "#F7AB4F") %>% 
#   dySeries("Precipitation", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#42B1FA") %>% 
#   dyLegend(width = 150, labelsSeparateLines = T) %>%
#   #dyLegend(labelsDiv = "legendDivID") %>%
#   dyOptions(drawGrid = TRUE) %>%
#   dyOptions(useDataTimezone = TRUE) %>%
#   dyCrosshair(direction = "vertical") %>%
#   dyRoller(rollPeriod = 1)



from <- "2018-03-09 00:10:00"
to <- "2018-03-09 12:00"
model <- 2
plot <- plotModel(model = model, from = from, to = to)   #plotModel(types[4], i=1, from = from, to = to, )
plot

# #test
filename <- paste("../Figures/Results/Best_model_forecasts/Accuracy/model_", model, "_Accuracy.svg", sep = "")
svg(filename = filename, height= 10, width = 11)
plot
dev.off()



for (model in (1:5)){
  plot <- plotModel(model = model, from = from, to = to)
  filename <- paste("../Figures/Results/Best_model_forecasts/Accuracy/model_", model, "_Accuracy.svg", sep = "")
  svg(filename = filename, height= 10, width = 11)
  print(plot)
  dev.off()
}











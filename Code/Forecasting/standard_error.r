
library(xts)
library(dygraphs)
library(ggplot2)


Sys.setenv(TZ = "GMT")
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

# assign the "rainfall" series to the y2 axis
dygraph(data, main = "Precipitation and runoff") %>%
  dyAxis("y", label = "Runoff [m3/h]") %>%
  dyAxis("y2", label = "Rainfall [mm/min]", independentTicks = TRUE, valueRange = c(10, 0)) %>%
  dySeries("Damning", axis = 'y', color = "#10C312") %>% 
  dySeries("Damhusaen", axis = 'y', color = "#F7AB4F") %>% 
  dySeries("Precipitation", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#42B1FA") %>% 
  dyLegend(width = 150, labelsSeparateLines = T) %>%
  #dyLegend(labelsDiv = "legendDivID") %>%
  dyOptions(drawGrid = TRUE) %>%
  dyOptions(useDataTimezone = TRUE) %>%
  dyCrosshair(direction = "vertical") %>%
  dyRoller(rollPeriod = 1)

## Unnormalize data before forecasting
head(data)
data$Damhusaen <- data$Damhusaen * station2_norm
data$Damning <- data$Damning * station1_norm
data$Precipitation <- data$Precipitation * precipitation_norm
head(data)



#Cut the data before forecasting
station <- data$Damhusaen
from <- which(index(data) == as.character("2018-09-08 01:00:00"))
to <- which(index(data) == as.character("2018-09-08 21:00:00"))
x <- station[from:to,]



## Model
order <- c(3,1,2)

library(forecast)
fit <- Arima(y = x, order = order)
checkresiduals(fit)

autoplot(forecast(fit, h = 100))




#### Bellow is implementation which requires me to draw the interval, better above just to use forecast
model <- arima(x=x, order = order, fixed = coefficients)
predictions <- predict(model, n.ahead = 9)


ts_measured <- station[(to+1):(to+9)]
timestamps_predictions <- as.POSIXct(index(ts_measured))
prediction_pred <- xts(predictions$pred, order.by = timestamps_predictions)
prediction_error <-  xts(predictions$se, order.by = timestamps_predictions)

data <- data.frame(cbind(ts = x[((nrow(x)-5):nrow(x)),], 
                         ts_meas = ts_measured,
                         pred = prediction_pred, 
                         error = prediction_error))

ggplot(data, aes(x = as.POSIXct(row.names(data)))) +
  geom_point(aes(y = Damhusaen, color = "Damhusaen"), size = 0.7) + 
  geom_line(aes(y = Damhusaen, color = "Damhusaen")) +
  geom_point(aes(y = Damhusaen.1, color = "ts_meas"), size = 0.7) + 
  geom_line(aes(y = Damhusaen.1, color = "ts_meas")) +
  geom_point(aes(y = pred, color = "pred"), size = 0.7) + 
  geom_line(aes(y = pred, color = "pred"))
















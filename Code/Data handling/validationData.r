
### The data is trained based on a rain events based on precipitation and untill the rain event is finished + 12hours
### With this, the data will be trained on datapoints prior to the wet weather activation
### However, we will evaluate each model base on predictive preformance based on wet weather activation prediction.
### The treshold will be 500 m3/hr and here we will aquire the rain index and all that to evaluate the model in meta_optim



######################## Data reading ############################
d <- read.csv("../Data/Data handling/Normalized/d_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)

s1 <- read.csv("../Data/Data handling/Normalized/s1_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)

s2 <- read.csv("../Data/Data handling/Normalized/s2_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)


s1_badData <- read.csv("../Data/s1_badDAta.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1_badData$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_badData_xts <- xts(x = as.numeric(s1_badData$Flag), order.by = timestamp)

s2_badData <- read.csv("../Data/s1_badDAta.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2_badData$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_badData_xts <- xts(x = as.numeric(s2_badData$Flag), order.by = timestamp)

s1_WW <- read.csv("../Data/s1_wetWeatherEvents.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1_WW$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_WW_xts <- xts(x = as.numeric(s1_WW$Flag), order.by = timestamp)


s2_WW <- read.csv("../Data/s2_wetWeatherEvents.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2_WW$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_WW_xts <- xts(x = as.numeric(s2_WW$Flag), order.by = timestamp)



data <- cbind(s1_xts, s1_badData_xts)
data$s1_xts[which(data$s1_badData_xts != 0)] <- NA
s1_xts <- data$s1_xts # We redefine s1_xts as a time series which have taken care of bad data.

data <- cbind(s2_xts, s2_badData_xts)
data$s2_xts[which(data$s2_badData_xts != 0)] <- NA
s2_xts <- data$s2_xts # We redefine s1_xts as a time series which have taken care of bad data.


## Getting training data for now... later this will go away
validation_d_xts <- window(d_xts, start = "2018-01-01 00:10", end = "2018-12-31 23:50")
validation_s1 <- window(s1_xts, start ="2018-01-01 00:10", end =  "2018-12-31 23:50")
validation_s2 <- window(s2_xts, start ="2018-01-01 00:10", end =  "2018-12-31 23:50")
validation_regressor_s1 <- window(s1_WW_xts, start ="2018-01-01 00:10", end =  "2018-12-31 23:50")
validation_regressor_s2 <- window(s2_WW_xts, start ="2018-01-01 00:10", end =  "2018-12-31 23:50")


## Regressors!!
temp <- merge(validation_rain = validation_d_xts, 
              validation_s1, 
              validation_s2, 
              validataion_wwIndex_s1 = validation_regressor_s1, 
              validataion_wwIndex_s2 = validation_regressor_s2)


D <- temp$validation_rain
S1 <- temp$s1_xts
S2 <- temp$s2_xts
S1_WW <- temp$validataion_wwIndex_s1
S2_WW <- temp$validataion_wwIndex_s2


timestamp <- format(index(D), "%Y/%m/%d %H:%M", ts="GMT")
D <- cbind(timestamp, coredata(D))
colnames(D) <- c("Timestamp", "Value")
head(D)

timestamp <- format(index(S1), "%Y/%m/%d %H:%M", ts="GMT")
S1 <- cbind(timestamp, coredata(S1))
colnames(S1) <- c("Timestamp", "Value")
head(S1)


timestamp <- format(index(S2), "%Y/%m/%d %H:%M", ts="GMT")
S2 <- cbind(timestamp, coredata(S2))
colnames(S2) <- c("Timestamp", "Value")
head(S2)

timestamp <- format(index(S1_WW), "%Y/%m/%d %H:%M", ts="GMT")
S1_WW <- cbind(timestamp, coredata(S1_WW))
colnames(S1_WW) <- c("Timestamp", "Flag")
head(S1_WW)

timestamp <- format(index(S2_WW), "%Y/%m/%d %H:%M", ts="GMT")
S2_WW <- cbind(timestamp, coredata(S2_WW))
colnames(S2_WW) <- c("Timestamp", "Flag")
head(S2_WW)


write.table(D, file = "../Data/Validation_data/d_validation.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(S1, file = "../Data/Validation_data/s1_validation.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(S2, file = "../Data/Validation_data/s2_validation.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(S1_WW, file = "../Data/Validation_data/s1_WW_validation.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(S2_WW, file = "../Data/Validation_data/s2_WW_validation.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)








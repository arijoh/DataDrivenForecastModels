library(xts)


d <- read.csv("../Data/Data handling/Normalized/d_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)
d_xts <- window(d_xts, from = as.character("2017-08-16 10:00:00"), end = as.character("2019-12-31 23:50:00") )

s1 <- read.csv("../Data/Data handling/Normalized/s1_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)

s2 <- read.csv("../Data/Data handling/Normalized/s2_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)

s1_WW <- read.csv("../Data/s1_wetWeatherEvents.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1_WW$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_WW_xts <- xts(x = as.numeric(s1_WW$Flag), order.by = timestamp)


s2_WW <- read.csv("../Data/s2_wetWeatherEvents.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2_WW$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_WW_xts <- xts(x = as.numeric(s2_WW$Flag), order.by = timestamp)

s1_badData <- read.csv("../Data/s1_badDAta.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1_badData$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_badData_xts <- xts(x = as.numeric(s1_badData$Flag), order.by = timestamp)

s2_badData <- read.csv("../Data/s1_badDAta.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2_badData$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_badData_xts <- xts(x = as.numeric(s2_badData$Flag), order.by = timestamp)


data <- cbind(s1_xts, s1_badData_xts)
data$s1_xts[which(data$s1_badData_xts != 0)] <- NA
s1_xts <- data$s1_xts # We redefine s1_xts as a time series which have taken care of bad data.

data <- cbind(s2_xts, s2_badData_xts)
data$s2_xts[which(data$s2_badData_xts != 0)] <- NA
s2_xts <- data$s2_xts # We redefine s1_xts as a time series which have taken care of bad data.



head(d_xts)
head(s1_xts)
head(s2_xts)
head(s1_WW_xts)
head(s2_WW_xts)

tail(d_xts)
tail(s1_xts)
tail(s2_xts)
tail(s1_WW_xts)
tail(s2_WW_xts)


# Testing if everything is ok..
dygraph(cbind(d_xts, s1_xts, s1_WW_xts)) %>%   # Can try D also to visualize
  dyOptions(useDataTimezone = TRUE) #THIS FIXES TS ISSUES WITH DYGRAPH!!!!

dygraph(cbind(d_xts, s2_xts, s2_WW_xts)) %>%   # Can try D also to visualize
  dyOptions(useDataTimezone = TRUE) #THIS FIXES TS ISSUES WITH DYGRAPH!!!!


# Window data for training....
S1 <- window(s1_xts, start = "2017-08-16 13:00:00", end = "2017-12-31 23:50:00")
D  <- window(d_xts, start = "2017-08-16 13:00:00", end = "2017-12-31 23:50:00")
S2 <- window(s2_xts, start = "2017-08-16 13:00:00", end = "2017-12-31 23:50:00")
S1_WW <- window(s1_WW_xts, start = "2017-08-16 13:00:00", end = "2017-12-31 23:50:00")
S2_WW <- window(s2_WW_xts, start = "2017-08-16 13:00:00", end = "2017-12-31 23:50:00")


temp <- merge(D, S1, S2, S1_WW, S2_WW) #merge, to get them on same length


any((S1_WW == 1) && (is.na(S1)))
any((S2_WW == 1) && (is.na(S2)))

length(temp$D)
length(temp$S1)
length(temp$S2)
length(temp$S1_WW)
length(temp$S2_WW)

D <- temp$D
S1 <- temp$s1_xts
S2 <- temp$s2_xts
S1_WW <- temp$S1_WW
S2_WW <- temp$S2_WW


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


write.table(D, file = "../Data/Training_data/d_training.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(S1, file = "../Data/Training_data/s1_training.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(S2, file = "../Data/Training_data/s2_training.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(S1_WW, file = "../Data/Training_data/s1_WW_training.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(S2_WW, file = "../Data/Training_data/s2_WW_training.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)


rm(list = ls())




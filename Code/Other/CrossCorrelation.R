library(xts)

d <- read.csv("../Data/Data handling/Normalized/d_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)
d_xts <- window(d_xts, start = "2017-08-16 10:00:00", end = "2019/12/31 23:50")

s1 <- read.csv("../Data/Data handling/Normalized/s1_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)
s1_xts <- window(s1_xts, start = "2017-08-16 10:00:00", end = "2019/12/31 23:50")

s2 <- read.csv("../Data/Data handling/Normalized/s2_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)
s2_xts <- window(s2_xts, start = "2017-08-16 10:00:00", end = "2019/12/31 23:50")

data <- as.data.frame(cbind(d_xts, s1_xts, s2_xts))




pdf(file="../Figures/CCF_s1_s2.pdf", width = 5, height =  4)
par(mfrow=c(1,1))
ccf(data$s1_xts, data$s2_xts, na.action = na.pass, lag.max = 15, main = "CCF of runoff between Dæmningen and Damhusåen",ylab = "CCF")
dev.off()

 pdf(file="../Figures/CCF_d_s.pdf", width = 10, height =  4)
par(mfrow=c(1,2))
ccf(data$d, data$s1_xts, na.action = na.pass, main = "CCF of precipitation and runoff at Dæmningen",ylab = "CCF")
ccf(data$d, data$s2_xts, na.action = na.pass, main = "CCF of precipitation and runoff at Damhusåen",ylab = "CCF")
dev.off()


## CCF(S1, S2)
x <- ccf(data$s1_xts, data$s2_xts, na.action = na.pass, plot = FALSE)
x
# Highest CCF at -3



## CCF(D, S1)
x <- ccf(data$d_xts, data$s1_xts, na.action = na.pass)
x
#Highest CCF at lag -12
12/6*60 #120 min


## CCF(D, S2)
x <- ccf(data$d, data$s2_xts, na.action = na.pass, plot = FALSE)
x
# Highest CCF at -14 og -13
14/6*60 #140 min
13/6*60 #130 min




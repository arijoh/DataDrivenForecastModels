library(xts)
library(highfrequency)

d  <- read.csv("../Data/Data handling/Data_same_range/Precipitation.txt", header = TRUE, sep = "", stringsAsFactors=FALSE) #Strings as factors to remove "levels" when printed

##10 min data at t has the value of (t+1):(t+3) for 2 min data.
#So to get 10 min data, take check (t+1):(t+3) for 2 min data and imput it into t for  10 min data
#first 2 min data stars in idx=14950 ut before 10 min values are correct.

#we go from 1:9043 and move from 2-min to 10 min data, bebcause it is 10 min data
d[1:9043,4] <- d[1:9043,3]
d[1:9043,3] <- NA

#Now we take every 5th observation for t
idx <- 9044
temp <- matrix(ncol = 4, nrow = nrow(d))
colnames(temp) <- colnames(d)
counter <- 1
while (idx <= nrow(d)){
  if (!is.na(d[idx, 4])){
    temp[counter,1] <- d$Date[idx]
    temp[counter,2] <- d$Time[idx]
    temp[counter,3] <- NA
    temp[counter,4] <- d$Value.10min.[idx]
    counter = counter + 1
  }
  
  idx = idx + 1
}
temp <- temp[1:(counter-1),]

d_new <- d[1:9043,]

#Combine d_new on top of temp
dim(d_new)
dim(temp)
d <- rbind(d_new, temp)
d <- d[-3] #Drop 2min column

timestamp <- as.POSIXct(paste(d$Date, d$Time), format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value.10min.), order.by = timestamp)
timestamp <- format(timestamp, "%Y/%m/%d %H:%M", ts="GMT")

d_write <- cbind(as.character(timestamp), as.numeric(d_xts))
colnames(d_write) <- c("Timestamp", "Value")
write.table(d_write, file = "../Data/Data handling/10MinData/dataP_10minutes.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)






######################### Station 1

s1 <- read.csv("../Data/Data handling/Data_same_range/Station1.txt", header = TRUE, sep = "", stringsAsFactors=FALSE)
timestamp <- as.POSIXct(paste(s1$Date, s1$Time), tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)

#at 12:30 we calculate mean from 12:20 to 12:28 and put it to 12:30
s1_write = aggregatets(s1_xts,on="minutes",k=10, FUN = "mean");

timestamp <- as.POSIXct(s1_write, tz = "GMT")
timestamp <- format(timestamp, "%Y/%m/%d %H:%M", ts="GMT")

s1_write <- cbind(timestamp, coredata(s1_write))
colnames(s1_write) <- c("Timestamp", "Value")

write.table(s1_write, file = "../Data/10MinData/dataS1_10minutes.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)




######################### Station 2
s2 <- read.csv("../Data/Data handling/Data_same_range/Station2.txt", header = TRUE, sep = "", stringsAsFactors=FALSE)
timestamp <- as.POSIXct(paste(s2$Date, s2$Time), tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)

#at 12:30 we calculate mean from 12:20 to 12:28 and put it to 12:30
s2_write = aggregatets(s2_xts,on="minutes",k=10, FUN = "mean");

timestamp <- as.POSIXct(s2_write, tz = "GMT")
timestamp <- format(timestamp, "%Y/%m/%d %H:%M", ts="GMT")

s2_write <- cbind(timestamp, coredata(s2_write))
colnames(s2_write) <- c("Timestamp", "Value")

write.table(s2_write, file = "../Data/10MinData/dataS2_10minutes.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)


rm(list=ls()) 












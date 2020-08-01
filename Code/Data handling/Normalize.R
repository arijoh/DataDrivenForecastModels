library(xts)

d <- read.csv("../Data/Data handling/DST/DST_P.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)

s1 <- read.csv("../Data/Data handling/DST/DST_Station1.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)

s2 <- read.csv("../Data/Data handling/DST/DST_Station2.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)

plot(d_xts) #One value looks really unrealistic and is around 23
plot(s1_xts)
plot(s2_xts)

#we remove the strange value and set it to the average of the neighbouring values which is 0.
idx <- which(d_xts == max(d_xts))
d_xts[idx-1]
d_xts[idx]
d_xts[idx+1]

d_xts[idx] <- mean(d_xts[idx-1], d_xts[idx+1])

plot(d_xts) 


# Now we can move on to normalizing
min_d <- min(d_xts)
max_d <- max(d_xts)

min_s1 <- min(s1_xts, na.rm = TRUE)
max_s1 <- max(s1_xts, na.rm = TRUE)

min_s2 <- min(s2_xts, na.rm = TRUE)
max_s2 <- max(s2_xts, na.rm = TRUE)

min_d
max_d

min_s1
max_s1

min_s2
max_s2

d_xts_normalized <- (d_xts-min_d)/(max_d-min_d)
plot(d_xts_normalized)

s1_xts_normalized <- (s1_xts-min_s1)/(max_s1-min_s1)
plot(s1_xts_normalized)

s2_xts_normalized <- (s2_xts-min_s2)/(max_s2-min_s2)
plot(s2_xts_normalized)


timestamp <- format(index(d_xts_normalized), "%Y/%m/%d %H:%M", ts="GMT")
d_xts_normalized <- cbind(timestamp, coredata(d_xts_normalized))
colnames(d_xts_normalized) <- c("Timestamp", "Value")
head(d_xts_normalized)

timestamp <- format(index(s1_xts_normalized), "%Y/%m/%d %H:%M", ts="GMT")
s1_xts_normalized <- cbind(timestamp, coredata(s1_xts_normalized))
colnames(s1_xts_normalized) <- c("Timestamp", "Value")
head(s1_xts_normalized)


timestamp <- format(index(s2_xts_normalized), "%Y/%m/%d %H:%M", ts="GMT")
s2_xts_normalized <- cbind(timestamp, coredata(s2_xts_normalized))
colnames(s2_xts_normalized) <- c("Timestamp", "Value")
head(s2_xts_normalized)


write.table(d_xts_normalized, file = "../Data/Data handling/Normalized/d_normalized.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(s1_xts_normalized, file = "../Data/Data handling/Normalized/s1_normalized.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(s2_xts_normalized, file = "../Data/Data handling/Normalized/s2_normalized.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)









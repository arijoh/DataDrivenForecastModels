library(plotly)
library(highfrequency)
library(ggplot2)
library(xts)
######################### Read data
d <- read.csv("../Data/Data handling/DST/DST_P.txt", header = TRUE, sep = "\t")
s1 <- read.csv("../Data/Data handling/DST/DST_Station1.txt", header = TRUE, sep = "\t")
s2 <- read.csv("../Data/Data handling/DST/DST_Station2.txt", header = TRUE, sep = "\t")

# d <- read.csv("../Data/Data handling/Normalized/d_normalized.txt", header = TRUE, sep = "\t")
# s1 <- read.csv("../Data/Data handling/Normalized/s1_normalized.txt", header = TRUE, sep = "\t")
# s2 <- read.csv("../Data/Data handling/Normalized/s2_normalized.txt", header = TRUE, sep = "\t")



timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)

timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)

timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)

######Maybe remove the bad data intervals

#at 12:30 we calculate mean from 12:20 to 12:28 and put it to 12:30
d_xts <-  aggregatets(d_xts,on="hours",k=2, FUN = "sum");
s1_xts <-  aggregatets(s1_xts,on="hours",k=2, FUN = "sum");
s2_xts <-  aggregatets(s2_xts,on="hours",k=2, FUN = "sum");


s1_p_compare <- cbind(d_xts, s1_xts)
s1_p_compare <- as.data.frame(s1_p_compare)
s1_p_compare <- cbind(as.character(rownames(s1_p_compare)), s1_p_compare)
colnames(s1_p_compare) <- c("Timestamp", "Precipitation", "Runoff")
rownames(s1_p_compare) <- (1:nrow(s1_p_compare))
head(s1_p_compare)

s2_p_compare <- cbind(d_xts, s2_xts)
colnames(s2_p_compare) <- c("Precipitation", "Runoff")
s2_p_compare <- as.data.frame(s2_p_compare)
s2_p_compare <- cbind(as.character(rownames(s2_p_compare)), s2_p_compare)
colnames(s2_p_compare) <- c("Timestamp", "Precipitation", "Runoff")
rownames(s2_p_compare) <- (1:nrow(s2_p_compare))
head(s2_p_compare)


#Combine the data frames with station labebl
s1_p_compare$Station <- "Station 1"
s2_p_compare$Station <- "Station 2"
data <- rbind(s1_p_compare, s2_p_compare)
head(data)
tail(data)

write.table(data, file = "../Data/PrecipitationvsRunoff.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)


p1 <- ggplot(data = data, aes(x=Runoff, y=Precipitation, name=Timestamp, color = Station)) +
  geom_point(size = 0.5)+
  theme_bw() +
  labs(x = "Precipitation [mm/min]", 
       y = "Runoff at stations [m3/h]") + 
  ggtitle("Precipitation vs Runoff")

p1


ggplotly(p1)





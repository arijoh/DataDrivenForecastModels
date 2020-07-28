# We cannot pick anything in between 1.oct to 31 des 2019
library(dygraphs)
library(xts)
library(ggplot2)

extractRE <- function(RE_times){
  RE_runoff_s1 <- window(s1_xts, start = RE_times[1], end = RE_times[2])
  RE_runoff_s2 <- window(s2_xts, start = RE_times[1], end = RE_times[2])
  RE_precipitation <- window(d_xts, start = RE_times[1], end = RE_times[2])
  pRE_runoff_s1 <- window(s1_xts, start = as.character(as.POSIXct(RE_times[1], tz = "GMT")- n_pRE), end = as.character(as.POSIXct(RE_times[1], tz = "GMT")- 60*10))
  pRE_runoff_s2 <- window(s2_xts, start = as.character(as.POSIXct(RE_times[1], tz = "GMT")- n_pRE), end = as.character(as.POSIXct(RE_times[1], tz = "GMT")- 60*10))
  pRE_precipitation <- window(d_xts, start = as.character(as.POSIXct(RE_times[1], tz = "GMT")- n_pRE), end = as.character(as.POSIXct(RE_times[1], tz = "GMT")- 60*10))
  pRE <- as.data.frame(cbind(Runoff_s1 = pRE_runoff_s1, Runoff_s2 = pRE_runoff_s2, Precipitation = pRE_precipitation))
  RE <-  as.data.frame(cbind(Runoff_s1 = RE_runoff_s1, Runoff_s2 = RE_runoff_s2, Precipitation = RE_precipitation))
  output <- list(pRE, RE)
  return(output)
}




Sys.setenv(TZ = "GMT")

d <- read.csv("../Data/Data handling/DST/DST_P.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)
d_xts <- window(d_xts, start = "2017-08-16 10:00:00", end = "2019/12/31 23:50")

s1 <- read.csv("../Data/Data handling/DST/DST_Station1.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)
s1_xts <- window(s1_xts, start = "2017-08-16 10:00:00", end = "2019/12/31 23:50")

s2 <- read.csv("../Data/Data handling/DST/DST_Station2.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)
s2_xts <- window(s2_xts, start = "2017-08-16 10:00:00", end = "2019/12/31 23:50")


s1_wwIndex <- read.csv("../Data/s1_wetWeatherEvents.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s1_wwIndex$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_wwIndex_xts <- xts(x = as.numeric(s1_wwIndex$Flag), order.by = timestamp)
s1_wwIndex_xts <- window(s1_wwIndex_xts, start = "2017-08-16 10:00:00", end = "2019/12/31 23:50")

s2_wwIndex <- read.csv("../Data/s2_wetWeatherEvents.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXct(s2_wwIndex$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_wwIndex_xts <- xts(x = as.numeric(s2_wwIndex$Flag), order.by = timestamp)
s2_wwIndex_xts <- window(s2_wwIndex_xts, start = "2017-08-16 10:00:00", end = "2019/12/31 23:50")

#s1 and s2 have different ww index,
all(s1_wwIndex_xts == s2_wwIndex_xts)
any(is.na(s1_wwIndex_xts))
any(is.na(s2_wwIndex_xts))

### We decide to merge the wet weather index
data <- cbind(s1_wwIndex_xts, s2_wwIndex_xts)
data$wwIndex <- 0
data$wwIndex[which((data$s1_wwIndex_xts == 1) & (data$s2_wwIndex_xts == 1))] <- 1
wwIndex_xts <- data$wwIndex
## Here we can check stuff
# was data_ but changed to data
data <- cbind(Runoff_s1 = s1_xts, Runoff_s2 = s2_xts, Precipitation = d_xts, wwIndex = wwIndex_xts)
dygraph(data) %>%
  dyOptions(useDataTimezone = TRUE)



## We need to merge this to get the same length
length(s1_wwIndex_xts)
length(s2_wwIndex_xts)
length(d_xts)
length(s1_xts)
nrow(data)


# We go through wwIndex and check when a sequence of 1s starts or stops
# We then store the indices, then we can manage RE more easily
indices <- as.data.frame(matrix(ncol = 2, nrow = 500))
colnames(indices) <- c("Start", "Stop")
counter <- 0
temp <- coredata(data$wwIndex)
for (i in (2:nrow(data))){
  if ((temp[i-1] == 0) & (temp[i] == 1)){
    ## Store start index (i)
    counter = counter + 1
    indices$Start[counter] <- i
  }
  else if  ((temp[i-1] == 1) & (temp[i] == 0)){
    ## Store stop index (i-1)
    indices$Stop[counter] <- i-1
  }
}
indices <- na.omit(indices)

## This makes a listfor each rain event with the duration (hrs), volume (xx), and rainfall/time (xx/hrs)
rainEvents <- as.data.frame(matrix(nrow = 0, ncol = 8))
colnames(rainEvents) <- c("rain_duration", 
                          "rain_sum", 
                          "rain_sumPerhour",
                          "runoff_sum_s1",
                          "runoff_sum_s2",
                          "peak_s1",
                          "peak_s2",
                          "Treshold tigger")

tresh <- 5000 # From report
n_pRE <- 2*60*60
counter <- 1
for (i in (1:nrow(indices))){
  if (any(is.na(data$Precipitation[(indices[i, 1]-(n_pRE/(60*10))):indices[i, 2], ])) == TRUE){
    print("NA's")
  }
  else{
    rain_duration <- (indices[i,2]-indices[i,1])/6
    rain_sum <- sum(data$Precipitation[indices[i,1]:indices[i,2]])
    rain_sumPerhour <- rain_sum/rain_duration
    
    runoff_sum_s1 <- sum(data$Runoff_s1[indices[i,1]:indices[i,2]])
    runoff_sum_s2 <- sum(data$Runoff_s2[indices[i,1]:indices[i,2]])
    
    peak_s1 <- max(data$Runoff_s1[indices[i,1]:indices[i,2]], na.rm = T)
    peak_s2 <- max(data$Runoff_s2[indices[i,1]:indices[i,2]], na.rm = T)
    
    if (peak_s2 >= tresh){
      tresh_triggered <- 1
    }else{
      tresh_triggered <- 0
    }
    templist <- c(
                  rain_duration, 
                  rain_sum, 
                  rain_sumPerhour,
                  runoff_sum_s1,
                  runoff_sum_s2,
                  peak_s1,
                  peak_s2,
                  tresh_triggered)
    
    if (counter == 1){
      rainEvents[i,] <- templist
      counter = counter + 1
    }
    else{
      rainEvents[i,] <- templist
      counter = counter + 1
    }
  }
}
rainEvents
tail(rainEvents)

## Now we can pick rain events and pRE should not include NA's


which(max(rainEvents$peak_s2, na.rm = TRUE) == rainEvents$peak_s2)
which.min(abs(quantile(rainEvents$peak_s2, 0.75, na.rm = TRUE)-rainEvents$peak_s2))
rainEvents[82,]
rainEvents[62,]
which(max(rainEvents$rain_sumPerhour, na.rm = TRUE) == rainEvents$rain_sumPerhour)
which.min(abs(quantile(rainEvents$rain_sumPerhour, 0.75, na.rm = TRUE)-rainEvents$rain_sumPerhour))
rainEvents[216,] 
rainEvents[33,]
which(max(rainEvents$peak_s1, na.rm = TRUE) == rainEvents$peak_s1)
which.min(abs(quantile(rainEvents$peak_s1, 0.75, na.rm = TRUE)-rainEvents$peak_s1))
rainEvents[251,]
rainEvents[247,]
## pick rain events
selected <- c(82, 62, 216, 33, 251, 247)

# Rain plots
library(reshape)
precipitationColor <- "#69b3a2"
Station1color <-  "#10C312"
Station2color <-  "#F7AB4F"


data1 <- as.data.frame(cbind(rainEvents$rain_sum[selected], paste("RE", selected)))#, data$runoff_sum_s1, data$runoff_sum_s2))
colnames(data1) <- c("value", "ID")
data1 <- melt(data1, id.vars='ID')
data1$value <- as.numeric(levels(data1$value)[data1$value])
data1

data4 <- as.data.frame(cbind(rainEvents$rain_duration[selected], paste("RE", selected)))
colnames(data4) <- c("rain_duration", "ID")
data4 <- melt(data4, id.vars='ID')
data4$value <- as.numeric(levels(data4$value)[data4$value])
data4

data2 <- as.data.frame(cbind(rainEvents$runoff_sum_s1[selected], rainEvents$runoff_sum_s2[selected], paste("RE", selected)))
colnames(data2) <- c("Station 1", "Station 2", "ID")
data2 <- melt(data2, id.vars='ID')
data2$value <- as.numeric(levels(data2$value)[data2$value])
data2

data3 <- as.data.frame(cbind(rainEvents$peak_s1[selected], rainEvents$peak_s2[selected], paste("RE", selected)))
colnames(data3) <- c("Station 1", "Station 2", "ID")
data3 <- melt(data3, id.vars='ID')
data3$value <- as.numeric(levels(data3$value)[data3$value])
data3

scaleFUN <- function(x) sprintf("%.2f", x)

p1 <- ggplot(data1, aes(ID, value)) +
  geom_bar(aes(fill = variable), stat="identity", fill = precipitationColor) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab("Total rainfall [mm]") +
  xlab("")+
  scale_y_continuous(labels=scaleFUN)


p4 <- ggplot(data4, aes(ID, value)) +
  geom_bar(aes(fill = variable), stat="identity", fill = precipitationColor) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab("")+
  ylab("Rain duration [hrs]") + 
  scale_y_continuous(labels=scaleFUN)


p2 <- ggplot(data2, aes(ID, value)) +
  geom_bar(aes(fill = variable), stat="identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab("Total runoff") + 
  xlab("") +
  scale_fill_manual("", values = c("Station 1" = Station1color, "Station 2" = Station2color))+
  scale_y_continuous(labels=scaleFUN)


p3 <- ggplot(data3, aes(ID, value)) +
  geom_bar(aes(fill = variable), stat="identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ylab("Peak runoff") + 
  xlab("")+
  scale_fill_manual("", values = c("Station 1" = Station1color, "Station 2" = Station2color)) +
  scale_y_continuous(labels=scaleFUN)



library(ggpubr)
pp1 <- ggarrange(p1, p4, nrow = 1, ncol = 2)
pp2 <- ggarrange(p2, p3, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom", label.x = F)
ggarrange(pp1, pp2, nrow = 2, ncol = 1)


# For now  we pick ranodm rain events 
# but we need a code that detects rain events and maybe classifies them into subsets

# can remove
#data$wwIndex[indices[1,1]] == as.character("2017-08-17 18:40:00 ")
#index(data$wwIndex[indices[1,1]]) == as.character("2017-08-17 18:40:00 ")
RE1_times <- c(as.character(index(data$wwIndex[indices[selected[1],1]])),   as.character(index(data$wwIndex[indices[selected[1],2]])))
RE2_times <- c(as.character(index(data$wwIndex[indices[selected[2],1]])),  as.character(index(data$wwIndex[indices[selected[2],2]])))
RE3_times <- c(as.character(index(data$wwIndex[indices[selected[3],1]])), as.character(index(data$wwIndex[indices[selected[3],2]])))
RE4_times <- c(as.character(index(data$wwIndex[indices[selected[4],1]])), as.character(index(data$wwIndex[indices[selected[4],2]])))
RE5_times <- c(as.character(index(data$wwIndex[indices[selected[5],1]])), as.character(index(data$wwIndex[indices[selected[5],2]])))
RE6_times <- c(as.character(index(data$wwIndex[indices[selected[6],1]])), as.character(index(data$wwIndex[indices[selected[6],2]])))


output <- extractRE(RE1_times)
pRE1 <- as.data.frame(output[1])
RE1 <- as.data.frame(output[2])

output <- extractRE(RE2_times)
pRE2 <- as.data.frame(output[1])
RE2 <- as.data.frame(output[2])

output <- extractRE(RE3_times)
pRE3 <- as.data.frame(output[1])
RE3 <- as.data.frame(output[2])

output <- extractRE(RE4_times)
pRE4 <- as.data.frame(output[1])
RE4 <- as.data.frame(output[2])

output <- extractRE(RE5_times)
pRE5 <- as.data.frame(output[1])
RE5 <- as.data.frame(output[2])

output <- extractRE(RE6_times)
pRE6 <- as.data.frame(output[1])
RE6 <- as.data.frame(output[2])





writeFormat <- function(x){
  timestamp <- format(as.POSIXct(row.names(x)), "%Y/%m/%d %H:%M", ts="GMT")
  x <- cbind(timestamp, x$Runoff_s1, x$Runoff_s2, x$Precipitation)
  colnames(x) <- c("Timestamp", "Runoff_s1", "Runoff_s2", "Precipitation")
  
  return(x)
}

pRE1 <- writeFormat(pRE1)
pRE2 <- writeFormat(pRE2)
pRE3 <- writeFormat(pRE3)
pRE4 <- writeFormat(pRE4)
pRE5 <- writeFormat(pRE5)
pRE6 <- writeFormat(pRE6)

RE1 <- writeFormat(RE1)
RE2 <- writeFormat(RE2)
RE3 <- writeFormat(RE3)
RE4 <- writeFormat(RE4)
RE5 <- writeFormat(RE5)
RE6 <- writeFormat(RE6)

write.table(pRE1, file = "../Data/Testing data/pRE/pRE1.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(pRE2, file = "../Data/Testing data/pRE/pRE2.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(pRE3, file = "../Data/Testing data/pRE/pRE3.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(pRE4, file = "../Data/Testing data/pRE/pRE4.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(pRE5, file = "../Data/Testing data/pRE/pRE5.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(pRE6, file = "../Data/Testing data/pRE/pRE6.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)

write.table(RE1, file = "../Data/Testing data/RE/RE1.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(RE2, file = "../Data/Testing data/RE/RE2.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(RE3, file = "../Data/Testing data/RE/RE3.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(RE4, file = "../Data/Testing data/RE/RE4.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(RE5, file = "../Data/Testing data/RE/RE5.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(RE6, file = "../Data/Testing data/RE/RE6.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)


library(xtable)

xportTable <- function(X){
  temp <- data.frame(matrix(nrow = nrow(X), ncol = ncol(X)))
  colnames(temp) <- colnames(X)
  for (i in (1:nrow(X))){
    
    temp$rain_duration[i] <- as.character(sprintf("%.1f", unlist(X$rain_duration[i])))
    temp$rain_sum[i] <- as.character(sprintf("%1.1f", unlist(X$rain_sum[i])))
    temp$runoff_sum_s1[i] <- as.character(sprintf("%.1f", unlist(X$runoff_sum_s1[i])))
    temp$runoff_sum_s2[i] <- as.character(sprintf("%.1f", unlist(X$runoff_sum_s2[i])))
    temp$peak_s1[i] <- as.character(sprintf("%.1f", unlist(X$peak_s1[i])))
    temp$peak_s2[i] <- as.character(sprintf("%.1f", unlist(X$peak_s2[i])))
    temp$Treshold_tigger_s1[i] <- as.character(sprintf("% .2f", unlist(X$Treshold_tigger_s1[i])))
    temp$Treshold_tigger_s2[i] <- as.character(sprintf("% .2f", unlist(X$Treshold_tigger_s2[i])))
  
  }
  temp <- temp[, -3]
  return(xtable(temp, type = "latex", file = "temp.tex",))
}
# sprintf("% .4f", unlist(X[[i]]["parameters"])

xportTable(data)



rm(list = ls())





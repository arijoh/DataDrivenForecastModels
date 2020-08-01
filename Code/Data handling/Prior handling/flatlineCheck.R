library(xts)
library(dygraphs)

flatlineChecker <- function(x_xts, W){
  
  QI <- vector()
  for (i in 1:(length(x_xts)-W)){
    s <- seq(from = i, to = i+W-1)
    temp <- coredata(x_xts[s])
      if (((max(temp)-min(temp)) == 0) | (is.na(max(temp)-min(temp))) | (any(temp==0))){ ##condition for flatlining
        QI[i] <- 1
      }
      else{
        QI[i] <- 0
      }
    }
    QI[(length(x_xts)-W+1):length(x_xts)] <- TRUE
    QI <- xts(x = QI, order.by = index(x_xts))
    return(QI)
}
countWindows <- function(QI){
  
  ## Count how many good snippets we have
  count <- 0
  for (i in 1:(length(QI)-1)){
    
    if (coredata(QI[i]) != coredata(QI[i+1])){
      count = count + 1 
    }
    
  }
  ## because we switch bebtwen two states so if we have 6 transitions, we have 3 good states and 3 bad states
  count = ceiling(count/2)
  return(count)
}
when_flatline <- function(QI, nr_of_windows){
  
  store <- data.frame(ncol = 2, nrow = (nr_of_windows))
  counter <- 1
  counterold <- 0
  for (i in 1:(nrow(QI)-1)){ ## We only keep the bad intervals
    if ((counterold != counter) & (coredata(QI[i] == 1))){
      start <- index(QI[i])
      counterold <- counter
    }
    if ((coredata(QI[i] == 1)) & (coredata(QI[i+1] == 0))){
      end <- index(QI[i])
      store[counter, 1] <- as.character(start)
      store[counter, 2] <- as.character(end)
      counter = counter + 1
    }
  }
  colnames(store) <- c("From", "To")
  return(store)
}
addDyshades <- function(dg, flatlineDays){
  for( i in 1:nrow(flatlineDays)) {
    dg <- dyShading(dg, from = flatlineDays[i, 1] , to = flatlineDays[i, 2], color = "#FFAAAA")
  }
  return(dg)
}

### Read in data
d <- read.csv("../Data/Data handling/DST/DST_P.txt", header = TRUE, sep = "\t")
s1 <- read.csv("../Data/Data handling/DST/DST_Station1.txt", header = TRUE, sep = "\t")
s2 <- read.csv("../Data/Data handling/DST/DST_Station2.txt", header = TRUE, sep = "\t")

#Define window size
W <- 5 #window size.


###################### Station 1

timestamp <- as.POSIXct((s1$Timestamp), format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)
QI_s1 <- flatlineChecker(s1_xts, W)

nr_of_windows_s1 <- countWindows(QI_s1) #Maybe detrermine the mean and std size of each window
flatlineDays <- when_flatline(QI_s1, nr_of_windows_s1)
write.table(flatlineDays, file = "../Data/Data handling/FlatlineDays/FlatlineDays_S1.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)

dg_s1 <- dygraph(s1_xts) %>%
  dyOptions(useDataTimezone = TRUE)

dg_s1 <- addDyshades(dg_s1, flatlineDays)
dg_s1

# test <- cbind(QI_s1, s1_xts)
# dygraph(test) %>%
#   dyOptions(useDataTimezone = TRUE)


###################### Station 2
timestamp <- as.POSIXct((s2$Timestamp), format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)
QI_s2 <- flatlineChecker(s2_xts, W)

nr_of_windows_s2 <- countWindows(QI_s2) #Maybe detrermine the mean and std size of each window
flatlineDays <- when_flatline(QI_s2, nr_of_windows_s2)
write.table(flatlineDays, file = "../Data/Data handling/FlatlineDays/FlatlineDays_S2.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)

dg_s2 <- dygraph(s2_xts)  %>%
  dyOptions(useDataTimezone = TRUE)

dg_s2 <- addDyshades(dg_s2, flatlineDays)
dg_s2


#No flatlines in precipitation


timestamp <- format(index(QI_s1), "%Y/%m/%d %H:%M", tz="GMT")
QI_s1 <- cbind(timestamp, coredata(QI_s1))
colnames(QI_s1) <- c("Timestamp", "Flag")
head(QI_s1)


timestamp <- format(index(QI_s2), "%Y/%m/%d %H:%M", tz="GMT")
QI_s2 <- cbind(timestamp, coredata(QI_s2))
colnames(QI_s2) <- c("Timestamp", "Flag")
head(QI_s2)


write.table(QI_s1, file = "../Data/s1_badData.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(QI_s2, file = "../Data/s2_badData.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)







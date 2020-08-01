library(xts)

removeDST <- function(x, dst){
  
  timestamp <- as.POSIXct(x$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
  
  ano <- 1
  while(ano<nrow(dst)){
    wintershift <- as.POSIXct(dst[ano,1], format="%Y/%m/%d %H:%M", tz = "GMT")
    ano = ano + 1
    summershift <- as.POSIXct(dst[ano,2], format="%Y/%m/%d %H:%M", tz = "GMT")
    
    
    idx1 <- which(timestamp == wintershift)[2] #First value to shift 1 hour 
    idx2 <- which(timestamp == summershift) # Last value to shift
    
    if (length(idx2) == 0){
      idx2 <- length(timestamp)
    }
    
    for (i in (1:length(timestamp))){
      if((i>=idx1) & (i<=idx2)){
        timestamp[i] <- timestamp[i] + 60*60
      }
    }
  }
  d_xts <- xts(x = as.numeric(x$Value), order.by = timestamp)
  return(d_xts)
}


removeDST_Stations <- function(x, dst){
  timestamp <- as.POSIXct(paste(x$Date, x$Time), format="%Y/%m/%d %H:%M", tz = "GMT")
  d_xts <- xts(x = as.numeric(x$Value), order.by = timestamp) 
  
  
  ano <- 1
  d_xts_new <- d_xts
  while(ano<nrow(dst)){
    wintershift <- as.POSIXct(dst[ano,1], format="%Y/%m/%d %H:%M", tz = "GMT")
    ano = ano + 1
    summershift <- as.POSIXct(dst[ano,2], format="%Y/%m/%d %H:%M", tz = "GMT") + 60*10
    
    idx1 <- which(timestamp == wintershift) #First value to shift 1 hour 
    idx2 <- which(timestamp == summershift) # Last value to shift
    
    if (length(idx2) == 0){
      idx2 <- length(d_xts)
    }
    
    for (i in (1:(length(d_xts_new)-6))){
      if((i>=idx1) & (i<=idx2)){
        coredata(d_xts_new[i+6]) <- coredata(d_xts[i])
      }
    }
  }
  
  return(d_xts_new)
}


testDST <- function(x, x_xts_new){
  timestamp <- as.POSIXct(x$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
  d_xts <- xts(x = as.numeric(x$Value), order.by = timestamp)
  z <- cbind(d_xts, x_xts_new)
  z <- as.data.frame(z)
  return(z)
}


## We read in the 10 miute sereis for forecasts and the 2 minute series for the runoffs.
d <- read.csv("../Data/Data handling/10MinData/dataP_10minutes.txt", header = TRUE, sep = "\t", stringsAsFactors=FALSE)
s1 <- read.csv("../Data/Data handling/10MinData/dataS1_10minutes.txt", header = TRUE, sep = "\t", stringsAsFactors=FALSE)
s2 <- read.csv("../Data/Data handling/10MinData/dataS2_10minutes.txt", header = TRUE, sep = "\t", stringsAsFactors=FALSE)
dst <- read.csv("../Data/Data handling/DaylightSavingDates.txt", header = TRUE, sep = "\t", stringsAsFactors=FALSE)



## Precipitation
d_xts_new <- removeDST(d, dst) ## The new timestamps seem to be correct
#z <- testDST(d, d_xts_new) ## Look here if want to test
timestamp <- format(index(d_xts_new), "%Y/%m/%d %H:%M", ts="GMT")
d_write <- cbind(timestamp, coredata(d_xts_new))
colnames(d_write) <- c("Timestamp", "Value")
write.table(d_write, file = "../Data/Data handling/DST/DST_P.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)


## Data from stations is different because duplicated values have most likely be removed (unlike for precipitation)
# Thus we use similar function, but not quite the same
## S1
s1_xts_new <- removeDST_Stations(s1, dst)
z <- testDST(s1, s1_xts_new) ## Look here if want to test
timestamp <- format(index(s1_xts_new), "%Y/%m/%d %H:%M", ts="GMT")
s1_write <- cbind(timestamp, coredata(s1_xts_new))
colnames(s1_write) <- c("Timestamp", "Value")
write.table(s1_write, file = "../Data/Data handling/DST/DST_Station1.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)


## S2
s2_xts_new <- removeDST_Stations(s2, dst)
z <- testDST(s1, s1_xts_new) ## Look here if want to test
timestamp <- format(index(s2_xts_new), "%Y/%m/%d %H:%M", ts="GMT")
s2_write <- cbind(timestamp, coredata(s2_xts_new))
colnames(s2_write) <- c("Timestamp", "Value")
write.table(s2_write, file = "../Data/Data handling/DST/DST_Station2.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)



rm(list=ls()) 





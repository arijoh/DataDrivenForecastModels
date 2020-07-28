library(xts)
library(dygraphs)

addDyshades <- function(dg, flatlineDays){
  for( i in 1:nrow(flatlineDays)) {
    dg <- dyShading(dg, from = flatlineDays[i, 1] , to = flatlineDays[i, 2], color = "#FFAAAA")
  }
  return(dg)
}
getRainIdx <- function(D, THRESH){
  ### For loop to merge close rain events
  
  # Get start point of rain event
  # Get end point of rain event
  idx_start <- as.POSIXct(vector())
  idx_stop <- as.POSIXct(vector())
  start_counter <- 1
  stop_counter <- 1
  i <- 1
  while (i < (length(D))){
    
    temp <- coredata(D[i])
    
    if (is.na(temp)){i = i + 1}
    
    else if (temp  > THRESH){
      idx_start[start_counter] <- (index(D[i]))
      start_counter = start_counter + 1
      
      ii <- i
      while (ii < (length(D))){
        
        temp <- coredata(D[ii])
        
        if (is.na(temp)){ii = ii + 1}
        
        else if (temp <= THRESH){
          idx_stop[stop_counter] <- (index(D[ii]))
          stop_counter = stop_counter + 1
          i = ii
          break;
        }
        else {ii = ii + 1}
      }
    }
    else {i = i + 1}
  }
  
  return(c(idx_start, idx_stop))
}
mergeRain <- function(D, idx_start, idx_stop){
  
  merge = 0
  
  ## Merging rain events
  for (i in (1:(length(idx_start)-1))){
    
    
    start_1 <- idx_start[i]
    stop_1 <- idx_stop[i]
    
    start_2 <- idx_start[i+1]
    stop_2 <- idx_stop[i+1]
    
    # If start_2 - start_1 < 4 hours, merge rain events
    if (abs(as.numeric(difftime(start_2, start_1, units = "mins"))) < (4*60)){
      idx1 <- which(index(D) == start_1)
      idx2 <-  which(index(D) == stop_2)
      D[idx1:idx2] <- 1
      merge = merge + 1
    }
    
    # If start_2 - stop_1 < 2 hours, merge rain events
    if (abs(as.numeric(difftime(start_2, stop_1, units = "mins"))) < (2*60)){
      idx1 <- which(index(D) == start_1)
      idx2 <-  which(index(D) == stop_1)
      D[idx1:idx2] <- 1
      merge = merge + 1
    }
  }
  
  if (merge > 0){return(D)}
  else {return("Merge completed")}
  
}

###### Data loading
Sys.setenv(TZ = "GMT")

d <- read.csv("../Data/Data handling/Normalized/d_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXlt(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)
d_xts <- window(d_xts, from = as.character("2017-08-16 10:00:00"), end = as.character("2019-12-31 23:50:00") )

s1 <- read.csv("../Data/Data handling/Normalized/s1_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXlt(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)

s1_badData <- read.csv("../Data/s1_badData.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXlt(s1_badData$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_badData_xts <- xts(x = as.numeric(s1_badData$Flag), order.by = timestamp)


s2 <- read.csv("../Data/Data handling/Normalized/s2_normalized.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXlt(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)

s2_badData <- read.csv("../Data/s2_badData.txt", header = TRUE, sep = "\t")
timestamp <- as.POSIXlt(s2_badData$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_badData_xts <- xts(x = as.numeric(s2_badData$Flag), order.by = timestamp)

######  S2 bellow

################################
## The goal is to take the bad data index (flatlinechecker) and merge it with rain index 
# We start by changing all D values to 1 if they are above certain threshold and call it D_flag 
# Thereafter, we use the new vector (D_flag_xts) to merge temporarely close rain events, calling the vector again D_flag_xts
# Then, we add 'redefine' the rain events as D_flag_xts + 12 hours after each rain event.
# Finally, Next we use these binary vectors (Station_badData_xts and D_flag) to remove rain events with NA or bad data.
################################


#D is set to 1 if it is more that treshold
THRESH <- 0.01
D_flag_xts <- d_xts
D_flag_xts[D_flag_xts<THRESH] <- 0
D_flag_xts[D_flag_xts>=THRESH] <- 1


output <- getRainIdx(D_flag_xts, THRESH)
idx_start <- output[1:(length(output)/2)]
idx_stop <- output[((length(output)/2)+1):(length(output))]
dg <- dygraph(cbind(d_xts, s1_xts, s2_xts, s1_badData_xts, s2_badData_xts)) %>%   # Can try D also to visualize
  dyOptions(useDataTimezone = TRUE) #THIS FIXES TS ISSUES WITH DYGRAPH!!!!
dg <- addDyshades(dg,  as.data.frame(cbind(as.character(idx_start), as.character(idx_stop))))
dg

# Here we get indices and merge rain events
flag <- TRUE
counter <- 0
while (flag == TRUE){
  counter = counter + 1
  output <- getRainIdx(D_flag_xts, THRESH)
  idx_start <- output[1:(length(output)/2)]
  idx_stop <- output[((length(output)/2)+1):(length(output))]
  output <- mergeRain(D_flag_xts, idx_start, idx_stop)
  if (all(output == "Merge completed")){
    output <- getRainIdx(D_flag_xts, THRESH)
    idx_start <- output[1:(length(output)/2)]
    idx_stop <- output[((length(output)/2)+1):(length(output))]
    print(paste("Nr of iterations: ", counter))
    flag = FALSE
    }
  else {
    D_flag_xts = output
    print(paste("nr of rain events is: ", length(idx_start)))}
}


# After rainfall stops, +12 hours are taken into the rain event for the system to empty its basins
BasisRange <- 6*12
D_flag_temp_xts <- D_flag_xts
i <- 1
while (i < (length(D_flag_xts)-1)){
  if ((D_flag_xts[i] == 1) && (D_flag_xts[i+1] == 0)){
    D_flag_temp_xts[(i+1):(i+1+BasisRange)] <- 1
    i <- (i+1+BasisRange)
  }
  else{
    i = i + 1
  }
}
D_flag_xts <- D_flag_temp_xts


## Get the indices before looking if there is a bad data in between it
output <- getRainIdx(D_flag_xts, THRESH)
idx_start <- output[1:(length(output)/2)]
idx_stop <- output[((length(output)/2)+1):(length(output))]


dg <- dygraph(cbind(D_flag_xts, d_xts, s2_xts))%>%
  dyOptions(useDataTimezone = TRUE) 
dg <- addDyshades(dg,  as.data.frame(cbind(as.character(idx_start), as.character(idx_stop))))
dg

print(paste("Nr of rain events before removing NA's/badData: ", length(idx_start)))


##  We start by removing rain events from Station 1 (NAs and badData)
counter <-  0
temp <- cbind(D_flag_xts, s1_badData_xts, s1_xts)
####temp$D_flag_xts[which(is.na(temp$D_flag_xts))] <- 0 # All NA's are make 0 ,,, CANT DO THIS, THEN WE MIGHT BE SPLITTING EVENTS

for (i in (1:length(idx_start))){
  start <- idx_start[i]
  stop <- idx_stop[i]
  win <- window(temp, start = start, end = stop-1)
  
  if (any(win$s1_badData_xts == 1) || any(is.na(win$D_flag_xts)) || any(is.na(win$s1_badData_xts)) || any(is.na(win$s1_xts))){
    
    counter = counter + 1
    print(paste("Delete this rain event for i: ", i, ", nr:",  counter))
    
    
    temp$D_flag_xts[which(start == index(temp$D_flag_xts)):(which(stop == index(temp$D_flag_xts))-1)] <- 0
  }
}
D_flag_xts_s1 <- temp$D_flag_xts
D_flag_xts_s1[which(is.na(D_flag_xts_s1))] <- 0
# Getting latest indices
output <- getRainIdx(D_flag_xts_s1, THRESH)
idx_start <- output[1:(length(output)/2)]
idx_stop <- output[((length(output)/2)+1):(length(output))]
print(paste("Number of flags are:", sum(coredata(D_flag_xts_s1) == 1)))
#Number of rain event in total (before merge)
n_after_merge <- length(idx_start)
print(paste("Number of rain events after  merge: ", n_after_merge))


# Plotting latest indices red along with merged RE + 12 hours
data <- cbind(s1_xts, d_xts, D_flag_xts_s1)
dg <- dygraph(data) %>%   # Can try D also to visualize
  dyOptions(useDataTimezone = TRUE) #THIS FIXES TS ISSUES WITH DYGRAPH!!!!
dg <- addDyshades(dg,  as.data.frame(cbind(as.character(idx_start), as.character(idx_stop))))
dg




## Get the indices before looking if there is a bad data in between it
output <- getRainIdx(D_flag_xts, THRESH)
idx_start <- output[1:(length(output)/2)]
idx_stop <- output[((length(output)/2)+1):(length(output))]

##  We Then remove rain events from Station 2 (NAs and badData)
counter <-  0
temp <- cbind(D_flag_xts, s2_badData_xts, s2_xts)
for (i in (1:length(idx_start))){
  start <- idx_start[i]
  stop <- idx_stop[i]
  win <- window(temp, start = start, end = stop-1)
  if (any(win$s2_badData_xts == 1) || any(is.na(win$D_flag_xts)) || any(is.na(win$s2_badData_xts)) || any(is.na(win$s2_xts))){
    counter = counter + 1
    print(paste("Delete this rain event for i: ", i, ", nr:",  counter))
    temp$D_flag_xts[which(start == index(temp$D_flag_xts)):(which(stop == index(temp$D_flag_xts))-1)] <- 0
  }
}
temp$D_flag_xts[which(is.na(temp$D_flag_xts))] <- 0
D_flag_xts_s2 <- temp$D_flag_xts
# Getting latest indices
output <- getRainIdx(D_flag_xts_s2, THRESH)
idx_start <- output[1:(length(output)/2)]
idx_stop <- output[((length(output)/2)+1):(length(output))]
print(paste("Number of flags are:", sum(coredata(D_flag_xts_s2) == 1)))
#Number of rain event in total (before merge)
n_after_merge <- length(idx_start)
print(paste("Number of rain events after  merge: ", n_after_merge))


# Plotting latest indices red along with merged RE + 12 hours
data <- cbind(s2_xts, d_xts, D_flag_xts_s2)
dg <- dygraph(data) %>%   # Can try D also to visualize
  dyOptions(useDataTimezone = TRUE) #THIS FIXES TS ISSUES WITH DYGRAPH!!!!
dg <- addDyshades(dg,  as.data.frame(cbind(as.character(idx_start), as.character(idx_stop))))
dg



## Saving results
timestamp <- format(index(D_flag_xts_s1), "%Y/%m/%d %H:%M", ts="GMT")
WW <- cbind(timestamp, coredata(D_flag_xts_s1))
colnames(WW) <- c("Timestamp", "Flag")
head(WW)
write.table(WW, file = "../Data/s1_wetWeatherEvents.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)

## Saving results
timestamp <- format(index(D_flag_xts_s2), "%Y/%m/%d %H:%M", ts="GMT")
WW <- cbind(timestamp, coredata(D_flag_xts_s2))
colnames(WW) <- c("Timestamp", "Flag")
head(WW)
write.table(WW, file = "../Data/s2_wetWeatherEvents.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)


rm(list = ls())

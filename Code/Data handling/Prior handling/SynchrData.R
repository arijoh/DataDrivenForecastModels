# Functions that snips data
snipData <- function(x, S){ ## S=TRUE for stations, when aggeragated they go from 10:10 to 00:00 if we don't do this
  if (S == TRUE){
    timestamp <- as.POSIXct(paste(x$Date, x$Time), format="%Y/%m/%d %H:%M", tz = "GMT")
    from <- as.POSIXct("2017/08/16 09:50", format="%Y/%m/%d %H:%M", tz = "GMT")
    to <- as.POSIXct("2019/12/31 23:40", format="%Y/%m/%d %H:%M", tz = "GMT")
  }
  else{
    timestamp <- as.POSIXct(paste(x$Date, x$Time), format="%Y/%m/%d %H:%M", tz = "GMT")
    from <- as.POSIXct("2017/08/16 10:00", format="%Y/%m/%d %H:%M", tz = "GMT")
    to <- as.POSIXct("2019/12/31 23:50", format="%Y/%m/%d %H:%M", tz = "GMT")
  }
  
  from_idx <- which(timestamp == from)
  to_idx <- which(timestamp == to)
  x_write <- x[from_idx:to_idx,]
  return(x_write)
}


d <- read.csv("../Data/DMInowcast.txt", header = TRUE, sep = "", stringsAsFactors=FALSE)
# FROM 2017/07/04	12:30 TO 2020/01/01	00:00

s1 <- read.csv("../Data/GeCa01_QF.txt", header = TRUE, sep = "")
# FROM 2017/08/16	09:28 TO 2019/12/31	23:58

s2 <- read.csv("../Data/GeIn_QF.txt", header = TRUE, sep = "")
# FROM 2017/04/05	14:58 TO 2019/12/31	23:58

# So we decide to have data on the range 2017/08/16 10.00 to 2019/12/31	23:50


d_write <- snipData(d, FALSE)
s1_write <- snipData(s1, TRUE)
s2_write <- snipData(s2, TRUE)

write.table(d_write, file = "../Data/Data handling/Data_same_range/Precipitation.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(s1_write, file = "../Data/Data handling/Data_same_range/Station1.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(s2_write, file = "../Data/Data handling/Data_same_range/Station2.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)

rm(list=ls()) 



wwS1 <- read.csv("../Data/s1_wetWeatherEvents.txt", header = TRUE, sep = "\t")
wwS2 <- read.csv("../Data/s2_wetWeatherEvents.txt", header = TRUE, sep = "\t")


percentageRainEvents <- function(df){
  x <- sum(df$Flag == 1)
  y <- length(df$Flag)
  return(x/y)
}


paste(round(percentageRainEvents(wwS1),3), "% of the data is valid for training")
paste(round(percentageRainEvents(wwS2),3), "% of the data is valid for training")


countRainEvents <- function(df){
  return(sum(diff(df$Flag) == 1))
}

paste("Nr of rain events from ", wwS1$Timestamp[1], " to ", wwS1$Timestamp[length(wwS1)], "are ", countRainEvents(wwS1))
paste("Nr of rain events from ", wwS1$Timestamp[1], " to ", wwS1$Timestamp[length(wwS1)], "are ", countRainEvents(wwS2))



## Training data
start = "2017/08/17 00:10"
end = "2017/12/31 23:50"

start_idx <- which(as.character(wwS1$Timestamp) == start)
end_idx <-  which(as.character(wwS1$Timestamp) == end)
wwS1_val <- wwS1[start_idx:end_idx,]

paste(round(percentageRainEvents(wwS1_val),3), "% of the data is valid for training")
paste("Nr of rain events are ", countRainEvents(wwS1_val))

start_idx <- which(as.character(wwS2$Timestamp) == start)
end_idx <-  which(as.character(wwS2$Timestamp) == end)
wwS2_val <- wwS2[start_idx:end_idx,]

paste(round(percentageRainEvents(wwS2_val),3), "% of the data is valid for training")
paste("Nr of rain events are ", countRainEvents(wwS1_val))







### Validation data
start = "2018/01/01 00:10"
end = "2018/12/31 23:50"

start_idx <- which(as.character(wwS1$Timestamp) == start)
end_idx <-  which(as.character(wwS1$Timestamp) == end)
wwS1_val <- wwS1[start_idx:end_idx,]

paste(round(percentageRainEvents(wwS1_val),3), "% of the data is valid for training")
paste("Nr of rain events are ", countRainEvents(wwS1_val))

start_idx <- which(as.character(wwS2$Timestamp) == start)
end_idx <-  which(as.character(wwS2$Timestamp) == end)
wwS2_val <- wwS2[start_idx:end_idx,]

paste(round(percentageRainEvents(wwS2_val),3), "% of the data is valid for training")
paste("Nr of rain events are ", countRainEvents(wwS1_val))







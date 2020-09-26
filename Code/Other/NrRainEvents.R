

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





library(scales)
library(ggpubr)
library(xts)

RE1 <- read.csv("../Data/Testing data/RE/RE1.txt", header = TRUE, sep = "\t", row.names = NULL)
timestamp <- as.POSIXct(RE1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
RE1 <- cbind(Runoff_s1 = xts(x = as.numeric(RE1$Runoff_s1), order.by = timestamp),
             Runoff_s2 = xts(x = as.numeric(RE1$Runoff_s2), order.by = timestamp),
             Precipitation = xts(x = as.numeric(RE1$Precipitation), order.by = timestamp))

RE2 <- read.csv("../Data/Testing data/RE/RE2.txt", header = TRUE, sep = "\t", row.names = NULL)
timestamp <- as.POSIXct(RE2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
RE2 <- cbind(Runoff_s1 = xts(x = as.numeric(RE2$Runoff_s1), order.by = timestamp),
             Runoff_s2 = xts(x = as.numeric(RE2$Runoff_s2), order.by = timestamp),
             Precipitation = xts(x = as.numeric(RE2$Precipitation), order.by = timestamp))

RE3 <- read.csv("../Data/Testing data/RE/RE3.txt", header = TRUE, sep = "\t", row.names = NULL)
timestamp <- as.POSIXct(RE3$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
RE3 <- cbind(Runoff_s1 = xts(x = as.numeric(RE3$Runoff_s1), order.by = timestamp),
             Runoff_s2 = xts(x = as.numeric(RE3$Runoff_s2), order.by = timestamp),
             Precipitation = xts(x = as.numeric(RE3$Precipitation), order.by = timestamp))


RE4 <- read.csv("../Data/Testing data/RE/RE4.txt", header = TRUE, sep = "\t", row.names = NULL)
timestamp <- as.POSIXct(RE4$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
RE4 <- cbind(Runoff_s1 = xts(x = as.numeric(RE4$Runoff_s1), order.by = timestamp),
             Runoff_s2 = xts(x = as.numeric(RE4$Runoff_s2), order.by = timestamp),
             Precipitation = xts(x = as.numeric(RE4$Precipitation), order.by = timestamp))


RE5 <- read.csv("../Data/Testing data/RE/RE5.txt", header = TRUE, sep = "\t", row.names = NULL)
timestamp <- as.POSIXct(RE5$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
RE5 <- cbind(Runoff_s1 = xts(x = as.numeric(RE5$Runoff_s1), order.by = timestamp),
             Runoff_s2 = xts(x = as.numeric(RE5$Runoff_s2), order.by = timestamp),
             Precipitation = xts(x = as.numeric(RE5$Precipitation), order.by = timestamp))


RE6 <- read.csv("../Data/Testing data/RE/RE6.txt", header = TRUE, sep = "\t", row.names = NULL)
timestamp <- as.POSIXct(RE6$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
RE6 <- cbind(Runoff_s1 = xts(x = as.numeric(RE6$Runoff_s1), order.by = timestamp),
             Runoff_s2 = xts(x = as.numeric(RE6$Runoff_s2), order.by = timestamp),
             Precipitation = xts(x = as.numeric(RE6$Precipitation), order.by = timestamp))



plotRE <- function(RE,i){
  precipitationColor <- "#69b3a2"
  station1Color <- "#10C312"
  station2Color <- "#F7AB4F"
  station_min <- 0
  station_max <- 25000
  Precip_min <- 0
  Precip_max <- 1
  
  p1 <- ggplot(RE, aes(x = as.POSIXct(index(RE)))) +
    geom_point(aes(y = Runoff_s1, color = "Station 1"), size = 0.7) + 
    geom_line(aes(y = Runoff_s1, color = "Station 1")) + 
    geom_point(aes(y = Runoff_s2, color = "Station 2"), size = 0.7) + 
    geom_line(aes(y = Runoff_s2, color = "Station 2")) +
    scale_color_manual(values=c(station1Color, station2Color)) +
    ylim(station_min, station_max)+
    xlab("")+
    ylab("Q") +
    scale_x_datetime(labels = date_format("%Y-%m-%d %H"))
  
  p2 <- ggplot(RE, aes(x = (index(RE)))) +
    geom_bar(aes(y = Precipitation), stat="identity", size=0.1, fill=precipitationColor, color="black")+
    ylim(Precip_min, Precip_max)+
    xlab("Time")+
    ylab("P") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    ggtitle(paste("Rain event", i))
  
  #p <- egg::ggarrange(p2, p1, heights = c(0.25, 0.75))
  p <- ggarrange(p2, p1, ncol = 1, nrow = 2, heights = c(0.25, 0.75), common.legend = T, align = "v", legend = "bottom")
  return(p)
}

selected <- c(82, 62, 216, 33, 251, 247)



p1 <- plotRE(RE1, selected[1])
p2 <- plotRE(RE2, selected[2])
p3 <- plotRE(RE3, selected[3])
p4 <- plotRE(RE4, selected[4])
p5 <- plotRE(RE5, selected[5])
p6 <- plotRE(RE6, selected[6])

p1
p2
p3
p4
p5
p6
















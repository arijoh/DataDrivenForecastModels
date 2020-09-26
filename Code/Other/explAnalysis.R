library(extrafont)
extrafont::font_import()
library(xts)
library(dygraphs)
library(ggplot2)
library(scales)
library(ggpubr)
library(reshape2)

d <- read.csv("../Data/Data handling/DST/DST_P.txt", header = TRUE, sep = "\t")
s1 <- read.csv("../Data/Data handling/DST/DST_Station1.txt", header = TRUE, sep = "\t")
s2 <- read.csv("../Data/Data handling/DST/DST_Station2.txt", header = TRUE, sep = "\t")

timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)
d_xts <- window(d_xts, start = "2017-08-16 10:00:00", end = "2019-12-31 23:50:00")

timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)

timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)

stations <- cbind(s1_xts, s2_xts)
colnames(stations) <- c("Damningen", "Damhusaen")
colnames(d_xts) <- "Precipitation"



summary(d_xts)


d_xts$Precipitation[which(is.na(d_xts))] <- 0
d_xts$Precipitation[which(d_xts == max(d_xts))] <- 0
summary(d_xts)
summary(stations)

#station 1 has 43131 rain flags in total and 
#missing data/flatlines for station 1 is 8337
#station 2 has 43356 rain flags in total
#missing data/flatlines for station 2 is 8241
#should be 129656 in eachdata files

print("Fraction of rain events for station 1 are: ", 43131/(129656-8337))
print("Fraction of rain events for station 2 are: ", 43356/(129656-8337)) 

#around 35% of the data is rainy.
#we'll put the runoff threshold at the 80th precentile
quantile(stations$Damningen, probs = seq(0,1,0.1), na.rm= T)
quantile(stations$Damhusaen, probs = seq(0,1,0.1), na.rm= T)

s1_tresh <- 1800
s2-tresh <- 4300


data <- cbind(d_xts, stations)
na_n_s1 <- sum(is.na(data$Damningen) == TRUE)
na_n_s2 <- sum(is.na(data$Damhusaen) == TRUE)
na_n_d <- sum(is.na(data$Precipitation) == TRUE)
lengthSeries <- nrow(data)


print(paste("Whole series should be: ", lengthSeries))
print(paste("Number of NA for Station 1: ", na_n_s1, ", or ", (na_n_s1/lengthSeries)*100, "% of the data"))
print(paste("Number of NA for Station 2: ", na_n_s2, ", or ", (na_n_s2/lengthSeries)*100, "% of the data"))
print(paste("Number of NA for Precipitation: ", na_n_d, ", or ", (na_n_d/lengthSeries)*100, "% of the data"))




# assign the "rainfall" series to the y2 axis
dygraph(data, main = "Precipitation and runoff") %>%
  dyAxis("y", label = "Runoff [m3/h]") %>%
  dyAxis("y2", label = "Rainfall [mm/hr]", independentTicks = TRUE, valueRange = c(10, 0)) %>%
  dySeries("Damningen", axis = 'y', color = "#10C312") %>% 
  dySeries("Damhusaen", axis = 'y', color = "#F7AB4F") %>% 
  dySeries("Precipitation", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#42B1FA") %>% 
  dyLegend(width = 150, labelsSeparateLines = T) %>%
  #dyLegend(labelsDiv = "legendDivID") %>%
  dyOptions(drawGrid = TRUE) %>%
  dyOptions(useDataTimezone = TRUE) %>%
  dyCrosshair(direction = "vertical") %>%
  dyRoller(rollPeriod = 1) 



addDyshades <- function(dg, flatlineDays){
  for( i in 1:nrow(flatlineDays)) {
    dg <- dyShading(dg, from = flatlineDays[i, 1] , to = flatlineDays[i, 2], color = "#FF0000")
  }
  return(dg)
}

flatlineDaysS1 <- read.csv("../Data/Data handling/FlatlineDays/FlatlineDays_S1.txt", header = TRUE, sep = "\t")
flatlineDaysS2 <- read.csv("../Data/Data handling/FlatlineDays/FlatlineDays_S2.txt", header = TRUE, sep = "\t")



data <- cbind(d_xts, stations)
prec_na <- data$Precipitation
data$Precipitation[which(is.na(data$Precipitation))] <- 0
# assign the "rainfall" series to the y2 axis
dg <- dygraph(data, main = "Precipitation and runoff") %>%
  dyAxis("y", label = "Runoff [m3/h]") %>%
  dyAxis("y2", label = "Rainfall [mm/hr]", independentTicks = TRUE, valueRange = c(10, 0)) %>%
  dySeries("Damningen", axis = 'y', color = "#10C312") %>% 
  dySeries("Damhusaen", axis = 'y', color = "#F7AB4F") %>% 
  dySeries("Precipitation", axis = 'y2', stepPlot = TRUE, fillGraph = TRUE, color = "#42B1FA") %>% 
  dyLegend(width = 150, labelsSeparateLines = T) %>%
  #dyLegend(labelsDiv = "legendDivID") %>%
  dyOptions(drawGrid = TRUE) %>%
  dyOptions(useDataTimezone = TRUE) %>%
  dyCrosshair(direction = "vertical") %>%
  dyRoller(rollPeriod = 1)

dg <- addDyshades(dg, flatlineDaysS1)
dg <- addDyshades(dg, flatlineDaysS2)
na_idx <- index(prec_na[which(is.na(prec_na))])

as.numeric(na_idx[2]-na_idx[1])
for(i in (1:(length(na_idx)-1))) {
  if (as.numeric(na_idx[i+1]-na_idx[i]) == 10){
    dg <- dyShading(dg, from = na_idx[i] , to = na_idx[i+1], color = "#FF0000")
  }
}
dg



#### Put to ggplot

plotTS <- function(data, from, to, ymin, ymax, title){

  data <- window(data, start = from, end = to)
  data_df <- as.data.frame(data)
  timestamp <- as.POSIXct(rownames(data_df), format="%Y-%m-%d %H:%M")
  rownames(data_df) <- 1:nrow(data_df)
  data_1 <- data_df[,c(1,2)]
  data_1 <- cbind(timestamp, data_1)
  data_2 <- data_df[,c(1,3)]
  data_2 <- cbind(timestamp, data_2)
  colnames(data_1) <- c("Timestamp", "Precipitation", "Runoff")
  colnames(data_2) <- c("Timestamp", "Precipitation", "Runoff")
  data_1$Station <- rep("Dæmningen")
  data_2$Station <- rep("Damhusåen")
  data <- rbind(data_1, data_2)

  dy_r <- 1
  yl=c(ymin, ymax)
  yl_r=c(0, max(data$Precipitation, na.rm = T)) 
  scalingFactor=30000/yl_r[2]
  dummy_R=as.numeric(coredata(data$Precipitation)*scalingFactor)
  in_R  = data.frame(x1=as.character(data$Timestamp),
                     x2=c(as.character(data$Timestamp)[2:length(data$Timestamp)],as.character(data$Timestamp[length(data$Timestamp)]+60)),   
                     y1=rep(yl[2],length(data$Timestamp)),   
                     y2=(yl[2]-dummy_R))
  
  
  yTick=c(seq(yl[2]/scalingFactor,yl[2]/scalingFactor-yl_r[2],-dy_r))
  ylab=seq(0,(length(yTick)-1)*dy_r,dy_r)
  
  plot <- ggplot(data, aes(x = as.POSIXct(Timestamp, format="%Y-%m-%d %H:%M")))+
    geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
              fill='#6bdcff', colour = "#00c3ff",color=NA)+
    #geom_point(aes(y = Runoff), color = "#10C312") + 
    geom_line(aes(y = Runoff), color = "#10C312")+
     scale_x_datetime(labels = date_format("%Y/%m"))+
     scale_y_continuous(limits = yl, expand = c(0, 0), breaks=seq(yl[1],yl[2],5000),
                       sec.axis = sec_axis(~./scalingFactor, name = "Precipitation [mm/hr]          ", breaks=yTick,labels =ylab))+
    facet_grid(cols = vars(Station))+
    ylab("Runoff [m3/hr]")+xlab("Year/month")+
    ggtitle(title)+
    theme_pubclean()
    theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
          panel.grid.minor = element_blank(),
          panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"),
          legend.title = element_blank(),
          plot.title = element_text(size = 14), text = element_text(size=14), axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(plot)
}



### Full time series
from = "2017-08-16 10:00:00"
to = "2019-12-31 23:50:00"
ymin = 0
ymax = 30000
full_plot <- plotTS(data = data, from = from, to = to, ymin = ymin, ymax = ymax, title = "Full dataset (precipitation and runoff)")




# ggsave(filename="../Figures/fulldataplot.pdf", width = 10, height =  4)
# full_plot
# dev.off()




plotTS <- function(data, from, to, ymin, ymax, title){
  
  data <- window(data, start = from, end = to)
  data_df <- as.data.frame(data)
  timestamp <- as.POSIXct(rownames(data_df), format="%Y-%m-%d %H:%M")
  rownames(data_df) <- 1:nrow(data_df)
  data_1 <- data_df[,c(1,2)]
  data_1 <- cbind(timestamp, data_1)
  data_2 <- data_df[,c(1,3)]
  data_2 <- cbind(timestamp, data_2)
  colnames(data_1) <- c("Timestamp", "Precipitation", "Runoff")
  colnames(data_2) <- c("Timestamp", "Precipitation", "Runoff")
  data_1$Station <- rep("Dæmningen")
  data_2$Station <- rep("Damhusåen")
  data <- rbind(data_2, data_1)
  
  dy_r <- 0.5
  yl=c(ymin, ymax)
  yl_r=c(0, max(data$Precipitation, na.rm = T)) 
  scalingFactor=20000/yl_r[2]
  dummy_R=as.numeric(coredata(data$Precipitation)*scalingFactor)
  in_R  = data.frame(x1=as.character(data$Timestamp),
                     x2=c(as.character(data$Timestamp)[2:length(data$Timestamp)],as.character(data$Timestamp[length(data$Timestamp)]+60)),   
                     y1=rep(yl[2],length(data$Timestamp)),   
                     y2=(yl[2]-dummy_R))
  
  
  yTick=c(seq(yl[2]/scalingFactor,yl[2]/scalingFactor-yl_r[2],-dy_r))
  ylab=seq(0,(length(yTick)-1)*dy_r,dy_r)
  
  plot <- ggplot(data, aes(x = as.POSIXct(Timestamp, format="%Y-%m-%d %H:%M")))+
    geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
              fill='#6bdcff', colour = "#00c3ff",color=NA)+
    geom_point(aes(y = Runoff), color = "#009603", size = 0.8) + 
    geom_line(aes(y = Runoff), color = "#10C312")+
    scale_x_datetime(labels = date_format("%H:%M"))+
    scale_y_continuous(limits = yl, expand = c(0, 0), breaks=seq(yl[1],yl[2],5000),
                       sec.axis = sec_axis(~./scalingFactor, name = "Rain [mm/hr]          ", breaks=yTick,labels =ylab))+
    facet_grid(cols = vars(Station))+
    ylab("Runoff [m3/hr]")+xlab("Time")+
    ggtitle(title)+
    theme_pubclean()+
    theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
          panel.grid.minor = element_blank(),
          panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"),
          legend.title = element_blank(),
          plot.title = element_text(size = 14), text = element_text(size=14),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(plot)
}


## Zoom in
from = "2018-12-08 06:00:00"
to = "2018-12-09 00:10:00"
ymin = 0
ymax = 30000
zoom_plot <- plotTS(data = data, from = from, to = to, ymin = ymin, ymax = ymax, title = "Rain event on 2018/12/08")
zoom_plot

# ggsave(filename="../Figures/zoomdataplot.pdf", width = 10, height =  4)
# zoom_plot
# dev.off()







##### ggplot with flatlines labeled...
## flatlineDaysS1
## flatlineDaysS2


plotTS_flatlines <- function(data, flatlines, from, to, ymin, ymax, title){
  
  data <- window(data, start = from, end = to)

  data_df <- as.data.frame(data)
  timestamp <- as.POSIXct(rownames(data_df), format="%Y-%m-%d %H:%M")
  rownames(data_df) <- 1:nrow(data_df)
  data_1 <- data_df[,c(1,2)]
  data_1 <- cbind(timestamp, data_1)
  data_2 <- data_df[,c(1,3)]
  data_2 <- cbind(timestamp, data_2)
  colnames(data_1) <- c("Timestamp", "Precipitation", "Runoff")
  colnames(data_2) <- c("Timestamp", "Precipitation", "Runoff")
  data_1$Station <- rep("Dæmningen")
  data_2$Station <- rep("Damhusåen")
  data <- rbind(data_1, data_2)
  
  dy_r <- 1
  yl=c(ymin, ymax)
  yl_r=c(0, max(data$Precipitation, na.rm = T)) 
  scalingFactor=30000/yl_r[2]
  dummy_R=as.numeric(coredata(data$Precipitation)*scalingFactor)
  in_R  = data.frame(x1=as.character(data$Timestamp),
                     x2=c(as.character(data$Timestamp)[2:length(data$Timestamp)],as.character(data$Timestamp[length(data$Timestamp)]+60)),   
                     y1=rep(yl[2],length(data$Timestamp)),   
                     y2=(yl[2]-dummy_R))
  
  
  yTick=c(seq(yl[2]/scalingFactor,yl[2]/scalingFactor-yl_r[2],-dy_r))
  ylab=seq(0,(length(yTick)-1)*dy_r,dy_r)
  
  plot <- ggplot(data)+
    geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
              fill='#6bdcff', colour = "#00c3ff",color=NA)+
    geom_rect(data=flatlines, inherit.aes = T, aes(xmin = as.POSIXct(x1), xmax = as.POSIXct(x2),
                                                   ymin = y1, ymax = y2),
              fill='#ffadad', colour = "#ffadad",color=NA)+
    #geom_point(aes(y = Runoff), color = "#10C312") + 
    geom_line(aes(x = as.POSIXct(Timestamp, format="%Y-%m-%d %H:%M"), y = Runoff), color = "#10C312")+
    scale_x_datetime(labels = date_format("%Y/%m"))+
    scale_y_continuous(limits = yl, expand = c(0, 0), breaks=seq(yl[1],yl[2],5000),
                       sec.axis = sec_axis(~./scalingFactor, name = "Precipitation [mm/hr]          ", breaks=yTick,labels =ylab))+
    facet_grid(cols = vars(Station))+
    ylab("Runoff [m3/hr]")+xlab("Year/month")+
    ggtitle(title)+
    theme_pubclean()+
    theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
          panel.grid.minor = element_blank(),
          panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"),
          legend.title = element_blank(),
          plot.title = element_text(size = 14), text = element_text(size=14), axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(plot)
}



### Full time series
from = "2017-08-16 10:00:00"
to = "2019-12-31 23:50:00"
ymin = 0
ymax = 30000

head(flatlineDaysS1)
head(flatlineDaysS2)


flatlineDaysS1_temp  = data.frame(x1= as.character(flatlineDaysS1$From),
                                  x2= as.character(flatlineDaysS1$To),   
                                  y1=ymin,   
                                  y2=ymax)

flatlineDaysS2_temp  = data.frame(x1= as.character(flatlineDaysS2$From),
                                  x2= as.character(flatlineDaysS2$To),   
                                  y1=ymin,   
                                  y2=ymax)


flatlineDaysS1_temp$Station <- rep("Dæmningen")
flatlineDaysS2_temp$Station <- rep("Damhusåen")

head(flatlineDaysS1_temp)
head(flatlineDaysS2_temp)

flatlines <- rbind(flatlineDaysS1_temp,
                   flatlineDaysS2_temp)

full_plot_flatlines <- plotTS_flatlines(data = data, flatlines = flatlines,from = from, to = to, ymin = ymin, ymax = ymax, title = "Full dataset (precipitation and runoff)")
full_plot_flatlines





# ggsave(filename = "../Figures/fullplotflatlines.pdf", width = 10, height =  4)
# full_plot_flatlines
# dev.off()




###### merged rain events with added 12 hours
wwS1 <- read.csv("../Data/s1_wetWeatherEvents.txt", header = TRUE, sep = "\t")
wwS2 <- read.csv("../Data/s2_wetWeatherEvents.txt", header = TRUE, sep = "\t")



### Full time series
from = "2018-12-19 06:00:00"
to = "2018-12-24 06:00:00"
ymin = 0
ymax = 30000

start = which(as.POSIXct(wwS1$Timestamp) == from)
end = which(as.POSIXct(wwS1$Timestamp) == to)

wwS1 <- wwS1[(start:end), ]


start = which(as.POSIXct(wwS2$Timestamp) == from)
end = which(as.POSIXct(wwS2$Timestamp) == to)

wwS2 <- wwS2[(start:end), ]

startWW <- which(diff(wwS1$Flag)==1)
endWW <- which(diff(wwS1$Flag)==-1)

head(startWW)
head(endWW)

startWW_time <- as.POSIXct(wwS1$Timestamp)[startWW]
endWW_time <- as.POSIXct(wwS1$Timestamp)[endWW]

head(startWW_time)
head(endWW_time)

wwS1 <- as.data.frame(cbind(From = as.character(startWW_time), To = as.character(endWW_time)))



startWW <- which(diff(wwS2$Flag)==1)
endWW <- which(diff(wwS2$Flag)==-1)

head(startWW)
head(endWW)

startWW_time <- as.POSIXct(wwS2$Timestamp)[startWW]
endWW_time <- as.POSIXct(wwS2$Timestamp)[endWW]

head(startWW_time)
head(endWW_time)

wwS2 <- as.data.frame(cbind(From = as.character(startWW_time), To = as.character(endWW_time)))







wwS1_temp  = data.frame(x1= as.character(wwS1$From),
                                  x2= as.character(wwS1$To),   
                                  y1=ymin,   
                                  y2=ymax)

wwS2_temp  = data.frame(x1= as.character(wwS1$From),
                                  x2= as.character(wwS1$To),   
                                  y1=ymin,   
                                  y2=ymax)



wwS1_temp$Station <- rep("Dæmningen")
wwS2_temp$Station <- rep("Damhusåen")

head(wwS1_temp)
head(wwS2_temp)

ww <- rbind(wwS1_temp,
            wwS2_temp)






plotTS_zoom_wwindex <- function(data, from, wwindex, to, ymin, ymax, title){
  
  data <- window(data, start = from, end = to)
  data_df <- as.data.frame(data)
  timestamp <- as.POSIXct(rownames(data_df), format="%Y-%m-%d %H:%M")
  rownames(data_df) <- 1:nrow(data_df)
  data_1 <- data_df[,c(1,2)]
  data_1 <- cbind(timestamp, data_1)
  data_2 <- data_df[,c(1,3)]
  data_2 <- cbind(timestamp, data_2)
  colnames(data_1) <- c("Timestamp", "Precipitation", "Runoff")
  colnames(data_2) <- c("Timestamp", "Precipitation", "Runoff")
  data_1$Station <- rep("Dæmningen")
  data_2$Station <- rep("Damhusåen")
  data <- rbind(data_1, data_2)
  
  dy_r <- 0.25
  yl=c(ymin, ymax)
  yl_r=c(0, max(data$Precipitation, na.rm = T)) 
  scalingFactor=20000/yl_r[2]
  dummy_R=as.numeric(coredata(data$Precipitation)*scalingFactor)
  in_R  = data.frame(x1=as.character(data$Timestamp),
                     x2=c(as.character(data$Timestamp)[2:length(data$Timestamp)],as.character(data$Timestamp[length(data$Timestamp)]+60)),   
                     y1=rep(yl[2],length(data$Timestamp)),   
                     y2=(yl[2]-dummy_R))
  
  
  yTick=c(seq(yl[2]/scalingFactor,yl[2]/scalingFactor-yl_r[2],-dy_r))
  ylab=seq(0,(length(yTick)-1)*dy_r,dy_r)
  
  plot <- ggplot(data, aes(x = as.POSIXct(Timestamp, format="%Y-%m-%d %H:%M")))+
    geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
              fill='#6bdcff', colour = "#00c3ff",color=NA)+
    geom_rect(data=wwindex, inherit.aes = F, aes(xmin = as.POSIXct(x1), xmax = as.POSIXct(x2),
                                                   ymin = y1, ymax = y2),
              fill='#ff5252', colour = "#ff5252",color=NA, alpha = 0.3)+
    geom_point(aes(y = Runoff), color = "#009603", size = 0.8) + 
    geom_line(aes(y = Runoff), color = "#10C312")+
    scale_x_datetime(breaks = date_breaks("24 hours"), date_labels = "%Y-%m-%d %H:%M")+
    scale_y_continuous(limits = yl, expand = c(0, 0), breaks=seq(yl[1],yl[2],5000),
                       sec.axis = sec_axis(~./scalingFactor, name = "Rain [mm/hr]          ", breaks=yTick,labels =ylab))+
    facet_grid(cols = vars(Station))+
    ylab("Runoff [m3/hr]")+xlab("Time")+
    ggtitle(title)+
    theme_pubclean()+
    theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
          panel.grid.minor = element_blank(),
          panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"),
          legend.title = element_blank(),
          plot.title = element_text(size = 14), text = element_text(size=14),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(plot)
}




full_plot_wwindex <- plotTS_zoom_wwindex(data = data, wwindex = ww,from = from, to = to, ymin = ymin, ymax = ymax, title = "Dataset and wet weather index")





# ggsave(filename="../Figures/fullplotwwindex.pdf", width = 10, height =  4)
# full_plot_wwindex
# dev.off()










###### Updated zommed figure for publication



plotTS_zoom_wwindex <- function(data, from, wwindex, to, ymin, ymax, title){
  
  data <- window(data, start = from, end = to)
  data_df <- as.data.frame(data)
  timestamp <- as.POSIXct(rownames(data_df), format="%Y-%m-%d %H:%M")
  rownames(data_df) <- 1:nrow(data_df)
  data_1 <- data_df[,c(1,2)]
  data_1 <- cbind(timestamp, data_1)
  data_2 <- data_df[,c(1,3)]
  data_2 <- cbind(timestamp, data_2)
  colnames(data_1) <- c("Timestamp", "Precipitation", "Runoff")
  colnames(data_2) <- c("Timestamp", "Precipitation", "Runoff")
  data_1$Station <- rep("Dæmningen")
  data_2$Station <- rep("Damhusåen")
  data <- rbind(data_1, data_2)
  
  dy_r <- 0.25
  yl=c(ymin, ymax)
  yl_r=c(0, max(data$Precipitation, na.rm = T)) 
  scalingFactor=20000/yl_r[2]
  dummy_R=as.numeric(coredata(data$Precipitation)*scalingFactor)
  in_R  = data.frame(x1=as.character(data$Timestamp),
                     x2=c(as.character(data$Timestamp)[2:length(data$Timestamp)],as.character(data$Timestamp[length(data$Timestamp)]+60)),   
                     y1=rep(yl[2],length(data$Timestamp)),   
                     y2=(yl[2]-dummy_R))
  
  
  yTick=c(seq(yl[2]/scalingFactor,yl[2]/scalingFactor-yl_r[2],-dy_r))
  ylab=seq(0,(length(yTick)-1)*dy_r,dy_r)
  
  plot <- ggplot(data, aes(x = as.POSIXct(Timestamp, format="%Y-%m-%d %H:%M")))+
    geom_rect(data=in_R, inherit.aes = F,aes(xmin=as.POSIXct(x1), xmax=as.POSIXct(x2), ymin=y1, ymax=y2), 
              fill='#6bdcff', colour = "#787878",color=NA)+
    geom_rect(data=wwindex, inherit.aes = F, aes(xmin = as.POSIXct(x1), xmax = as.POSIXct(x2),
                                                 ymin = y1, ymax = y2),
              fill='#808080', colour = "#808080",color=NA, alpha = 0.5)+
    geom_point(aes(y = Runoff), color = "#505050", size = 0.2) + 
    geom_line(aes(y = Runoff), color = "#505050")+
    scale_x_datetime(breaks = date_breaks("24 hours"), date_labels = "%Y-%m-%d %H:%M")+
    scale_y_continuous(limits = yl, expand = c(0, 0), breaks=seq(yl[1],yl[2],5000),
                       sec.axis = sec_axis(~./scalingFactor, name = "Rain [mm/hr]          ", breaks=yTick,labels =ylab))+
    facet_grid(cols = vars(Station))+
    ylab("Runoff [m3/hr]")+xlab("Time")+
    ggtitle(title)+
    theme_pubclean()+
    theme(panel.grid.major = element_line(size=.20,colour = "grey50"),
          panel.grid.minor = element_blank(),
          panel.ontop = FALSE,panel.background = element_rect(fill = NA,size = 0.2, linetype = "solid",colour = "black"),
          legend.title = element_blank(),
          plot.title = element_text(size = 14), text = element_text(size=14),
          axis.title = element_text(size = 10), axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(plot)
}



full_plot_wwindex_bw <- plotTS_zoom_wwindex(data = data, wwindex = ww,from = from, to = to, ymin = ymin, ymax = ymax, title = "Flow measurments, precipitation, and wet-weather index")
full_plot_wwindex_bw




ggsave(filename="../Figures/data_and_wwindex_bw.pdf", width = 10, height =  4)
full_plot_wwindex_bw
dev.off()
 














library(shiny)
library(tidyverse)
library(dplyr)
library(xts)
library(dygraphs)
library(plotly)
library(ggplot2)


addDyshades <- function(dg, flatlineDays){
  for( i in 1:nrow(flatlineDays)) {
    dg <- dyShading(dg, from = flatlineDays[i, 1] , to = flatlineDays[i, 2], color = "#FFAAAA")
  }
  return(dg)
}

#x <- setwd(paste(getwd(), "/Documents/Skóli/DTU/Thesis/Data", sep=""));

######################### Read data
d <- read.csv("../Data/Data handling/DST/DST_P.txt", header = TRUE, sep = "\t")
s1 <- read.csv("../Data/Data handling/DST/DST_Station1.txt", header = TRUE, sep = "\t")
s2 <- read.csv("../Data/Data handling/DST/DST_Station2.txt", header = TRUE, sep = "\t")

flatlineDaysS1 <- read.csv("../Data/Data handling/FlatlineDays/FlatlineDays_S1.txt", header = TRUE, sep = "\t")
flatlineDaysS2 <- read.csv("../Data/Data handling/FlatlineDays/FlatlineDays_S2.txt", header = TRUE, sep = "\t")


precipitationvsRunoff <- read.csv("../Data/PrecipitationvsRunoff.txt", header = TRUE, sep = "\t")

p1 <- ggplot(data = precipitationvsRunoff, aes(x=Precipitation, y=Runoff, name=Timestamp, color = Station)) +
  geom_point(size = 0.5)+
  theme_bw() +
  labs(x = "Precipitation", 
       y = "Runoff at stations",
       title = "Relationship between precipitation and runoff")

######################### xts
timestamp <- as.POSIXct(d$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
d_xts <- xts(x = as.numeric(d$Value), order.by = timestamp)

timestamp <- as.POSIXct(s1$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s1_xts <- xts(x = as.numeric(s1$Value), order.by = timestamp)

timestamp <- as.POSIXct(s2$Timestamp, format="%Y/%m/%d %H:%M", tz = "GMT")
s2_xts <- xts(x = as.numeric(s2$Value), order.by = timestamp)

stations <- cbind(s1_xts, s2_xts)
colnames(stations) <- c("Station 1", "Station 2")
colnames(d_xts) <- "Precipitation"











# Define UI for random distribution app ----
ui <- fluidPage(
  navbarPage("Comparions of precipitation and runoff", 
    tabPanel("Plot",
      sidebarLayout(      
          sidebarPanel(
            selectInput("Stations", "Stations:",
                        choices=c("Station 1", "Station 2", "Both Stations"),
                        selected = "Both Stations"),
            hr(),
            helpText("Select for station to display.")
            ),
            mainPanel(
              fluidRow(
                column(12, dygraphOutput("dygraphStations")),
                column(12, dygraphOutput("dygraphPrecipitation")),
              )
            ),),),
      navbarMenu("Comparisons", tabPanel("Precipitation vs Runoff", mainPanel(plotlyOutput("comparison1")))),
      navbarMenu("Correlation", tabPanel("ACF"), tabPanel("PACF")),
    tabPanel("Help")
  )
)






# Define server logic for random distribution app ----
server <- function(input, output) {

              output$dygraphStations <- renderDygraph({
                if (input$Stations == "Station 1"){
                  data = s1_xts
                }
                else if (input$Stations == "Station 2"){
                  data = s2_xts
                }
                else{
                  data = stations
                }
                
                  dg_s1 <- dygraph(data = data, main = "Stations", group = "main") %>%
                              dyAxis("y", label = "Runoff volume [m3/h]") %>%
                              dyOptions(drawGrid = TRUE) %>%
                              dyRangeSelector(retainDateWindow = TRUE) %>%
                              dyCrosshair(direction = "vertical") %>%
                              dyOptions(useDataTimezone = TRUE) %>%
                              dyRoller(rollPeriod = 1)
                  dg_s1 <- addDyshades(dg_s1, flatlineDaysS1)
                  dg_s1 <- addDyshades(dg_s1, flatlineDaysS2)
                  dg_s1
                  
                  })


              output$dygraphPrecipitation <- renderDygraph({
                
                dygraph(d_xts, main="Precipitation", group = "main") %>%
                  dyAxis("y", label = "Precipitation []") %>%
                  dyOptions(drawGrid = TRUE) %>%
                  dyOptions(useDataTimezone = TRUE) %>%
                  dyCrosshair(direction = "vertical") %>%
                  dyRoller(rollPeriod = 1) %>%
                  dyBarChart()
              })
              
              output$comparison1 <- renderPlotly(
                
                ggplotly(p1)
                
              )
}

# Create Shiny app ----
shinyApp(ui, server)





library(tidyverse)
library(ggplot2)
#### HERE WE GET THE TOP PERFORMING MODELS (NO MATTER THE MODEL TYPE, I.E. SINGLE/MULTI-STEP ARIMA/ARIMAX)
#### THE BEST MODELS ARE SAVED AS DATA FRAME AND CAN BE PRINTED OUT TO LATEX TABLES

## Singlestep
files <- list.files(path = "Evaluation/DDS/Evaluation_singlestep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S1 <- lapply(paste("Evaluation/DDS/Evaluation_singlestep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_singlestep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ss_S2 <- lapply(paste("Evaluation/DDS/Evaluation_singlestep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_singlestep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S1 <- lapply(paste("Evaluation/DDS/Evaluation_singlestep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_singlestep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ss_S2 <- lapply(paste("Evaluation/DDS/Evaluation_singlestep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

## Multistep
files <- list.files(path = "Evaluation/DDS/Evaluation_multistep/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S1 <- lapply(paste("Evaluation/DDS/Evaluation_multistep/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_multistep/ARIMA/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_ms_S2 <- lapply(paste("Evaluation/DDS/Evaluation_multistep/ARIMA/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_multistep/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S1 <- lapply(paste("Evaluation/DDS/Evaluation_multistep/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)

files <- list.files(path = "Evaluation/DDS/Evaluation_multistep/ARIMAX/S2/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_ms_S2 <- lapply(paste("Evaluation/DDS/Evaluation_multistep/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)



labelList <- function(L, ofc_, model_){
  for (i in (1:length(L))){
    L[[i]]$ofc <- ofc_
    L[[i]]$model <- model_
  }
  return(L)
}

## take together all models for station
ARIMA_ss_S1 <- labelList(ARIMA_ss_S1, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S1 <- labelList(ARIMAX_ss_S1, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S1 <- labelList(ARIMA_ms_S1, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S1 <- labelList(ARIMAX_ms_S1, ofc_ = "multi-step", model_ = "ARIMAX")
DamningenList_singlestep <- c(ARIMA_ss_S1, ARIMAX_ss_S1)
DamningenList_multistep <- c(ARIMA_ms_S1, ARIMAX_ms_S1)


ARIMA_ss_S2 <- labelList(ARIMA_ss_S2, ofc_ = "single-step", model_ = "ARIMA")
ARIMAX_ss_S2 <- labelList(ARIMAX_ss_S2, ofc_ = "single-step", model_ = "ARIMAX")
ARIMA_ms_S2 <- labelList(ARIMA_ms_S2, ofc_ = "multi-step", model_ = "ARIMA")
ARIMAX_ms_S2 <- labelList(ARIMAX_ms_S2, ofc_ = "multi-step", model_ = "ARIMAX")
DamhusaenList_singlestep <- c(ARIMA_ss_S2, ARIMAX_ss_S2)
DamhusaenList_multistep <- c(ARIMA_ms_S2, ARIMAX_ms_S2)






pullData <- function(score){
  orderListPI <- function(x, fh){
    values <- vector()
    for (i in (1:length(x))){
      temp <- unlist(x[[i]]$PI[fh])
      if (is.null(temp) || (is.nan(temp))){
        values[i] <- NA
      }
      else{
        values[i] <- as.numeric(temp) 
      }
    }
    orderedList <- x[order(values, decreasing = TRUE)]
    return(orderedList)
  }
  orderListAccuracy <- function(x, fh){
    values <- vector()
    for (i in (1:length(x))){
      temp <- unlist(x[[i]]$accuracy[[fh]]$accuracy_correct)
      if (is.null(temp) || (is.nan(temp))){
        values[i] <- NA
      }
      else{
        values[i] <- as.numeric(temp) 
      }
    }
    orderedList <- x[order(values, decreasing = TRUE)]
    return(orderedList)
  }
  
  if (score == "PI"){
    ### Best models based on PI ordered by differetn forecasting horizon
    DamningenList_singlestep_30 <- orderListPI(DamningenList_singlestep, fh = 1)
    DamningenList_singlestep_60 <- orderListPI(DamningenList_singlestep, fh = 2)
    DamningenList_singlestep_90 <- orderListPI(DamningenList_singlestep, fh = 3)
    
    DamningenList_multistep_30 <- orderListPI(DamningenList_multistep, fh = 1)
    DamningenList_multistep_60 <- orderListPI(DamningenList_multistep, fh = 2)
    DamningenList_multistep_90 <- orderListPI(DamningenList_multistep, fh = 3)
    
    DamhusaenList_singlestep_30 <- orderListPI(DamhusaenList_singlestep, fh = 1)
    DamhusaenList_singlestep_60 <- orderListPI(DamhusaenList_singlestep, fh = 2)
    DamhusaenList_singlestep_90 <- orderListPI(DamhusaenList_singlestep, fh = 3)
    
    DamhusaenList_multistep_30 <- orderListPI(DamhusaenList_multistep, fh = 1)
    DamhusaenList_multistep_60 <- orderListPI(DamhusaenList_multistep, fh = 2)
    DamhusaenList_multistep_90 <- orderListPI(DamhusaenList_multistep, fh = 3)
    
  
    #### Construct table
    ## singlestep
    table_singlestep <- as.data.frame(matrix(NA, nrow = 2, ncol = 3))
    rownames(table_singlestep) <- c("Dæmningen", "Damhusåen")
    colnames(table_singlestep) <- c("30 min", "60 min", "90 min")
    
    table_singlestep[1,1] <- DamningenList_singlestep_30[[1]]$PI$PI30
    table_singlestep[1,2] <- DamningenList_singlestep_60[[1]]$PI$PI60
    table_singlestep[1,3] <- DamningenList_singlestep_90[[1]]$PI$PI90
    
    table_singlestep[2,1] <- DamhusaenList_singlestep_30[[1]]$PI$PI30
    table_singlestep[2,2] <- DamhusaenList_singlestep_60[[1]]$PI$PI60
    table_singlestep[2,3] <- DamhusaenList_singlestep_90[[1]]$PI$PI90
    
    ## multistep
    table_multistep <- as.data.frame(matrix(NA, nrow = 2, ncol = 3))
    rownames(table_multistep) <- c("Dæmningen", "Damhusåen")
    colnames(table_multistep) <- c("30 min", "60 min", "90 min")
    
    table_multistep[1,1] <- DamningenList_multistep_30[[1]]$PI$PI30
    table_multistep[1,2] <- DamningenList_multistep_60[[1]]$PI$PI60
    table_multistep[1,3] <- DamningenList_multistep_90[[1]]$PI$PI90
    
    table_multistep[2,1] <- DamhusaenList_multistep_30[[1]]$PI$PI30
    table_multistep[2,2] <- DamhusaenList_multistep_60[[1]]$PI$PI60
    table_multistep[2,3] <- DamhusaenList_multistep_90[[1]]$PI$PI90

    dt_singlestep <- table_singlestep %>%
      rownames_to_column() %>%
      gather(colname, value, -rowname)
    
    dt_multistep <- table_multistep %>%
      rownames_to_column() %>%
      gather(colname, value, -rowname)
    
    dt_singlestep$Type <- "Single-step\n(SSE)"
    dt_multistep$Type <- "Multi-step\n(SC)"
    
    dt <- rbind(dt_singlestep, dt_multistep)
    
    dt$rowname <- factor(dt$rowname, levels = c("Dæmningen", "Damhusåen"))
  }
  else if (score == "Accuracy"){
    ### Best models based on PI ordered by differetn forecasting horizon
    DamningenList_singlestep_30 <- orderListAccuracy(DamningenList_singlestep, fh = 1)
    DamningenList_singlestep_60 <- orderListAccuracy(DamningenList_singlestep, fh = 2)
    DamningenList_singlestep_90 <- orderListAccuracy(DamningenList_singlestep, fh = 3)
    
    DamningenList_multistep_30 <- orderListAccuracy(DamningenList_multistep, fh = 1)
    DamningenList_multistep_60 <- orderListAccuracy(DamningenList_multistep, fh = 2)
    DamningenList_multistep_90 <- orderListAccuracy(DamningenList_multistep, fh = 3)
    
    DamhusaenList_singlestep_30 <- orderListAccuracy(DamhusaenList_singlestep, fh = 1)
    DamhusaenList_singlestep_60 <- orderListAccuracy(DamhusaenList_singlestep, fh = 2)
    DamhusaenList_singlestep_90 <- orderListAccuracy(DamhusaenList_singlestep, fh = 3)
    
    DamhusaenList_multistep_30 <- orderListAccuracy(DamhusaenList_multistep, fh = 1)
    DamhusaenList_multistep_60 <- orderListAccuracy(DamhusaenList_multistep, fh = 2)
    DamhusaenList_multistep_90 <- orderListAccuracy(DamhusaenList_multistep, fh = 3)
    
    
    #### Construct table
    ## singlestep
    table_singlestep <- as.data.frame(matrix(NA, nrow = 2, ncol = 3))
    rownames(table_singlestep) <- c("Dæmningen", "Damhusåen")
    colnames(table_singlestep) <- c("30 min", "60 min", "90 min")
    
    table_singlestep[1,1] <- DamningenList_singlestep_30[[1]]$accuracy$accuracy30$accuracy_correct
    table_singlestep[1,2] <- DamningenList_singlestep_60[[1]]$accuracy$accuracy60$accuracy_correct
    table_singlestep[1,3] <- DamningenList_singlestep_90[[1]]$accuracy$accuracy90$accuracy_correct
    
    table_singlestep[2,1] <- DamhusaenList_singlestep_30[[1]]$accuracy$accuracy30$accuracy_correct
    table_singlestep[2,2] <- DamhusaenList_singlestep_60[[1]]$accuracy$accuracy60$accuracy_correct
    table_singlestep[2,3] <- DamhusaenList_singlestep_90[[1]]$accuracy$accuracy90$accuracy_correct
    
    ## multistep
    table_multistep <- as.data.frame(matrix(NA, nrow = 2, ncol = 3))
    rownames(table_multistep) <- c("Dæmningen", "Damhusåen")
    colnames(table_multistep) <- c("30 min", "60 min", "90 min")
    
    table_multistep[1,1] <- DamningenList_multistep_30[[1]]$accuracy$accuracy30$accuracy_correct
    table_multistep[1,2] <- DamningenList_multistep_60[[1]]$accuracy$accuracy60$accuracy_correct
    table_multistep[1,3] <- DamningenList_multistep_90[[1]]$accuracy$accuracy90$accuracy_correct
    
    table_multistep[2,1] <- DamhusaenList_multistep_30[[1]]$accuracy$accuracy30$accuracy_correct
    table_multistep[2,2] <- DamhusaenList_multistep_60[[1]]$accuracy$accuracy60$accuracy_correct
    table_multistep[2,3] <- DamhusaenList_multistep_90[[1]]$accuracy$accuracy90$accuracy_correct
    
    dt_singlestep <- table_singlestep %>%
      rownames_to_column() %>%
      gather(colname, value, -rowname)
    
    dt_multistep <- table_multistep %>%
      rownames_to_column() %>%
      gather(colname, value, -rowname)
    
    dt_singlestep$Type <- "Single-step\n(SSE)"
    dt_multistep$Type <- "Multi-step\n(SC)"
    
    dt <- rbind(dt_singlestep, dt_multistep)
    
    dt$rowname <- factor(dt$rowname, levels = c("Dæmningen", "Damhusåen"))
    
  }
  else{
    print("Provide error: PI or Accuracy")
  }
  return(dt)
}


plotThis <- function(df, error){
  if (error == "PI"){
    plot <- ggplot(df, aes(x = colname, y = Type, fill =  value))+
      geom_tile()+
      facet_grid(cols = vars(rowname))+
      geom_text(aes(label = round(value, 2)))+
      scale_fill_gradient(low = "white",high = "#808080", na.value = "red")+
      scale_x_discrete(expand=c(0,0)) + 
      scale_y_discrete(expand=c(0,0)) +
      labs(tag = "A")+
      theme_bw()+
      theme(plot.margin = unit(c(0,0,0,0), "cm"),
            plot.title = element_text(size = 9), 
            plot.tag = element_text(size = 9),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.spacing = unit(0, "lines"),
            legend.position = "bottom")+
      ylab("") + xlab("Forecasting horizon") + labs(fill = "PI")
  } else if (error == "Accuracy"){
    plot <- ggplot(df, aes(x = colname, y = Type, fill =  value))+
      geom_tile()+
      facet_grid(cols = vars(rowname))+
      geom_text(aes(label = round(value, 2)))+
      scale_fill_gradient(low = "white",high = "#808080", na.value = "red")+
      scale_x_discrete(expand=c(0,0)) + 
      scale_y_discrete(expand=c(0,0)) +
      labs(tag = "B")+
      theme_bw()+
      theme(plot.title = element_text(size = 9), 
            plot.tag = element_text(size = 9),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.spacing = unit(0, "lines"),
            legend.position = "bottom")+
      ylab("") + xlab("Forecasting horizon") + labs(fill = "CSI")
  } else {plot <- NULL}
  
  return(plot)
}


df_PI <- pullData("PI")
plot_PI <- plotThis(df_PI, error = "PI")

df_Accuracy <- pullData("Accuracy")
plot_Accuracy <- plotThis(df_Accuracy, error = "Accuracy")

plot <- annotate_figure(ggarrange(plot_PI, plot_Accuracy, nrow = 1, ncol = 2), 
                        top = text_grob("Comparing objective function criteria", size = 10))
plot

tiff(file = "../Figures/Results/DDS/Heatmap_ofc.tiff", height = 70, width = 190, unit = "mm", res = 300)
plot
dev.off()
  





















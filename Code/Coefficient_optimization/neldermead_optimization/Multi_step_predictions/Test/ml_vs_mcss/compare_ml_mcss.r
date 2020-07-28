args <- commandArgs(trailingOnly = F)
myargument <- args[length(args)]
myargument <- sub("-","",myargument)
myargument <- as.numeric(myargument)

setwd("/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code")
wd <- getwd()
source(paste(wd,"/Coefficient_optimization/Multi_step_predictions/meta_optim_functions/meta_optim_mcss.r", sep = ""))
source(paste(wd,"/Coefficient_optimization/Multi_step_predictions/meta_optim_functions/meta_optim_ml.r", sep = ""))
load("Coefficient_optimization/Multi_step_predictions/Job_splitting/ARIMA/orders.RData")

##################### Reading in data ##################### 
s1_training <-  read.csv("../Data/Training_data/s1_training.txt", header = TRUE, sep = "\t")
ts <- s1_training$Value
observed <- s1_training$Value
s1_wwIndex <- read.csv("../Data/Training_data/s1_WW_training.txt", header = TRUE, sep = "\t")
wwIndex <- s1_wwIndex$Flag

order <- as.vector(orders[myargument,], mode = "numeric")
print(order)

##################### Optimization for Dammning (station 1) ##################### 
start_time <- Sys.time()
results_mcss <- meta_optim_mcss(order, External_Regressor = FALSE)
end_time <- Sys.time()
results_mcss$Time <- end_time -  start_time  ## We want to see time as well in the list

start_time <- Sys.time()
results_ml <- meta_optim_ml(order, External_Regressor = FALSE)
end_time <- Sys.time()
results_ml$Time <- end_time -  start_time  ## We want to see time as well in the list


name <- paste("CO_ARIMA_s1_ml_c(", order[1], ", ", order[2], ", ", order[3], ")", sep = "")
assign(name, results_ml)
filename <- paste("Coefficient_optimization/Multi_step_predictions/ml_vs_mcss/Lists/ml/", name, ".rdata",sep = "")
save(results_ml, file = filename)

name <- paste("CO_ARIMA_s1_mcss_c(", order[1], ", ", order[2], ", ", order[3], ")", sep = "")
assign(name, results_mcss)
filename <- paste("Coefficient_optimization/Multi_step_predictions/ml_vs_mcss/Lists/mcss/", name, ".rdata",sep = "")
save(results_mcss, file = filename)



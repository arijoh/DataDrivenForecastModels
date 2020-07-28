
myargument <- 3

setwd("/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code")
wd <- getwd()
source(paste(wd,"/Coefficient_optimization/Multi_step_predictions/meta_optim_functions/meta_optim.r", sep = ""))
source(paste(wd,"/Coefficient_optimization/Multi_step_predictions/meta_optim_functions/evaluationPart_of_meta_optim.r", sep = ""))
load("Coefficient_optimization/Multi_step_predictions/Job_splitting/ARIMA/orders.RData")

##################### Reading in training data ##################### 
ts <-  read.csv("../Data/Training_data/s2_training.txt", header = TRUE, sep = "\t")$Value
wwIndex <- read.csv("../Data/Training_data/s2_WW_training.txt", header = TRUE, sep = "\t")$Flag
d_validation <- read.csv("../Data/Validation_data/d_validation.txt", header = TRUE, sep = "\t")$Value
ts_validation <-  read.csv("../Data/Validation_data/s2_validation.txt", header = TRUE, sep = "\t")$Value
timestamp_validation <- read.csv("../Data/Validation_data/s2_validation.txt", header = TRUE, sep = "\t")$Timestamp

order <- as.vector(orders[myargument,], mode = "numeric")
print(order)

orders <- order
External_Regressor = FALSE

## Constants used to de-normalize the data
norm_const_d <-  23.8588
norm_const_station <- 26776.54 ## For station 2 I assume??




##################### Optimization for Dammning (station 1) ##################### 
start_time <- Sys.time()
results <- meta_optim(order, External_Regressor = FALSE)
end_time <- Sys.time()
results$Time <- end_time -  start_time  ## We want to see time as well in the list




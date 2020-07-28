args <- commandArgs(trailingOnly = F)
myargument <- args[length(args)]
myargument <- sub("-","",myargument)
myargument <- as.numeric(myargument)

setwd("/zhome/6e/9/133731/Desktop/UpdateMS/Code")
source(paste(getwd(),"/Coefficient_optimization/neldermead_optimization/Multi_step_predictions/meta_optim_functions/meta_optim.r", sep = ""))
load("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/orders.RData")

##################### Reading in data ##################### 
ts <-  read.csv("../Data/Training_data/s1_training.txt", header = TRUE, sep = "\t")$Value
wwIndex <- read.csv("../Data/Training_data/s1_WW_training.txt", header = TRUE, sep = "\t")$Flag
#d_validation <- read.csv("../Data/Validation_data/d_validation.txt", header = TRUE, sep = "\t")$Value
#ts_validation <-  read.csv("../Data/Validation_data/s1_validation.txt", header = TRUE, sep = "\t")$Value
#timestamp_validation <- read.csv("../Data/Validation_data/s1_validation.txt", header = TRUE, sep = "\t")$Timestamp
#norm_const_station <- 26776.54 ## For station 2
#norm_const_station <- 9866.368 ## For station1

order <- as.vector(orders[myargument,], mode = "numeric")
print(order)

##################### Optimization for Dammning (station 2 ##################### 
start_time <- Sys.time()
results <- meta_optim(order, External_Regressor = FALSE)
end_time <- Sys.time()
results$Time <- end_time -  start_time  ## We want to see time as well in the list

name <- paste("CO_ARIMA_s1_c(", order[1], ", ", order[2], ", ", order[3], ")", sep = "")
assign(name, results)
filename <- paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMA/S1/Lists/", name, ".rdata",sep = "")
save(results, file = filename)





args <- commandArgs(trailingOnly = F)
myargument <- args[length(args)]
myargument <- sub("-","",myargument)
myargument <- as.numeric(myargument)

setwd("/zhome/6e/9/133731/Desktop/UpdateMS/Code")
source(paste(getwd(),"/Coefficient_optimization/neldermead_optimization/Multi_step_predictions/meta_optim_functions/meta_optim.r", sep = ""))
load("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/orders.RData")

##################### Reading in data ##################### 
s1_training <-  read.csv("../Data/Training_data/s1_training.txt", header = TRUE, sep = "\t")
s1 <- s1_training$Value
s1_wwIndex <- read.csv("../Data/Training_data/s1_WW_training.txt", header = TRUE, sep = "\t")
wwIndex <- s1_wwIndex$Flag
Regressor <-  read.csv("../Data/Training_data/d_training.txt", header = TRUE, sep = "\t")
Regressor <- Regressor$Value
ts <- s1
observed <- s1

order <- as.vector(orders[myargument,1:3], mode = "numeric")
reg.lag <- as.numeric(orders[myargument,4])
reg.nr <-  as.numeric(orders[myargument,5])

print(order)
print(reg.nr)
print(reg.lag)
orders <- order

##################### Optimization for Dammning (station 1) ##################### 
start_time <- Sys.time()
results <- meta_optim(order, External_Regressor = TRUE, reg.nr = reg.nr, reg.lag = reg.lag)
end_time <- Sys.time()
results$reg.nr <-  reg.nr
results$reg.lag <- reg.lag
results$Time <- end_time -  start_time  ## We want to see time as well in the list


name <- paste("CO_ARIMAX_s1_c(", order[1], ", ", order[2], ", ", order[3], "reg.nr=", reg.nr, "reg.lag=", reg.lag,  ")", sep = "")
assign(name, results)
filename <- paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S1/Lists/", name, ".rdata",sep = "")
save(results, file = filename)


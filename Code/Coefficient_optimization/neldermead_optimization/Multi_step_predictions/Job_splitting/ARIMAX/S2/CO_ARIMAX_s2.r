args <- commandArgs(trailingOnly = F)
myargument <- args[length(args)]
myargument <- sub("-","",myargument)
myargument <- as.numeric(myargument)

setwd("/zhome/6e/9/133731/Desktop/UpdateMS/Code")
source(paste(getwd(),"/Coefficient_optimization/neldermead_optimization/Multi_step_predictions/meta_optim_functions/meta_optim.r", sep = ""))
load("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/orders.RData")

##################### Reading in data ##################### 
s2_training <-  read.csv("../Data/Training_data/s2_training.txt", header = TRUE, sep = "\t")
s2 <- s2_training$Value
s2_wwIndex <- read.csv("../Data/Training_data/s2_WW_training.txt", header = TRUE, sep = "\t")
wwIndex <- s2_wwIndex$Flag
Regressor <-  read.csv("../Data/Training_data/d_training.txt", header = TRUE, sep = "\t")
Regressor <- Regressor$Value
ts <- s2
observed <- s2

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



name <- paste("CO_ARIMAX_s2_c(", order[1], ", ", order[2], ", ", order[3], "reg.nr=", reg.nr, "reg.lag=", reg.lag, ")", sep = "")
assign(name, results)
filename <- paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S2/Lists/", name, ".rdata",sep = "")
save(results, file = filename)





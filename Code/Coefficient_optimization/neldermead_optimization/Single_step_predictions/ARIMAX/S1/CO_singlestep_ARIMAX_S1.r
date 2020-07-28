args <- commandArgs(trailingOnly = F)
myargument <- args[length(args)]
myargument <- sub("-","",myargument)
myargument <- as.numeric(myargument)
setwd("/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code")
source(paste(getwd(), "/Coefficient_optimization/neldermead_optimization/Single_step_predictions/meta_optim.r", sep = ""))
load("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/orders.rdata")
d_training <-  read.csv("../Data/Training_data/d_training.txt", header = TRUE, sep = "\t")$Value
s1_training <-  read.csv("../Data/Training_data/s1_training.txt", header = TRUE, sep = "\t")$Value
s1_wwIndex <- read.csv("../Data/Training_data/s1_WW_training.txt", header = TRUE, sep = "\t")$Flag

order <- as.vector(orders[myargument,1:3], mode = "numeric")
reg.lag <- as.numeric(orders[myargument,4])
reg.nr <-  as.numeric(orders[myargument,5])

print(order)
print(reg.nr)
print(reg.lag)

ts <- s1_training
Regressor <- d_training
wwIndex <- s1_wwIndex

start_time <- Sys.time()
results <- meta_optim(order, External_Regressor = TRUE, reg.nr = reg.nr, reg.lag = reg.lag)
end_time <- Sys.time()
results$reg.nr <-  reg.nr
results$reg.lag <- reg.lag
results$Time <- end_time -  start_time  

name <- paste("CO_ARIMAX_S1_c(", order[1], ", ", order[2], ", ", order[3], ", reg.nr=", reg.nr, ", reg.lag=", reg.lag,  ")", sep = "")
assign(name, results)
filename <- paste("Coefficient_optimization/neldermead_optimization/Single_step_predictions/ARIMAX/S1/Lists/", name, ".rdata",sep = "")
save(results, file = filename)



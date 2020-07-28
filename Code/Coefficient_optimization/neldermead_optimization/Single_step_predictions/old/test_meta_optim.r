

#Singlestep
source(paste(getwd(), "/Coefficient_optimization/Single_step_predictions/HPC/meta_optim.r", sep = ""))

#Multistep
source(paste(getwd(),"/Coefficient_optimization/Multi_step_predictions/meta_optim_functions/meta_optim.r", sep = ""))


d_training <-  read.csv("../Data/Training_data/d_training.txt", header = TRUE, sep = "\t")$Value
s1_training <-  read.csv("../Data/Training_data/s1_training.txt", header = TRUE, sep = "\t")$Value
s1_wwIndex <- read.csv("../Data/Training_data/s1_WW_training.txt", header = TRUE, sep = "\t")$Flag

ts <- s1_training
Regressor <- d_training
wwIndex <- s1_wwIndex

External_Regressor <- TRUE
order <- c(1,0,1)
reg.nr <- 5
reg.lag <- 2


meta_optim(order = order, External_Regressor = External_Regressor, reg.nr = reg.nr, reg.lag = reg.lag)





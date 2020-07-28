


cores <- as.numeric(Sys.getenv('LSB_DJOB_NUMPROC'))
setwd("/zhome/6e/9/133731/Desktop/Thesis/Thesis")
#cores = parallel::detectCores()
source("Code/Coefficient optimization/Multi step predictions/HPC/meta_optim.R")


##################### Reading in data ##################### 
s2_training <-  read.csv("Data/Training data/s2_training.txt", header = TRUE, sep = "\t")
s2 <- s2_training$Value
s2_wwIndex <- read.csv("Data/Training data/s2_WW_training.txt", header = TRUE, sep = "\t")


##################### Parameters ##################### 
MAX <- 8
x <- expand.grid(0:MAX, 0:2, 0:MAX)
colnames(x) <- c("p", "d", "q")


##################### Optimization for Damhusaen (station 2) ##################### 

ts <- s2
observed <- s2
wwIndex <- s2_wwIndex$Flag





system.time(
  CO_s2 <- parallel::mclapply(1:nrow(x), function(i){
    temp <- as.matrix(x[i,])
    temp <- c(temp[1], temp[2], temp[3])
    start_time <- Sys.time()
    results <- meta_optim(temp, external_regressor = FALSE)
    end_time <- Sys.time()
    time_taken <- end_time - start_time
    print(paste("Order: ", "(", temp[1], ", ", temp[2], ", ", temp[3], ")", " takes ", signif(time_taken, 4), " seconds to optimize", sep = ""))
  }, mc.cores = cores, mc.allow.recursive = TRUE, mc.preschedule = FALSE)
)

save(CO_s2, file = "Code/Coefficient optimization/Multi step predictions/HPC/CO_multistep_arima_s2.RData")






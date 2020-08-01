args <- commandArgs(trailingOnly = F)
myargument <- args[length(args)]
myargument <- sub("-","",myargument)
myargument <- as.numeric(myargument)

library(xts)
Sys.setenv(TZ = "GMT")
setwd("/zhome/6e/9/133731/Desktop/UpdateMS/Code")
wd <- getwd()
source(paste(wd,"/Evaluation/functions.r", sep = ""))
files <- list.files(path = "Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMAX_S1 <- lapply(paste("Coefficient_optimization/neldermead_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)
ts <-  read.csv("../Data/Validation_data/s1_validation.txt", header = TRUE, sep = "\t")$Value
time <- as.POSIXct(read.csv("../Data/Validation_data/s1_validation.txt", header = TRUE, sep = "\t")$Timestamp, tz = "GMT")
ts <- as.xts(ts, order.by = time)
Regressor <- read.csv("../Data/Validation_data/d_validation.txt", header = TRUE, sep = "\t")$Value
Regressor <- as.xts(Regressor, order.by = time)
## Change wwIndex to FALSE/TRUE
wwIndex <- read.csv("../Data/Validation_data/s1_WW_validation.txt", header = TRUE, sep = "\t")$Flag
wwIndex <- as.logical((wwIndex))
wwIndex <- as.xts(wwIndex, order.by = time)
External_Regressor = TRUE
#norm_const_station <- 26776.54 ## For station 2
norm_const_station <- 9866.368 ## For station1

X <- ARIMAX_S1[[myargument]]
results <- evaluateLists(X)



name <- paste("eval_ARIMAX_S1_c(", X$order[1], ", ", X$order[2], ", ", X$order[3], ", ", "reg.nr=", X$reg.nr, ", ", "reg.lag=", X$reg.lag,  ")", sep = "")
assign(name, results)
filename <- paste("Evaluation/Evaluation_multistep/ARIMAX/S1/Lists/", name, ".rdata",sep = "")
save(results, file = filename)







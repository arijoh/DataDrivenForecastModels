
args <- commandArgs(trailingOnly = F)
myargument <- args[length(args)]
myargument <- sub("-","",myargument)
myargument <- as.numeric(myargument)

library(xts)
Sys.setenv(TZ = "GMT")
setwd("/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code")
wd <- getwd()
source(paste(wd,"/Evaluation/functions.r", sep = ""))
files <- list.files(path = "Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S1/Lists", pattern = ".rdata", recursive = TRUE)
ARIMA_S1 <- lapply(paste("Coefficient_optimization/dds_optimization/Single_step_predictions/ARIMA/S1/Lists/",files, sep = ""), function(x) mget(load(x))$results)
ts <-  read.csv("../Data/Validation_data/s1_validation.txt", header = TRUE, sep = "\t")$Value
time <- as.POSIXct(read.csv("../Data/Validation_data/s1_validation.txt", header = TRUE, sep = "\t")$Timestamp, tz = "GMT")
ts <- as.xts(ts, order.by = time)
## Change wwIndex to FALSE/TRUE
wwIndex <- read.csv("../Data/Validation_data/s1_WW_validation.txt", header = TRUE, sep = "\t")$Flag
wwIndex <- as.logical((wwIndex))
External_Regressor = FALSE
#norm_const_station <- 26776.54 ## For station 2
norm_const_station <- 9866.368 ## For station1
wwIndex <- as.xts(wwIndex, order.by = time)


X <- ARIMA_S1[[myargument]]
results <- evaluateLists(X)


name <- paste("eval_ARIMA_ss_S1_c(", X$order[1], ", ", X$order[2], ", ", X$order[3], ", ", "reg.nr=", X$reg.nr, "reg.lag=", X$reg.lag,  ")", sep = "")
assign(name, results)
filename <- paste("Evaluation/DDS/Evaluation_singlestep/ARIMA/S1/Lists/", name, ".rdata",sep = "")
save(results, file = filename)


files <- list.files(path = "Coefficient_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S2/Lists/", pattern = ".rdata", recursive = TRUE)
L <- lapply(paste("Coefficient_optimization/Multi_step_predictions/Job_splitting/ARIMAX/S2/Lists/",files, sep = ""), function(x) mget(load(x))$results)





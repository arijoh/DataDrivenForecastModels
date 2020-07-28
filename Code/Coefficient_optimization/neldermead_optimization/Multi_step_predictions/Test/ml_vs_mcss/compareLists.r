
files <- list.files(path = "Coefficient_optimization/Multi_step_predictions/ml_vs_mcss/Lists/mcss", pattern = ".rdata", recursive = TRUE)
L_mcss <- lapply(paste("Coefficient_optimization/Multi_step_predictions//ml_vs_mcss/Lists/mcss/",files, sep = ""), function(x) mget(load(x))$results)




files <- list.files(path = "Coefficient_optimization/Multi_step_predictions/ml_vs_mcss/Lists/ml", pattern = ".rdata", recursive = TRUE)
L_ml <- lapply(paste("Coefficient_optimization/Multi_step_predictions/ml_vs_mcss/Lists/ml/",files, sep = ""), function(x) mget(load(x))$results)

temp <- unlist(sapply(1:length(L_mcss), function(i) {L_mcss[[i]]$converge} ))
length(temp)

temp <- unlist(sapply(1:length(L_ml), function(i) {L_ml[[i]]$converge} ))
length(temp)

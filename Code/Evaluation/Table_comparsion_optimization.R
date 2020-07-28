library(xtable)

#### TABLES WITH BEST PERFORMING MODELS FOR EACH STATION HAVE BEEN MADE
#### HERE WE READ THEM IN AND TAKE AVERAGE PERFORMANCE OVER INTERACTION(STATION, OPTIMIZATION) FOR BOTH PI AND ACCURACY



## READ IN THE BEST MODELS
## We only load the best PI models based on PI
## or best accuracy models based on accuracy.
## ....The accuracy tables based on the best PI are not really relevant here..

# DDS
load("Evaluation/DDS/best_models/Damningen_PI_PI.Rdata")
load("Evaluation/DDS/best_models/Damningen_Accuracy_Accuracy.Rdata")
load("Evaluation/DDS/best_models/Damhusaen_PI_PI.Rdata")
load("Evaluation/DDS/best_models/Damhusaen_Accuracy_Accuracy.Rdata")
# load("Evaluation/DDS/best_models/Damningen_PI_Accuracy.Rdata")
# load("Evaluation/DDS/best_models/Damningen_Accuracy_PI.Rdata")
# load("Evaluation/DDS/best_models/Damhusaen_PI_Accuracy.Rdata")
# load("Evaluation/DDS/best_models/Damhusaen_Accuracy_PI.Rdata")

DDS_Damningen_PI_PI <- Damningen_PI_PI
DDS_Damningen_Accuracy_Accuracy <- Damningen_Accuracy_Accuracy
DDS_Damhusaen_PI_PI <- Damhusaen_PI_PI
DDS_Damhusaen_Accuracy_Accuracy <- Damhusaen_Accuracy_Accuracy

# NELDER-MEAD
load("Evaluation/Nelder-Mead/best_models/Damningen_PI_PI.Rdata")
load("Evaluation/Nelder-Mead/best_models/Damningen_Accuracy_Accuracy.Rdata")
load("Evaluation/Nelder-Mead/best_models/Damhusaen_PI_PI.Rdata")
load("Evaluation/Nelder-Mead/best_models/Damhusaen_Accuracy_Accuracy.Rdata")
# load("Evaluation/DDS/best_models/Damningen_PI_Accuracy.Rdata")
# load("Evaluation/DDS/best_models/Damningen_Accuracy_PI.Rdata")
# load("Evaluation/DDS/best_models/Damhusaen_PI_Accuracy.Rdata")
# load("Evaluation/DDS/best_models/Damhusaen_Accuracy_PI.Rdata")


NM_Damningen_PI_PI <- Damningen_PI_PI
NM_Damningen_Accuracy_Accuracy <- Damningen_Accuracy_Accuracy
NM_Damhusaen_PI_PI <- Damhusaen_PI_PI
NM_Damhusaen_Accuracy_Accuracy <- Damhusaen_Accuracy_Accuracy

rm(Damningen_PI_PI, Damningen_Accuracy_Accuracy, Damhusaen_PI_PI, Damhusaen_Accuracy_Accuracy)

### Take colmeans of tables to get average performance


DDS_Damningen_PI_PI <- colMeans(DDS_Damningen_PI_PI[,5:ncol(DDS_Damhusaen_PI_PI)])
DDS_Damningen_Accuracy_Accuracy <- colMeans(DDS_Damningen_Accuracy_Accuracy[,5:ncol(DDS_Damningen_Accuracy_Accuracy)])
DDS_Damhusaen_PI_PI <- colMeans(DDS_Damhusaen_PI_PI[,5:ncol(DDS_Damhusaen_PI_PI)])
DDS_Damhusaen_Accuracy_Accuracy <- colMeans(DDS_Damhusaen_Accuracy_Accuracy[,5:ncol(DDS_Damhusaen_Accuracy_Accuracy)])


NM_Damningen_PI_PI <- colMeans(NM_Damningen_PI_PI[,5:ncol(NM_Damhusaen_PI_PI)])
NM_Damningen_Accuracy_Accuracy <- colMeans(NM_Damningen_Accuracy_Accuracy[,5:ncol(NM_Damningen_Accuracy_Accuracy)])
NM_Damhusaen_PI_PI <- colMeans(NM_Damhusaen_PI_PI[,5:ncol(NM_Damhusaen_PI_PI)])
NM_Damhusaen_Accuracy_Accuracy <- colMeans(NM_Damhusaen_Accuracy_Accuracy[,5:ncol(NM_Damhusaen_Accuracy_Accuracy)])

### Forrm PI table
PI <- rbind(DDS_Damningen_PI_PI, NM_Damningen_PI_PI, DDS_Damhusaen_PI_PI, NM_Damhusaen_PI_PI)
rownames(PI) <- c("Dæmmningen DDS",
                      "Dæmmningen Nelder-Mead",
                      "Damhusåen DDS",
                      "Damhusåen Nelder-Mead")

### Form accuracy table
Accuracy <- rbind(DDS_Damningen_Accuracy_Accuracy, NM_Damningen_Accuracy_Accuracy, DDS_Damhusaen_Accuracy_Accuracy, NM_Damhusaen_Accuracy_Accuracy)
rownames(Accuracy) <- c("Dæmmningen DDS",
                  "Dæmmningen Nelder-Mead",
                  "Damhusåen DDS",
                  "Damhusåen Nelder-Mead")



digits1 <- as.vector(c(0,2,2,2), mode = "numeric")
digits2 <- as.vector(c(0,2,2,2,2,2,2), mode = "numeric")
xtable(PI, type = "latex", digits = digits1)
xtable(Accuracy, type = "latex", digits = digits2)



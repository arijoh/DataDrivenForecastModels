

load("Forecasting/publication/models/ErrorMetric/Damningen_PI.RData")
load("Forecasting/publication/models/ErrorMetric/Damningen_Accuracy.RData")
load("Forecasting/publication/models/ErrorMetric/Damhusaen_PI.RData")
load("Forecasting/publication/models/ErrorMetric/Damhusaen_Accuracy.RData")

## Fuction for plotting
source("Forecasting/publication/ErrorMetric/Plot.R")


m1_Damningen <- DamhusaenList_PI[[1]]
m2_Damningen <- DamhusaenList_Accuracy[[1]]
m1_damhusaen <- DamhusaenList_PI[[1]]
m2_damhusaen <- DamhusaenList_Accuracy[[1]]


titles <- c("Damningen - PI90", "Damningen - Accuracy", "Damhusaen - PI90", "Damhusaen - Accuracy")



### testing
model1_damningen <- m1_Damningen
model2_damningen <- m2_Damningen
model1_damhusaen <- m1_damhusaen
model2_damhusaen <- m2_damhusaen


p <- plotModel(m1_Damningen, m2_Damningen, m1_damhusaen, m2_damhusaen, titles) ## returns a plot that combines all four models
p















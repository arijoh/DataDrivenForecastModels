

load("Forecasting/publication/models/ErrorMetric/Damningen_PI.RData")
load("Forecasting/publication/models/ErrorMetric/Damningen_Accuracy.RData")
load("Forecasting/publication/models/ErrorMetric/Damhusaen_PI.RData")
load("Forecasting/publication/models/ErrorMetric/Damhusaen_Accuracy.RData")

## Fuction for plotting
source("Forecasting/publication/models/ErrorMetric/Plot.R")

# i <- 1
# m1_Damningen <- DamningenList_PI[[i]]
# m2_Damningen <- DamningenList_Accuracy[[i]]
# m1_damhusaen <- DamhusaenList_PI[[i]]
# m2_damhusaen <- DamhusaenList_Accuracy[[i]]
# 
# titles <- c("Dæmningen - PI ", "Dæmningen - Accuracy", "Damhusåen - PI", "Damhusåen - Accuracy")
# 
# 
# 
# ### testing
# # model1_damningen <- m1_Damningen
# # model2_damningen <- m2_Damningen
# # model1_damhusaen <- m1_damhusaen
# # model2_damhusaen <- m2_damhusaen
# 
# 
# p <- plotModel(m1_Damningen, m2_Damningen, m1_damhusaen, m2_damhusaen, titles) ## returns a plot that combines all four models
# p



for (i in 1:10){
  print(i)
  m1_Damningen <- DamningenList_PI[[i]]
  m2_Damningen <- DamningenList_Accuracy[[i]]
  m1_damhusaen <- DamhusaenList_PI[[i]]
  m2_damhusaen <- DamhusaenList_Accuracy[[i]]
  
  titles <- c("Dæmningen - PI ", "Dæmningen - Accuracy", "Damhusåen - PI", "Damhusåen - Accuracy")
  p <- plotModel(m1_Damningen, m2_Damningen, m1_damhusaen, m2_damhusaen, titles)
  
  filename <- paste("Forecasting/publication/models/ErrorMetric/Figures/model_error_metrics_", i, ".pdf", sep = "")
  print(filename)
  
  ggsave(filename = filename, height= 5, width = 9)
  p
  dev.off()
}

















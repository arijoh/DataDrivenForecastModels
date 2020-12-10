

load("Forecasting/publication/models/Regressor/Damningen_ARIMA.RData")
load("Forecasting/publication/models/Regressor/Damningen_ARIMAX.RData")
load("Forecasting/publication/models/Regressor/Damhusaen_ARIMA.RData")
load("Forecasting/publication/models/Regressor/Damhusaen_ARIMAX.RData")

## Fuction for plotting
source("Forecasting/publication/models/Regressor/Plot.R")


# i <- 2
# m1_Damningen <- DamningenList_ARIMA[[i]]
# m2_Damningen <- DamningenList_ARIMAX[[i]]
# m1_Damhusaen <- DamhusaenList_ARIMA[[i]]
# m2_Damhusaen <- DamhusaenList_ARIMAX[[i]]
# 
# titles <- c("Dæmningen - ARIMA", "Damhusåen - ARIMA", "Dæmningen - ARIMAX", "Damhusåen - ARIMAX")
# 
# 
# 
# ### testing
# model1_damningen <- m1_Damningen
# model2_damningen <- m2_Damningen
# model1_damhusaen <- m1_Damhusaen
# model2_damhusaen <- m2_Damhusaen
# 
# 
# p <- plotModel(m1_Damningen, m2_Damningen, m1_Damhusaen, m2_Damhusaen, titles) ## returns a plot that combines all four models
# 
# filename <- "Forecasting/publication/models/Regressor/Figures/test.tiff"
# print(filename)
# 
# 
# tiff(filename = filename, units = "mm", width = 190, height = 80, res = 500)
# p
# dev.off()



for (i in 1:10){
  
  m1_Damningen <- DamningenList_ARIMA[[i]]
  m2_Damningen <- DamningenList_ARIMAX[[i]]
  m1_damhusaen <- DamhusaenList_ARIMA[[i]]
  m2_damhusaen <- DamhusaenList_ARIMAX[[i]]
  
  titles <- c("Dæmningen - ARIMA", "Damhusåen - ARIMA", "Dæmningen - ARIMAX", "Damhusåen - ARIMAX")
  p <- plotModel(m1_Damningen, m2_Damningen, m1_damhusaen, m2_damhusaen, titles)
  
  filename <- paste("Forecasting/publication/models/Regressor/Figures/model_regressors_", i, ".tiff", sep = "")
  print(filename)
  
  tiff(filename = filename, units = "mm", width = 190, height = 80, res = 500)
  print(p)
  dev.off()

}
 


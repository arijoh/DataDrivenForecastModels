

load("Forecasting/publication/models/Objective_function_criteria/Damningen_ss.RData")
load("Forecasting/publication/models/Objective_function_criteria/Damningen_ms.RData")
load("Forecasting/publication/models/Objective_function_criteria/Damhusaen_ss.RData")
load("Forecasting/publication/models/Objective_function_criteria/Damhusaen_ms.RData")

## Fuction for plotting
source("Forecasting/publication/models/Objective_function_criteria/Plot.R")

# i <- 1
# m1_Damningen <- DamningenList_ss[[i]]
# m2_Damningen <- DamningenList_ms[[i]]
# m1_damhusaen <- DamhusaenList_ss[[i]]
# m2_damhusaen <- DamhusaenList_ms[[i]]
# 
# titles <- c("Dæmningen - single-step", "Damhusåen - single-step", "Dæmningen - multi-step", "Damhusåen - multi-step")
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
# filename <- "Forecasting/publication/models/Objective_function_criteria/Figures/test.tiff"
# print(filename)
# 
# 
# tiff(filename = filename, units = "mm", width = 190, height = 80, res = 500)
# p
# dev.off()



for (i in 1:10){

  m1_Damningen <- DamningenList_ss[[i]]
  m2_Damningen <- DamningenList_ms[[i]]
  m1_Damhusaen <- DamhusaenList_ss[[i]]
  m2_Damhusaen <- DamhusaenList_ms[[i]]
  titles <- c("Dæmningen - single-step", "Damhusåen - single-step", "Dæmningen - multi-step", "Damhusåen - multi-step")
  p <- plotModel(m1_Damningen, m2_Damningen, m1_Damhusaen, m2_Damhusaen, titles)
  
  filename <- paste("Forecasting/publication/models/Objective_function_criteria/Figures/model_ofc_", i, ".tiff", sep = "")
  print(filename)

  tiff(filename = filename, units = "mm", width = 190, height = 80, res = 500)
  print(p)
  dev.off()

}




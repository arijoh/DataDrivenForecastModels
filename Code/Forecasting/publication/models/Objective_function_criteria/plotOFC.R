

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
# titles <- c("Dæmningen - ss", "Dæmningen - ms", "Damhusåen - ss", "Damhusåen - ms")
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

  m1_Damningen <- DamningenList_ss[[i]]
  m2_Damningen <- DamningenList_ms[[i]]
  m1_Damhusaen <- DamhusaenList_ss[[i]]
  m2_Damhusaen <- DamhusaenList_ms[[i]]
  titles <- c("Dæmningen - Single-step", "Dæmningen - Multi-step", "Damhusåen - Single-step", "Damhusåen - Multi-step")
  p <- plotModel(m1_Damningen, m2_Damningen, m1_Damhusaen, m2_Damhusaen, titles)
  
  filename <- paste("Forecasting/publication/models/Objective_function_criteria/Figures/model_ofc_", i, ".pdf", sep = "")
  print(filename)

  ggsave(filename = filename, height= 5, width = 10)
  p
  dev.off()

}

















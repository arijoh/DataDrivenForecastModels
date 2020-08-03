

load("Evaluation/Nelder-Mead/best_models/List/PI_AVG/DamhusaenList_PI.Rdata")
load("Evaluation/Nelder-Mead/best_models/List/PI_AVG/DamningenList_PI.Rdata")


model_damningen <- DamningenList_PI[[2]]
model_damhusaen <- DamhusaenList_PI[[3]]



data <- model_damhusaen
data$accuracy
data <- unlist(data$evaluation$evaluation60)

data/sum(data)


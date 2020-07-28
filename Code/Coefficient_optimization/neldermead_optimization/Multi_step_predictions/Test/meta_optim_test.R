

setwd("/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code")
source("Coefficient optimization/Multi step predictions/Test/meta_optim.R")

##################### Reading in data ##################### 
s1_training <-  read.csv("../Data/Training data/s1_training.txt", header = TRUE, sep = "\t")
s1 <- s1_training$Value
s1_wwIndex <- read.csv("../Data/Training data/s1_WW_training.txt", header = TRUE, sep = "\t")
Regressor <-  read.csv("../Data/Training data/d_training.txt", header = TRUE, sep = "\t")
Regressor <- Regressor$Value
ts <- s1 
observed <- s1
wwIndex <- s1_wwIndex$Flag
Regressor_original <- read.csv("../Data/Training data/d_training.txt", header = TRUE, sep = "\t")$Value


## We passi into meta_optim, the order, reg.nr and re.lag


reg.nr_try <- c(4, 6, 8, 4, 6, 8)
reg.lag_try <- c(5, 10, 15, 5, 10, 15)
orders_try <- list(c(4,0,1),
                   c(4,1,4),
                   c(8,1,4),
                   c(3,0,8),
                   c(2,0,3),
                   c(1,1,1))

L <- list()

for (i in 1:6){
  reg.nr <- reg.nr_try[i]
  reg.lag <- reg.lag_try[i]
  orders <- unlist(orders_try[[i]])
  system.time(
    L[[i]] <- meta_optim(orders, ARIMAX = TRUE, reg.nr, reg.lag)
  )
}


save(L, file = "Coefficient optimization/Multi step predictions/Test/CodetestL.RData")


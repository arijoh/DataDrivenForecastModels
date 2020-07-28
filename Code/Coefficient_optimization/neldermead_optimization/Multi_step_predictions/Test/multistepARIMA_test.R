


source("Coefficient optimization/Multi step predictions/Test/multistepARIMA.R")



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
#Regressor <- 1:length(Regressor)



# We pass in this
reg.nr <- 10
reg.lag <- 9
order <- c(5,1,3)


# In fucntion
if ((!is.null(reg.lag)) || (!is.null(reg.nr))){
  reg = as.vector(1:reg.nr)
  reg <- unlist(sapply(reg, function(i){
    idx <- i - 1
    if (idx == 0){
      Regressor
    }
    else{
      c(rep(NA, idx), head(Regressor, -idx))
    }
  }))
  
  
  if (reg.nr > 1){reg <- rbind(matrix(NA, nrow = reg.lag, ncol = ncol(reg)) ,reg[1:(nrow(reg)-reg.lag),])}
  else if (reg.nr == 1){reg <- c(rep(NA, reg.lag) ,reg[1:(nrow(reg)-reg.lag),])}
  else if (reg.nr < 1){print("Reg.nr needs to be more than 1 if an external regressor is wanted")}
}


model <- arima(x = ts, order = order, xreg = reg)
coefficients <- coefficients(model)
output <- multistepARIMA(order, coefficients = coefficients, external_regressor = reg)


v <- lapply(1:10, function(i){
  mean((unlist(output$residuals[[i]]))^2, na.rm = T)
})


v <- unlist(v)
barplot(abs(v))

v[1]
mean(abs(model$residuals), na.rm = T)
mean(v)









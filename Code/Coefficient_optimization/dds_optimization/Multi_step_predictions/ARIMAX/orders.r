
p <- 0:8
d <- 0:1
q <- 0:8
reg.lag <- c(5, 10, 15)
reg.nr <- c(2, 4, 6, 8, 10)

# Number of current models are 2430.
# It could be good to break this up to 5 segments.
orders <- expand.grid(p,d,q,reg.lag,reg.nr)

save(orders, file = "Coefficient_optimization/dds_optimization/Multi_step_predictions/ARIMAX/orders.RData")

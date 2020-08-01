


t <- 100
x <- e <- rnorm(n = t, mean = 0, sd = 1)

for (t in 2:t) {
  x[t] <- x[t - 1] + e[t]
}


par(mfrow=c(1,2))
setEPS()
plot.ts(x, xlab="Time", ylab = "Y(t)", main = "Random walk, I = 0", cex.main=1)
plot.ts(diff(x), xlab="Time", ylab = "Y(t)", main = "Random walk, I = 1",cex.main=1)
postscript(file = "../Figures/eps/Differentiation.eps", onefile = TRUE, horiz=TRUE, width = 5, height = 5)
dev.off()

plot(acf(x, plot = FALSE))
plot(acf(diff(x), plot = FALSE))

# Function to perform Monte Carlo estimation of the binomial success probability
CLT_binom <- function(B, n, p) {
  Y <- rbinom(B, n, p)
  Ymean <- Y / n  
  var.mean <- p * (1 - p) / n  
  p.MC <- mean(Ymean)  
  varp.MC <- var(Ymean)  
  hist(Ymean, col = "gray", probability = TRUE, 
       main = paste("Histogram of Estimates (n =", n, ")"))
  curve(dnorm(x, mean = p, sd = sqrt(p * (1 - p) / n)), 
        add = TRUE, col = "blue", lwd = 2)
  return(list(var.mean = var.mean, p.MC = p.MC, varp.MC = varp.MC))
}

par(mfrow = c(2, 2))  
CLT_binom(100000, 10, 0.3)
CLT_binom(100000, 30, 0.3)
CLT_binom(100000, 100, 0.3)
CLT_binom(100000, 1000, 0.3)
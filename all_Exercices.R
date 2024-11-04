# Function to simulate sampling distribution of the mean for Poisson distribution
pois_CLT <- function(n, mu, B) {
  par(mfrow = c(2, 2))  
  for (i in 1:2) {
    Y <- matrix(rpois(n[i] * B, mu), ncol = n[i])  
    Ymean <- apply(Y, 1, mean)  
    barplot(table(Y[1,]), main = paste("Sample data distribution (n =", n[i], ")"), 
            xlab = "y", col = "lightsteelblue")
    hist(Ymean, main = paste("Sampling distribution (n =", n[i], ")"), 
         xlab = expression(bar(y)), col = "lightsteelblue", probability = TRUE)
  }
}

n <- c(10, 100)
pois_CLT(n, 0.7, 100000)

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

# Function to estimate the standard error of the sample median using Monte Carlo
sdmed <- function(B, n, mu, sigma) {
  medians <- rep(0, B)
  for (i in 1:B) {
    y <- rnorm(n, mu, sigma)  
    medians[i] <- median(y)  
  }
  return(sd(medians))  
}

standard_error <- sdmed(1000000, 100, 100, 16)
standard_error

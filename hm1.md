
# Exercise 3.18: Simulating Sampling Distribution of a Statistic
**Exercise Statement**: Simulate the sampling distribution of the mean for a sample of size \( n \) from a Poisson distribution with mean \( \mu \). Create histograms for sample sizes \( n = 10 \) and \( n = 100 \) with \( B = 100,000 \) simulated samples.

```{r echo=TRUE}
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
```

# Exercise 3.28: Monte Carlo Estimation of Binomial Success Probability
**Exercise Statement**: Perform Monte Carlo simulation to estimate the success probability \( \pi \) and the variance of the sampling distribution for a binomial distribution.

```{r echo=TRUE}
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
```

# Exercise 3.24: Monte Carlo Estimation of Sample Median
**Exercise Statement**: Estimate the standard error of the sample median using Monte Carlo simulation for a normal distribution.

```{r echo=TRUE}
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
```


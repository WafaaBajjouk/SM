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

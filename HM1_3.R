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

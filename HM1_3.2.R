# Part a: 
n <- 1648
SE <- sqrt(p * (1 - p) / n)
cat("Standard Error:", SE, "\n")

# Part b: simulation
set.seed(123)  
simulations <- 10000
sample_props <- rbinom(simulations, n, p) / n

mean_sample_prop <- mean(sample_props)
sd_sample_prop <- sd(sample_props)

hist(sample_props, main = "Simulated Sampling Distribution of Proportion",
     xlab = "Sample Proportion", breaks = 50, col = "lightblue")

observed_prop <- 0.515
is_surprising <- observed_prop > (mean_sample_prop + 2 * sd_sample_prop) || 
                 observed_prop < (mean_sample_prop - 2 * sd_sample_prop)

cat("Mean of Sample Proportions:", mean_sample_prop, "\n")
cat("Standard Deviation of Sample Proportions:", sd_sample_prop, "\n")
cat("Is the observed proportion surprising?:", is_surprising, "\n")

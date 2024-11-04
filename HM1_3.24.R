set.seed(456)
n_sample <- 1000
population_distribution <- rpois(n_sample, lambda = 1.5)

sample_mean <- mean(population_distribution)
sample_sd <- sd(population_distribution)

hist(population_distribution, main = "Distribution of Number of Alcoholic Drinks",
     xlab = "Number of Drinks", col = "lightgreen")

cat("Sample Mean:", sample_mean, "\n")
cat("Sample Standard Deviation:", sample_sd, "\n")

set.seed(789)
n_samples <- 10000
sample_means <- replicate(n_samples, mean(rpois(n_sample, lambda = 1.5)))

mean_sampling_distribution <- mean(sample_means)
sd_sampling_distribution <- sd(sample_means)

hist(sample_means, main = "Sampling Distribution of Sample Means",
     xlab = "Sample Mean", col = "lightblue")

cat("Mean of Sampling Distribution:", mean_sampling_distribution, "\n")
cat("Standard Deviation of Sampling Distribution:", sd_sampling_distribution, "\n")

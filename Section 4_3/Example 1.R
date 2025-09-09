###############################################################################
## EXAMPLE 1: Analysis of hierarchical data
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# Here is shown a comparison of two reasonable ways of testing differences
# in nested data, comparing it to a problematic one (ignoring the nesting).
# Nesting means typically that we have multiple measurements from an experimental
# subject, e.g. multiple cells per animal.

# Try to run the script multiple times with condition_null <- TRUE. Notice how
# the p-values using a t-test without averaging are lower than the other two, 
# and are often significant at the 0.05 level, where the other two are not.
# This demonstrates, how pseudoreplicating (taking nested measurements as 
# independent) can strongly inflate the rate of false positives.
# At the same time, the results using nested statistics or a t-test on averages
# are very similar.

# Now, if you change condition_null to FALSE, and again repeatedly run the script,
# you can see that also in this situation, the first two methods give very
# similar results, showing that averaging cells per animal can be an adequate
# solution when each animal produces a similar number of measured cells.

library(nlme)  # Package with functionality for multilevel statistics

# First, synthetic data are generated. For each of n_animals, the true
# average values in each animal in groups A and B are generated from normal 
# distribution. If condition_null is TRUE, data in group B are generated so that
# the animals have the same mean of the given property (5). If condition_null is 
# FALSE, the alternative hypothesis and group B has a higher average value than
# group A. 

n_animals <- 10
cells_per_animal <- 5
condition_null <- TRUE
true_means_A <- rnorm(n_animals, mean = 5, sd = 1) # Group A animal means

if (condition_null){
  true_means_B <- rnorm(n_animals, mean = 5, sd = 1) # Group B animal means
} else {
  true_means_B <- rnorm(n_animals, mean = 7, sd = 1) # Group B animal means
}

# Generate measurements for each animal
# measurements_A and measurements_B are each a matrix with n_animals columns
# and cells_per_animal rows. 
# measurements_A[i,j] represents the measurement in the i-th cell in j-th animal.
# In each animal, we generate synthetic
measurements_A <- sapply(true_means_A, function(mu) rnorm(cells_per_animal,
                                                          mean = mu,
                                                          sd = 1))

measurements_B <- sapply(true_means_B, function(mu) rnorm(cells_per_animal, 
                                                          mean = mu, 
                                                          sd = 1))

# Combine data into a data frame which contains all the data for groups A and B,
# tracking from which animal and cell within the group a measurement is.
data <- data.frame(
  group = rep(c("A", "B"), each = n_animals * cells_per_animal),
  animal = factor(rep(1:(2*n_animals), each = cells_per_animal)),
  cell = factor(rep(1:cells_per_animal, 2 * n_animals)),
  measurement = c(as.vector(measurements_A), as.vector(measurements_B))
)

# Second, we can finally run the tests for differences between groups A and B.

# 1) Fit the linear mixed-effects model using nlme
model <- lme(measurement ~ group, random = ~ 1 | animal/cell, data = data)
res = anova(model)
p_value_nested = res$`p-value`[2]

# 2) Run a t-test on averages of cells in each animal (i.e. comparing n_animals
# data points in each group)
animal_means <- aggregate(measurement ~ group + animal, data = data, mean)
t_test_result <- t.test(measurement ~ group, data = animal_means)
p_value_ttest_averaged = t_test_result$p.value

# Run t-test on the raw data points (non-hierarchical) - this is shown for the 
# purpose of illustration, this is exactly what should NOT be done in reality.
# This compares n_animals * cells_per_animal in each group
t_test_raw <- t.test(measurement ~ group, data = data)
p_value_ttest_unaveraged = t_test_raw$p.value

cat('p-value using hierarchical statistics: ', p_value_nested)
cat('p-value using a t-test on animal averages: ', p_value_ttest_averaged)
cat('p-value using a t-test without averaging: ', p_value_ttest_unaveraged)



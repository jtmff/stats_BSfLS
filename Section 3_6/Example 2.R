###############################################################################
## EXAMPLE 2: A paired permutation test for testing difference in means.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# This is a permutation test for paired data, with the null hypothesis that
# the mean is 0 (e.g., there is no effect of a treatment). It involves shuffling,
# for each subject in which paired measurements are taken, the order of the two 
# measurements (e.g. which value was taken before and which after a treatment).
set.seed(222)

# Definition of the permutation test function
perm_test_paired <- function(df, diff_observed, n_repeats = 50000){
  # df - input data frame with the field 'value'
  # diff_observed - observed difference in the tested data
  # n_repeats - number of tested permutations
  
  # Distribution of mean differences after-before treatment
  diffs_permuted <- rep(0, n_repeats)
  
  for (i_repeat in 1:n_repeats){
    # The following randomly flips the signs of the observed differences,
    # permuting the assignment of before/after treatment. 
    df$flipped_value <- df$value * sample(c(-1, 1), size = nrow(df), replace = TRUE)
    diffs_permuted[i_repeat] <- mean(df$flipped_value)
  }
  
  # As described in the book's text, 1 is added to numerator and denominator
  return((sum(abs(diffs_permuted) >= abs(diff_observed))+1)/(n_repeats+1))
}

# Generate synthetic paired data in 15 patients, measuring a certain variable
# before and after treatment.
vals_before <-runif(15, min = -5, max = 5)
vals_after <-runif(15, min = -2, max = 10)

# We further work only with the difference after-before treatment
df <- data.frame(value = vals_after - vals_before)
# Observed mean change after-before treatment
diff_observed <- mean(df$value) 

pval <- perm_test_paired(df, diff_observed)
print(pval) # ~0.0179
# If you change vals_after to the null hypothesis holding, i.e., 
# coming from uniform [-5,5], see how the p-value increases.

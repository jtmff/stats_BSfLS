###############################################################################
## EXAMPLE 1: An unpaired permutation test for testing difference in means.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# This example shows how to do a permutation test using the null hypothesis
# that two groups of data have the same mean (like a t-test). 
# It involves repeatedly shuffling the data, measuring in how many cases 
# is the difference in means at least as great as the one observed in data.

# Set seed for reproducibility
set.seed(222)

# Definition of permutation test function
perm_test_unpaired <- function(df, diff_observed, n_repeats = 50000){
  # df - input data frame with the field 'value'
  # diff_observed - observed difference in the tested data
  # n_repeats - number of tested permutations
  
  # This will be the distribution of mean differences between groups after
  # the data are permuted.
  diffs_permuted <- rep(0, n_repeats)
  
  for (i_repeat in 1:n_repeats){
    # For each repeat, shuffle the data
    df$Group_permuted <- sample(df$Group) 
    # And calculate the difference in means after shuffling
    df_summaryGP <- aggregate(value ~ Group_permuted, 
                              data = df, FUN = mean)
    diffs_permuted[i_repeat] <- df_summaryGP[2,2] - df_summaryGP[1,2]
  }
  
  # As described in the book's text, 1 is added to numerator and denominator
  return((sum(abs(diffs_permuted) >= abs(diff_observed))+1)/(n_repeats+1))
}

# Sample n=15 points from uniform distributions for two groups
# This is a similar scenario to Figure 3.22A-C
n <- 15
group1 <- runif(n, min = 3, max = 7)  # Uniform(3,7)
group2 <- runif(n, min = 4, max = 12)  # Uniform(5,9)

# Observed difference in means (will be compared to a distribution of
# differences generated through data shuffling).
diff_observed <- mean(group2) - mean(group1) 

# Combine data into a data frame
df <- data.frame(
  value = c(group1, group2),
  Group = factor(rep(c("Group 1", "Group 2"), each = n))
)

pval <- perm_test_unpaired(df, diff_observed)
print(pval) # ~0.00186

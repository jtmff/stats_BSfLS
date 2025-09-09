###############################################################################
## EXAMPLE 3: A simple simulation study to compare the basic vs Welch's t-test on data with uneven variance.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################

n_group_A = 40
n_group_B = 10

n_repeats = 10000  # How many times do we try to simulate the random generation of data?
pvals_basic = list()
pvals_welch = list()

for (i_repeat in 1:n_repeats) {
  data_A = rnorm(n_group_A, mean = 7, sd = 1)  # The first group consists of 10 points, sampled from a normal distribution with mean of 5, and relatively low spread (standard deviation of 1).
  data_B = rnorm(n_group_B, mean = 7, sd = 5)  # The second group has more points and the same average, but a larger variance
  
  results_basic <- t.test(data_A, data_B, var.equal = TRUE)  # Standard ("pooled") t-test
  results_welch <- t.test(data_A, data_B, var.equal = FALSE)  # Welch t-test
  pvals_basic[i_repeat] = results_basic$p.value
  pvals_welch[i_repeat] = results_welch$p.value
}

proportion_basic_sig = mean(pvals_basic < 0.05)  # This syntax first generates a binary (0 or 1) vector with ones where the p-value is <0.05, 
                                                 # and then takes the average of this, giving the proportion of p-values < 0.05.
proportion_welch_sig = mean(pvals_welch < 0.05)

sprintf("Basic t-test proportion of p<0.05: %f, Welch t-test  proportion of p-value<0.05: %f",proportion_basic_sig, proportion_welch_sig)

# In this case, the basic t-test assuming equal variance clearly performs poorly, rejecting the (true) null hypothesis
# in ca. 31% cases, whereas the Welch test correctly maintains rejection level at near 5%.
# Feel free to swap the group sizes, making the second group larger - in that case, the basic t-test almost never rejects
# the null hypothesis, also not operating at the 5% rejection level. Either imbalance is problematic in a different way,
# yielding excessive false positives or false negatives when used on data.

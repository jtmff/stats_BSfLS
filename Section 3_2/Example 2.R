###############################################################################
## EXAMPLE 2: Showing superiority of Brunner-Munzel test over the Mann-Whitney U test when null hypothesis holds, but distributions of the two groups have a different spread.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggplot2)
library(brunnermunzel)
# This script shows how the Mann-Whitney U test is not rejecting true null
# hypothesis at the 0.05 level when the variance of the group differs.

# We start with same-size groups.
n_group_A = 20
n_group_B = 20

n_repeats = 10000  # How many times do we try to simulate the random generation of data?
pvals_MW = list()
pvals_BM = list()

for (i_repeat in 1:n_repeats) {
  # We generate two groups of data, with the same mean, but a different spread.
  data_A = rnorm(n_group_A, mean = 7, sd = 1)  
  data_B = rnorm(n_group_B, mean = 7, sd = 5)  
  
  # Comparing the two tests
  results_MW <- wilcox.test(data_A, data_B) 
  results_BM <- brunnermunzel.test(data_A, data_B)  
  pvals_MW[i_repeat] = results_MW$p.value
  pvals_BM[i_repeat] = results_BM$p.value
}

# This first generates a binary (0 or 1) vector with 1s where the p-value is <0.05, and then takes the average of this, giving the proportion of p-values < 0.05.
proportion_MW_sig = mean(pvals_MW < 0.05)  
proportion_BM_sig = mean(pvals_BM < 0.05)

sprintf("Mann-Whitney proportion of p<0.05: %f, Brunner-Munzel t-test  proportion of p-value<0.05: %f",proportion_MW_sig, proportion_BM_sig)

# Feel free to experiment with group sizes (e.g. A/B being 10/20 or 20/10 points). With balanced group sizes, 
# The Brunner-Munzel test will generally reject the null hypothesis at around 5% rate.
# With balanced group sizes, the Mann-Whitney test will typically reject the null hypothesis
# at a slightly higher rate. If the first data group is substantially smaller than the second,
# the rejection rate of Mann-Whitney test will be very low, and conversely, 
# if the second data group has markedly fewer samples, the Mann-Whitney test will reject the
# null hypothesis at a much higher rate. This is clearly problematic, as the null hypothesis
# holds in all those cases - the test should be rejecting null hypotheses at 5% rate.
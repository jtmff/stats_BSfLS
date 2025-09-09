###############################################################################
## EXAMPLE 5: An illustration of equivalence testing
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library("TOSTER")
set.seed(51)
# Two groups of data drawn from normal distribution are compared.
n_group_A = 100
n_group_B = 100
data_A = rnorm(n_group_A, mean = 7, sd = 1)
data_B = rnorm(n_group_A, mean = 7, sd = 1)

# As inputs to the procedure, we specify means and sds, as well as group sizes.
# The eqb parameter is the equivalent boundary defining distance from zero.
# In this case, it corresponds to the interval (-0.5, 0.5).
# var.equal switches between pooled-variance t-test and 
# Welch's test (used here).
tsum_TOST(m1=mean(data_A),m2=mean(data_B),sd1=sd(data_A),sd2=sd(data_B),
          n1=n_group_A,n2=n_group_B, eqb=0.5, 
          alpha = 0.05, var.equal=FALSE)

# In this case, the difference is statistically significant, indicating
# that the difference between groups is small.
# Feel free to explore how parameter choices affect the overall p-value.
# E.g. changing mean in one of the groups to 7.1 retains statistical 
# significance, but 7.5 does not. If your sample sizes is 10 instead of 
# 100, the test loses power and does not report a significant similarity.
# If you increase sd from 1 to 10, the p-value increases markedly. This shows 
# how the procedure takes into account that with highly variable data, it
# is harder to falsify your null hypothesis.

# Equivalence for one-sided test can be done as follows,
# with the parameters based on the differences between paired data.
tsum_TOST(m1 = 0.1, sd1 = 0.5, n1 = 30, eqb = 0.3)

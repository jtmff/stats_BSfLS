###############################################################################
## EXAMPLE 3: Demonstrating how to use the two-sample Kolmogorov-Smirnov and Anderson-Darling tests
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(dgof) # For the Kolmogorov-Smirnov
library(kSamples) # For Anderson-Darling

# We generate bimodal data in the first group
data_A = c(1, 1.25, 1.5, 2, 2.25, 2.6, 3, 3.2, 8, 9, 9.25, 9.5, 10, 11, 12)
# And different-shaped data in the second group
data_B = c(5, 5.5, 6, 6.25, 6.5, 6.75, 7, 7.5, 8.25, 9.75, 10.25, 11.25, 12.25)

# The two distributions are indeed different, but this is hard to pick up using a t-test (Welch used here)
results_welch <- t.test(data_A, data_B, var.equal = FALSE)
print(results_welch$p.value)  # p-value = 0.09

# The Mann-Whitney test likewise does not indicate a clear difference
results_MW <- wilcox.test(data_A, data_B)
print(results_MW$p.value) # p-value = 0.18

# However, the more general Kolmogorov-Smirnov test does produce a low p-value (ca. 0.02),
# indicating a difference in the distribution.
results_ks = ks.test(data_A, data_B)
print(results_ks$p.value)

# The Anderson-Darling also produces a relatively low p-value (two p-values are produced actually, 
# for two versions of the test, both near 0.07).
results_ad = ad.test(data_A, data_B)
print(results_ad)

# This does not mean the K-S or A-D tests should be always used over t-tests or Mann-Whitney U test,
# not at all - but they can be useful when the difference in data distributions is not captured well
# by properties that the other tests investigate (e.g. a difference in means in the case of t-test).
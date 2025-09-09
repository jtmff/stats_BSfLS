###############################################################################
## EXAMPLE 1: Illustrating Bonferroni and Benjamini-Hochberg corrections
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# A simulated scenario is shown here, where 1000 tests are carried out, each
# comparing data in two groups. Using a switch, you can change between null
# scenario (there is no difference in any group), and a case where there
# is a true difference in all the comparisons.

# If you repeatedly run the script with condition_null TRUE, you can see how 
# the uncorrected tests produce ca. 5% (around 50 for 1000 tests) cases of 
# p<0.05, which in this case are false positives. Conversely, the number of 
# false positives with the corrections is very low.

# If you switch to condition_null FALSE (presence of a true difference in
# all the tests), you will see that many of the differences are detected by the
# uncorrected test. A lower, but broadly similar number of differences are detected
# when using the Benjamini-Hochberg. Far fewer differences are detected when
# using the Bonferroni correction, highlighting its increased stringency (and,
# in this case, probably overt conservatism).

# Parameters
n_tests <- 1000   # Number of tests
n_samples <- 16   # Number of samples per test (half are group 1, half group 2)
condition_null <- TRUE # TRUE ~ no difference, FALSE ~ difference

# Synthetic data are generated. For the null scenario, all n_samples are drawn
# from the same normal distribution. When condition_null is FALSE, the data 
# belonging to the second group are on average increased by 2.
df <- matrix(rnorm(n_tests * n_samples), nrow = n_tests, ncol = n_samples)
if (condition_null == FALSE) { 
  n_diff <- n_tests / 2
  df[1:n_diff, (n_samples / 2 + 1):n_samples] <- 
    df[1:n_diff, (n_samples / 2 + 1):n_samples] + 2
}

# Perform a two-sample t-test for each comparison
p_values <- apply(df, 1, function(x) {
  group1 <- x[1:(n_samples/2)]
  group2 <- x[(n_samples/2 + 1):n_samples]
  t.test(group1, group2)$p.value
})

# Bonferroni correction (applied as scaling of p-values)
bonferroni_p <- p.adjust(p_values, method = "bonferroni")

# Benjamini-Hochberg correction
bh_p <- p.adjust(p_values, method = "BH")

cat('Number of p<0.05, uncorrected: ', sum(p_values<0.05))
cat('Number of p<0.05, Bonferroni: ', sum(bonferroni_p<0.05))
cat('Number of p<0.05, Benjamini-Hochberg: ', sum(bh_p<0.05))

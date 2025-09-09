###############################################################################
## EXAMPLE 1: An illustration of how to conduct an unpaired t-test 
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# This example reuses data that we have summarized and plotted in Example 1, code samples 1.2
dataA <- c(171.57, 191.66, 167.33, 201.35, 174.83, 178.04, 181.84, 175.55)
dataB <- c(145.71, 148.38, 169.11, 169.76, 144.23, 162.04, 141.11, 167.39)

results <- t.test(dataA, dataB, var.equal = TRUE) # var.equal = TRUE means that the standard t-test is used
print(results) # This writes out a summary of the results produced by the t-test
print(results$p.value)  # This produces the p-value for the comparison, which is low in this case.
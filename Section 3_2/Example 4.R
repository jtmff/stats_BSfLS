###############################################################################
## EXAMPLE 4: Showing how to run nonparametric tests for paired measurements: Wilcoxon signed-rank test and Sign test
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(DescTools) # For the sign test

dataPre <- c(9.60, 8.79, 9.53, 10.02, 10.48,	9.7, 8.79, 10.58, 8.98, 9.91)
dataPost <- c(9.49, 8.04, 7.67, 8.93, 6.54, 8.08, 7.80, 9.98, 9.06, 6.19)

# Wilcoxon signed-rank paired test:
results_wilcox = wilcox.test(dataPre, dataPost, paired = TRUE)
print(results_wilcox$p.value)

# Sign test
results_sign = SignTest(dataPre, dataPost)
print(results_sign$p.value)
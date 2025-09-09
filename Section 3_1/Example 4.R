###############################################################################
## EXAMPLE 4: An illustration of a paired t-test
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
data_pre <- c(9.60, 8.79, 9.53, 10.02, 10.48,	9.7, 8.79, 10.58, 8.98, 9.91)
data_post <- c(9.49, 8.04, 7.67, 8.93, 6.54, 8.08, 7.80, 9.98, 9.06, 6.19)

data_difference = data_post - data_pre 
# We can confirm that the pre measurements are on average somewhat larger
print(mean(data_difference))

# With mu = 0, the null hypothesis is that the mean difference pre/post treatment is 0.
results <- t.test(data_difference, mu = 0) 
# This writes out a summary of the results produced by the one-sample/paired t-test
print(results) 
# This produces the p-value for the comparison, which is low in this case.
print(results$p.value)  
# Now try adding a single outlier point - e.g. adding 25 to dataPre at the end, and adding 2 to the end of dataPost. 
# This adds a point which also follows the trend of the data from the first group being greater, but the p-value in fact increases.
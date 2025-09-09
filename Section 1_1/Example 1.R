###############################################################################
## EXAMPLE 1: Calculating mean, median, standard deviation, and interquartile range of data.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################

# Create a vector of 9 numbers
my_data <- c(2, 4, 5, 8, 10, 3, 14, 1, 9)

# Calculate mean and standard deviation
mean_value <- mean(my_data)
sd_value <- sd(my_data)

# Calculate median and interquartile range (the range between 25 and 75-quantile)
median_value = median(my_data)
q25 = quantile(my_data, 0.25)  # boundary of lower quartile (25-percentile)
q75 = quantile(my_data, 0.75)  # boundary of upper quartile (75-percentile)

# Print the results
cat("Data:", my_data, "\n")
cat("Mean:", mean_value, "\n")
cat("Standard Deviation:", sd_value, "\n")
cat("Median:", median_value, "\n")
cat("Interquartile range:", q25,"-",q75, "\n")


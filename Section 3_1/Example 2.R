###############################################################################
## EXAMPLE 2: An illustration of how to conduct an unpaired Welch t-test on data with uneven variance.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggplot2)
library(Cairo)

# Here we randomly generate data from normal distributions with given parameters
n_group_A = 10
n_group_B = 40
dataA = rnorm(n_group_A, mean = 5, sd = 1)  # The first group consists of 10 points, sampled from a normal distribution with mean of 5, and relatively low spread (standard deviation of 1).
dataB = rnorm(n_group_B, mean = 7, sd = 4)  # The second group has more points, has a higher average value, and more spread around the mean

results_basic <- t.test(dataA, dataB, var.equal = TRUE)  # Standard ("pooled") t-test
results_welch <- t.test(dataA, dataB, var.equal = FALSE)  # Welch t-test

# Print output to R console
sprintf("Basic t-test p-value: %f, Welch t-test p-value: %f", results_basic$p.value, results_welch$p.value)

df <- data.frame(
  Value = c(dataA, dataB),
  Group = c(rep("A", each = n_group_A), rep("B", each = n_group_B)) 
)

# Dotplot is another convenient way of plotting data, being similar to
# plots of points with jitter we have shown previously.
# A dotplot does not add random variation, but instead groups data points
# to indicate raw values and shape of distribution at the same time.
# If you prefer the jittered points, you can comment the geom_dotplot line
# and uncomment the one below with geom_jitter

plt <- ggplot(df, aes(x=Group, y=Value)) + 
  geom_dotplot(binaxis='y', stackdir='center') + 
  theme_minimal()+
  theme(axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        axis.title.x = element_text(size = 27),
        axis.title.y = element_text(size = 27))

CairoPNG(file = "tt_standardVsWelch.png", width = 600, height = 400)
print(plt)
dev.off()

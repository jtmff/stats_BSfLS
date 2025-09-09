###############################################################################
## EXAMPLE 1: One-way ANOVA
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# Generate synthetic data that compare percentual effects of three drugs 
set.seed(223)
n <- 12 # number of measurements for each drug
drug1 <- rnorm(n, mean = 10, sd = 15)
drug2 <- rnorm(n, mean = 50, sd = 15)
drug3 <- rnorm(n, mean = 40, sd = 15)

# Combine the data into a dataframe
df <- data.frame(
  Group = rep(c("Drug 1", "Drug 2", "Drug 3"), each = n),
  Value = c(drug1, drug2, drug3),
  X = rep(c(1, 2, 3), each = n)
)

# Perform ANOVA
anova_results <- aov(Value ~ Group, data = df)
summary(anova_results) # A low p-value of 4.46e-6 is provided

# Perform Tukey-Kramer post-hoc test
tukey_results <- TukeyHSD(anova_results)
print(tukey_results) # Low p-value comparing drugs 1&3 and 1&2, but not 2&3.
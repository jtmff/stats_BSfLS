###############################################################################
## EXAMPLE 2: Problematic use of one-way ANOVA
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# This script recreates the issue shown in Figure 3.17 A, showing how violating
# equal sd assumption together with uneven group sizes leads to problems.
set.seed(379)

# Generate data
n_control <- 60
n_drug <- 12
ctrl <- rnorm(n_control, mean = 0, sd = 3)
drug1 <- rnorm(n_drug, mean = 50, sd = 15)
drug2 <- rnorm(n_drug, mean = 40, sd = 15)


# Combine the data into a dataframe
df <- data.frame(
  Group = rep(c("Control", "Drug 1", "Drug 2"), times = c(n_control, n_drug, n_drug)),
  Value = c(ctrl, drug1, drug2),
  X = rep(c(1, 2, 3), times = c(n_control, n_drug, n_drug))
)

# Perform ANOVA
anova_results <- aov(Value ~ Group, data = df)
summary(anova_results) # A low p-value of 8.23e-6 is provided

# Perform Tukey-Kramer post-hoc test
tukey_results <- TukeyHSD(anova_results)
print(tukey_results) # p<0.05 for comparing the two drugs

# But a t-test (without any multiple testing correction) is >0.05.
t_test_results <- t.test(Value ~ Group, 
                         data = subset(df, Group %in% c("Drug 1", "Drug 2")), 
                         var.equal = TRUE)
print(t_test_results) 

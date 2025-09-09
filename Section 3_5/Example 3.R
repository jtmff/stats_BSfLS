###############################################################################
## EXAMPLE 3: Dunnett's test
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(DescTools)
set.seed(124)

# Generate data for 5 groups
nPoints <- 12
ctrl <- rnorm(nPoints, mean = 0, sd = 10)
drug1 <- rnorm(nPoints, mean = 5, sd = 10)
drug2 <- rnorm(nPoints, mean = 13, sd = 10)
drug3 <- rnorm(nPoints, mean = 8, sd = 10)
drug4 <- rnorm(nPoints, mean = 15, sd = 10)

# Combine the data into a dataframe
df <- data.frame(
  Group = rep(c("Control", "Drug 1", "Drug 2", "Drug 3", "Drug 4"), each = nPoints),
  Value = c(ctrl, drug1, drug2, drug3, drug4),
  X = rep(c(1, 2, 3, 4, 5), each = nPoints)
)

# Perform Dunnett's test
DunnettTest(x=df$Value, g=df$Group)

# For comparison, perform ANOVA and Tukey-Kramer post-hoc test
anova_results <- aov(Value ~ Group, data = df)
tukey_results <- TukeyHSD(anova_results)
print(tukey_results)
# Notice how Drug 2-Control became (very weakly) non-significant at the 0.05 
# level, while its p-value was <0.05. This stems from ANOVA's more extensive
# correction for multiple testing, which accounts for more comparisons and is
# therefore more stringent. If you care only about drug vs control comparisons,
# this extra stringency is unnecessary and limits statistical power.
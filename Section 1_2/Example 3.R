###############################################################################
## EXAMPLE 3: Plotting data with a bargraph
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggplot2)  
library(Cairo)
# The same data as in the previous example are generated
dataA <- c(171.57, 191.66, 167.33, 201.35, 174.83, 178.04, 181.84, 175.55)
dataB <- c(145.71, 148.38, 169.11, 169.76, 144.23, 162.04, 141.11, 167.39)

# Calculate mean and standard deviation for each group
group1_mean <- mean(dataA)
group1_sd <- sd(dataA)
group2_mean <- mean(dataB)
group2_sd <- sd(dataB)

data_for_bargraph <- data.frame(
  Group = factor(rep(c("A", "B"), each = 1)),
  Mean = c(group1_mean, group2_mean),
  SD = c(group1_sd, group2_sd)
)

plt <- ggplot(data_for_bargraph, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.4) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  labs(x = "Group", y = "Weight (g)")+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24)
  )
  

CairoPNG(file = "bar.png", width = 600, height = 400)
print(plt)
dev.off()
###############################################################################
# EXAMPLE 7: Scatterplot
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggplot2)
library(Cairo)

# Generating synthetic data (linear relationship with added noise)
set.seed(69)  # for reproducibility
x <- 1:50
y <- 2*x + rnorm(50, mean = 0, sd = 20)  # Noisy linear relationship: y = 2*x + noise

# Create a data frame
df <- data.frame(x = x, y = y)

# Plot scatter plot
plt <- ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 3, alpha = 0.5) +  # Add points
  labs(x = "Variable 1", y = "Variable 2") +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24))

CairoPNG(file = "scatter.png", width = 600, height = 400)
print(plt)
dev.off()

# By adding few lines to the theme, it is possible to also show axes and ticks.
plt_axes <- ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 3, alpha = 0.5) +  # Add points
  labs(x = "Variable 1", y = "Variable 2") +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.ticks = element_line(color = "black"),  
        axis.ticks.length = unit(15, "pt"),
        axis.line.x = element_line(color = "black", size = 1.5),  # Bottom axis 
        axis.line.y = element_line(color = "black", size = 1.5))   # Left axis 

CairoPNG(file = "scatter_with_axes.png", width = 600, height = 400)
print(plt_axes)
dev.off()


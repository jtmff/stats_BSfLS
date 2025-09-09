###############################################################################
## EXAMPLE 1: Calculation of various correlation coefficient
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggplot2)
library(Cairo)

set.seed(333)

# Generate equidistant x-axis values between 1 and 10
n <- 100
x <- seq(1, 10, length.out = n)

# Generate linear-shaped data with noise
y_linear <- 2 * x + rnorm(n, mean = 0, sd = 5) 

# Generate exponential-shaped data with noise
y_nonlinear <- exp(x) + rnorm(n, mean = 0, sd = 100)  

# Calculate correlation coefficients for linear and nonlinear data
linear_pearson <- cor(x, y_linear, method = "pearson")
linear_spearman <- cor(x, y_linear, method = "spearman")
linear_kendall <- cor(x, y_linear, method = "kendall")

# For the nonlinear data, notice how Pearson correlation becomes
# weaker than for the other two.
nonlinear_pearson <- cor(x, y_nonlinear, method = "pearson")
nonlinear_spearman <- cor(x, y_nonlinear, method = "spearman")
nonlinear_kendall <- cor(x, y_nonlinear, method = "kendall")

## Plotting the data with correlation coefficients in the title
# Creating the plot titles
linear_title <- sprintf(
  "Linear data\nPearson: %.2f, Spearman: %.2f, Kendall: %.2f",
  linear_pearson, linear_spearman, linear_kendall
)

nonlinear_title <- sprintf(
  "Exponential data\nPearson: %.2f, Spearman: %.2f, Kendall: %.2f",
  nonlinear_pearson, nonlinear_spearman, nonlinear_kendall
)

# Theme for plots
custom_theme <- theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.text.x = element_text(size = 24),
  axis.text.y = element_text(size = 24),
  axis.title.x = element_text(size = 27),
  axis.title.y = element_text(size = 27)
)

# Plot linear data
linear_plot <- ggplot(data.frame(x = x, y = y_linear), aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.5) +
  ggtitle(linear_title) +
  theme_minimal()+
  custom_theme


# Plot nonlinear data
nonlinear_plot <- ggplot(data.frame(x = x, y = y_nonlinear), aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.5) +
  ggtitle(nonlinear_title) +
  theme_minimal()+
  custom_theme

# Save plots using Cairo
CairoPNG(filename = "linear_plot.png", width = 800, height = 600)
print(linear_plot)
dev.off()

CairoPNG(filename = "nonlinear_plot.png", width = 800, height = 600)
print(nonlinear_plot)
dev.off()






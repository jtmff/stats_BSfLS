###############################################################################
## EXAMPLE 2: Linear regression fitting to linear data, inspection of residuals, and a coefficient test
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# Load necessary libraries, set seed, and define theme for plots.
library(ggplot2)
library(Cairo)
set.seed(333)

# Theme for plots
custom_theme <- theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.text.x = element_text(size = 24),
  axis.text.y = element_text(size = 24),
  axis.title.x = element_text(size = 27),
  axis.title.y = element_text(size = 27)
)

# Generate equidistant x values between 1 and 10
n <- 100
x <- seq(1, 10, length.out = n)

# Generate noisy linear data
y_linear <- 2 * x + rnorm(n, mean = 0, sd = 5)  # Add noise

# Fit linear regresion model
linear_model <- lm(y_linear ~ x)

# Calculate residuals
linear_residuals <- residuals(linear_model)

# Like this, you can print various information on the properties
# of the linear model fit to the linear data
print(summary(linear_model))
# Or you can store and print just the p-value like this
p_value_slope <- summary(linear_model)$coefficients["x", "Pr(>|t|)"]
cat("P-value for the slope of x:", p_value_slope, "\n")

# Linear fit to linear data
linear_fit_plot <- ggplot(data.frame(x = x, y = y_linear), aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("X") + ylab("Y") +
  theme_minimal()+
  custom_theme

# Residuals of linear data fit by a line.
# These look appropriate for linear regression, being ~randomly spread.
linear_residuals_plot <- ggplot(data.frame(x = x, residuals = linear_residuals), 
                                aes(x = x, y = residuals)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  xlab("X") + ylab("Residuals") +
  theme_minimal()+
  custom_theme

# Saving plots
CairoPNG(filename = "linear_fit_plot.png", width = 800, height = 600)
print(linear_fit_plot)
dev.off()

CairoPNG(filename = "linear_residuals_plot.png", width = 800, height = 600)
print(linear_residuals_plot)
dev.off()
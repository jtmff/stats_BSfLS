###############################################################################
## EXAMPLE 3: Linear regression (problematically) fit to nonlinear data
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggplot2)
library(Cairo)

# Theme for plots
custom_theme <- theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.text.x = element_text(size = 24),
  axis.text.y = element_text(size = 24),
  axis.title.x = element_text(size = 27),
  axis.title.y = element_text(size = 27)
)

# Generate noisy exponential data
set.seed(334)
y_nonlinear <- exp(x) + rnorm(n, mean = 0, sd = 100)  # Add noise

# Fitting linear model and calculating residuals
exp_model <- lm(y_nonlinear ~ x)
exp_residuals <- residuals(exp_model)

# Plotting linear fit to nonlinear (exponential) data
exp_fit_plot <- ggplot(data.frame(x = x, y = y_nonlinear), aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("X") + ylab("Y") +
  theme_minimal()+
  custom_theme

# Plotting residuals of exponential data fit by a line
# These clearly indicate the data are not fit well by a linear model.
exp_residuals_plot <- ggplot(data.frame(x = x, residuals = exp_residuals), 
                             aes(x = x, y = residuals)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  xlab("X") + ylab("Residuals") +
  theme_minimal()+
  custom_theme

# Saving the plots
CairoPNG(filename = "exp_fit_plot.png", width = 800, height = 600)
print(exp_fit_plot)
dev.off()

CairoPNG(filename = "exp_residuals_plot.png", width = 800, height = 600)
print(exp_residuals_plot)
dev.off()

###############################################################################
## EXAMPLE 4: Fitting exponantial decay curve to  nonlinear data
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggplot2)
library(Cairo)
library(minpack.lm)

# Theme for plots
custom_theme <- theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.text.x = element_text(size = 24),
  axis.text.y = element_text(size = 24),
  axis.title.x = element_text(size = 27),
  axis.title.y = element_text(size = 27)
)

# Same exponential data as before are generated.
set.seed(334)
n <- 100  # Number of points
x <- seq(1, 10, length.out = n)  # Independent variable
y_nonlinear <- exp(x) + rnorm(n, mean = 0, sd = 100)  # Add noise

# Fitting nonlinear model and calculating residuals
# nls is used instead of lm, and the equation can be provided as below.
# start is a list of starting guessstimates that you need to provide.
# You can guess the approximate values from the shape of your data.
exp_model <- nlsLM(y_nonlinear ~ a * exp(b * x), start = list(a = 1, b = 0.5))
exp_residuals <- residuals(exp_model)

# Extract fitted values
fitted_values <- fitted(exp_model)

# Plotting nonlinear fit to data
exp_fit_plot <- ggplot(data.frame(x = x, y = y_nonlinear, fitted = fitted_values),
                       aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_line(aes(y = fitted), color = "red", size = 1) +  # Nonlinear fit
  xlab("X") + ylab("Y") +
  theme_minimal() +
  custom_theme

# Plotting residuals of nonlinear fit
# Now they are again appropriately distributed
exp_residuals_plot <- ggplot(data.frame(x = x, residuals = exp_residuals), 
                             aes(x = x, y = residuals)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  xlab("X") + ylab("Residuals") +
  theme_minimal() +
  custom_theme

# Saving the plots
CairoPNG(filename = "exp_fit_nlim_plot.png", width = 800, height = 600)
print(exp_fit_plot)
dev.off()

CairoPNG(filename = "exp_residuals_nlim_plot.png", width = 800, height = 600)
print(exp_residuals_plot)
dev.off()

###############################################################################
## EXAMPLE 5: Comparing parameters of groups of sigmoids (dose-dependence)
###############################################################################
# This example shows how to fit a set of sigmoids to data
# in each experimental group, where each cell in a group produced
# data at each of the concentrations tested. The code reproduces
# the comparison in Figure 3.16. Do not be put off by the length
# of the code - the majority is used for creating data and plotting outputs.
# The fitting itself is simple

library(ggplot2)
library(scales)
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

set.seed(125)

# Defining the sigmoid function, with x being the list of x-axis
# values where it is evaluated, k is the midpoint, and slope indicates
# how steeply the sigmoid rises around the midpoint.
sigmoid <- function(x, k, slope) {
  1/ (1 + (k / x)^slope)
}

# 1) First, synthetic data are created at a range of concentrations,
# using sigmoids with varying parameters.

# x values corresponding to concentrations in nM
x <- c(1, 10, 100, 1000, 5000, 10000, 50000, 100000, 500000)

# a finely sampled x-axis vector is also generated between minimum
# and maximum concentration - this serves to produce a smoother-looking
# plot (feel free to replace the right hand side with x to see why).
x_smooth <- seq(min(x), max(x), length.out = 50000)  

# Define parameters (k, slope) for the two groups
group1_k <- c(500,700, 800, 900)
group2_k <- c(1800, 2200, 2500, 2600)
group1_slope <- c(1.6, 1.8, 2.2, 1.9)
group2_slope <- c(1.7, 1.75, 1.7, 2.2)

# Data points of (sigmoidal) measurements at concentrations x
measurements <- data.frame()
n_curves <- 4
for (i in 1:n_curves) {
  # Generate data using the sigmoid function and store them in a data frame
  data_cell_G1 <- sigmoid(x, group1_k[i], group1_slope[i])
  data_cell_G2 <- sigmoid(x, group2_k[i], group2_slope[i])

  measurements <- rbind(measurements, data.frame(x = x, y = data_cell_G1,
                                                 group = "Drug 1", 
                                                 id = paste("Sigmoid", i), 
                                                 cell_id = i))
  measurements <- rbind(measurements, data.frame(x = x, y = data_cell_G2, 
                                                 group = "Drug 2", 
                                                 id = paste("Sigmoid", i + 4), 
                                                 cell_id = i + 4))
}

# 2) Now, we have sample data generated, and we can fit curves to them.

# Data frames are created to store curves fitted to data and their parameters.
curve_data <- data.frame()
fit_group1 <- data.frame()
fit_group2 <- data.frame()

n_curves_per_group <- 4
for (i in 1:n_curves_per_group) {
  # Data for an i-th cell from either group
  y1 <- measurements[measurements$cell_id == i,]$y
  y2 <- measurements[measurements$cell_id == i+n_curves_per_group,]$y 
  
  # Fit sigmoid curves the data (nlsLM is used, where lm usually would be)
  # In the first parameter, you specify which function is fitted to the data.
  # start contains a guesstimate of the parameters that you can roughly estimate
  # from the data by eyeballing. 
  fit1 <- nlsLM(y1 ~ sigmoid(x, k, slope), start = list(k = 2000, slope = 2))
  fit2 <- nlsLM(y2 ~ sigmoid(x, k, slope), start = list(k = 2000, slope = 2))
  
  # Extract fitted parameters (coefficients) of the functions
  params1 <- coef(fit1)
  params2 <- coef(fit2)
  
  # Save the parameters in separate data frames for each group
  fit_group1 <- rbind(fit_group1, data.frame(id = paste("Sigmoid", i),
                            group = "Drug 1", k = params1['k'], 
                            slope = params1['slope']))
  fit_group2 <- rbind(fit_group2, data.frame(id = paste("Sigmoid", i+4),
                             group = "Drug 2", k = params2['k'], 
                             slope = params2['slope']))
  
  # Generate smooth sigmoid curves based on the fitted parameters
  # Here x_smooth is used to produce smoother-looking curve.
  curve_y1 <- sigmoid(x_smooth, params1['k'], params1['slope'])
  curve_y2 <- sigmoid(x_smooth, params2['k'], params2['slope'])
  
  curve_data <- rbind(curve_data, data.frame(x = x_smooth, y = curve_y1, 
                      group = "Drug 1", id = paste("Sigmoid", i)))
  curve_data <- rbind(curve_data, data.frame(x = x_smooth, y = curve_y2, 
                      group = "Drug 2", id = paste("Sigmoid", i + 4)))
}

# 3) We can now plot the fitted sigmoids over the original data.
# A list of codes for shapes to be used for data points,
# so that it is clear which data point comes from which cell.
# This may be impractical for larger amounts of data from many cells,
# where one runs out of symbols, or the figure is not legible.
# There, feel free to just use a single symbol throughout. 
symbols <- c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

sig_comparison <- ggplot(measurements, aes(x = x/ 1000, y = y, 
                                           shape = as.factor(cell_id),
                                           color = group)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line(data = curve_data, aes(x = x/ 1000, y = y, color = group, linetype = id),
            size = 1, inherit.aes = FALSE) +  # Add sigmoid curves
  scale_shape_manual(values = symbols) +
  scale_x_log10(labels = label_number(scale = 1, big.mark = "")) +
  labs(x = "Concentration (µM)", y = "Channel block", shape = "Sigmoid ID", 
       color = "Group", linetype = "Sigmoid Curve") +
  theme_minimal() +
  custom_theme


# Saving the plot
CairoPNG(file = "comparing_sigmoids.png", width = 600, height = 600)
print(sig_comparison)
dev.off()

# Print the fitted parameters for each group
# You can see they perfectly correspond to the parameters we used
# to create the data in the first place. That is because there is
# no noise - if you add noise to the data, the parameters of fitted
# curves will differ slightly from the true values.

print("Fitted parameters for Group 1:")
print(fit_group1)

print("Fitted parameters for Group 2:")
print(fit_group2)

# Running t-tests on the midpoint parameters (k) and slope.
t_test_k <- t.test(fit_group1$k, fit_group2$k)
t_test_slope <- t.test(fit_group1$slope, fit_group2$slope)

cat("p-value for midpoint: ", t_test_k$p.value,"\n")
cat("p-value for slope: ", t_test_slope$p.value, "\n")
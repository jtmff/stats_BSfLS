###############################################################################
## EXAMPLE 3: A permutation test to compare independent dose-dependent data. 
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# This shows how to use a permutation test for data such as shown in Figure 3.22G,
# a comparison of two groups of "curve" data, where data points are independent
# (i.e., not repeated measures from one cell across concentrations), measured
# at a range of concentrations.

library(minpack.lm) # for nlsLM
library(dplyr) # for %>% and associated operations

set.seed(222)

# A function which generates simulated data representing sigmoidal 
# concentration-dependent behaviour at a range of concentrations.
# The different data points are assumed to be independent
make_synthetic_data <- function(x, IC50, Hill, IC50_sd, Hill_sd){
  # x - a vector of input concentrations for which y values are to be generated
  # IC50 - baseline IC50 for the generated data
  # Hill - baseline Hill coefficient for the generated data
  # IC50_sd - the baseline IC50 is perturbed by adding a normally distributed
  #           number with zero mean and this standard deviation
  # Hill_sd - analogous, for Hill coefficient
  
  # The perturbed IC50 and Hill parameters are generated. Note that the size of
  # the vector is the same as the length of input x - i.e., each input point
  # is simulated using a distinct IC50 and Hill. This is in contrast with 
  # generation of synthetic data for a repeated-measures design, where a sequence
  # of values would be generated within each cell using the same IC50 and Hill.
  IC50_perturbed <- IC50+rnorm(n=length(x), mean=0, sd=IC50_sd) 
  Hill_perturbed <- Hill+rnorm(n=length(x), mean=0, sd=Hill_sd)
  
  return(1/ (1 + (IC50_perturbed / x)^Hill_perturbed))
}

# Permutation test function
perm_test_ICshift <- function(df1, df2, diff_observed, n_repeats = 10000){
  # df1, df2 - data frames representing measurements in two distinct conditions
  # diff_observed - the difference between mean IC50s observed in tested data
  # n_repeats - number of times data are shuffled
  
  diffs_permuted <- rep(0, n_repeats)
  
  for (i in 1:n_repeats) {
    # Permute the data, so that data within the same concentration are shuffled 
    # between the groups.
    shuffled <- shuffle_data_within_concentrations(df1, df2)
    
    # fit a sigmoid to each group's data, extract IC50 difference 
    fm <- as.formula("y ~ 1/(1+ (IC50/x)^Hill)")
    m1 <- nlsLM(fm, data=shuffled$df1_out, start=list(IC50=2000, Hill=2))
    m2 <- nlsLM(fm, data=shuffled$df2_out, start=list(IC50=2000, Hill=2))
    diffs_permuted[i] <- as.numeric(m2$m$getPars()[1] - m1$m$getPars()[1])
  }
  
  # p-value calculation
  return((sum(abs(diffs_permuted) >= abs(diff_observed))+1)/(n_repeats+1))
}

# Function to take two groups of dose-response data and shuffle the responses 
# in a way where only data within the same concentration are permuted.
shuffle_data_within_concentrations <- function(df1, df2){
  # df1, df2 - data frames representing measurements in two distinct conditions
  
  df_H0 <- rbind(df1, df2)
  
  # One way of shuffling data between conditions only within each concentration
  df_shuffled <- df_H0 %>%
    group_by(x) %>%  # Group by concentration
    mutate(y = sample(y))  # Shuffle responses within each group
  
  return(list(df1_out = df_shuffled[1:nrow(df1),], 
              df2_out=df_shuffled[(nrow(df1)+1):(nrow(df1)+nrow(df2)),]))
}

concentrations <- c(10, 100, 1000, 5000, 10000, 50000, 100000)
n_per_group_per_concentration <- 10

# Create simulated data, x (concentrations) and y (responses)
x1 <- rep(concentrations, times = n_per_group_per_concentration)
x2 <- x1

# Variation is obtained in this case not through adding random noise to outputs 
# of fixed-parameter sigmoids (as would be the case with measurement noise), 
# but rather by randomly perturbing midpoints and slopes of the sigmoids 
# (representing variability in how different cells respond).
# It is important to be not too wrong when simulating your data; if they are 
# unrepresentative, so will be the outputs. 
# Do talk to a statistician if you are not sure.
y1 <- make_synthetic_data(x1, 2000, 2, 400, 0.2)
y2 <- make_synthetic_data(x2, 1500, 2, 400, 0.2)

df1 <- data.frame(y = y1, x = x1, group = "Group 1")
df2 <- data.frame(y = y2, x = x2, group = "Group 2")

# Fit a sigmoid to the observations in each data group.
fm <- as.formula("y ~ 1/(1+ (IC50/x)^Hill)")
m1 <- nlsLM(fm, data=df1, start=list(IC50=2000, Hill=2))
m2 <- nlsLM(fm, data=df2, start=list(IC50=2000, Hill=2))
# Record the observed difference in IC50.
diff_observed <- m2$m$getPars()[1] - m1$m$getPars()[1]
print(diff_observed)

# Using the permutation test to calculate a p-value.
pval <- perm_test_ICshift(df1, df2, diff_observed)
print(pval) # 0.0042
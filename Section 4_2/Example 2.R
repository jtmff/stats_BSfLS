###############################################################################
## EXAMPLE 2: Simulation-based power calculation for a permutation test
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# Simulation-based power calculation can take a long time to run with permutation 
# tests, which themselves take some time to calculate. Here we show how to use a 
# parallel for loop, which utilizes multiple cores on your CPU to accelerate the
# computation (you need to set num_cores to at most as many as you have available,
# and you may want to use fewer, so that there is spare capacity for other tasks). 
# The script repeatedly generates synthetic data for two dose-response data groups 
# that differ in their midpoint (IC50) by 300: 2000 vs 1700. For each such pair 
# of groups, it runs a permutation test, calculating in what proportion of cases 
# the test gives p < 0.05, detecting the difference.

# packages for model fitting, shuffling, and parallel processing
library(minpack.lm)
library(dplyr)
library(foreach)
library(doParallel)

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


# Change this to how many cores you have or few less
num_cores <- 15
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# For us, the code runs in few hours with 15 cores and 10k repeats
# Feel free to adjust to your computer.
n_repeats = 10000

# This is how to run a for loop in parallel 
pvals <- foreach(i_repeat = 1:n_repeats, .combine = 'c', .packages 
                 = c("stats", "dplyr", "minpack.lm")) %dopar% {
  
  concentrations <- c(10, 100, 1000, 5000, 10000, 50000, 100000)
  n_per_group_per_concentration <- 10
  
  # Create data
  x1 <- rep(concentrations, times = n_per_group_per_concentration)
  x2 <- rep(concentrations, times = n_per_group_per_concentration)
  
  # Simulated observations are defined here - feel free to change the 
  # true underlying IC50s here. If you make the IC50 2000 in both groups,
  # the null hypothesis holds and this script thus serves to confirm that
  # the proportion of tests with p<0.05 will be around 5%. If you in turn
  # change the 1700 to 1500, you will see a higher power (more tests with p<0.05).
  y1 <- make_synthetic_data(x1, 2000, 2, 400, 0.2)
  y2 <- make_synthetic_data(x2, 1700, 2, 400, 0.2)
  
  df1 <- data.frame(y = y1, x = x1)
  df2 <- data.frame(y = y2, x = x2)
  
  # Fit models
  fm <- as.formula("y ~ 1/(1+ (IC50/x)^Hill)")
  m1 <- nlsLM(fm, data=df1, start=list(IC50=2000, Hill=2))
  m2 <- nlsLM(fm, data=df2, start=list(IC50=2000, Hill=2))
  # Estimate difference in IC50s of the shuffled data
  stat_diffIC50 <- m2$m$getPars()[1] - m1$m$getPars()[1]
  
  # Compute p-value
  pval <- perm_test_ICshift(df1, df2, stat_diffIC50, 1000)
  
  return(pval)  # Return the p-value for each iteration
  
}

# Stop the parallel cluster
stopCluster(cl)

# Proportion of significant p-values at the 0.05 level
print(mean(unlist(pvals)<0.05, na.rm=TRUE))
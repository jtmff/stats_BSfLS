###############################################################################
## EXAMPLE 1: Power calculation for variants of a t-test.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# The function power.t.test() can be used to address questions related to study
# power. Below are multiple examples of how it can be used.

# What is the power of an unpaired (two-sample) t-test with n=10 in each
# group with standard deviation of 2, for detecting a difference of 1 in means?
power.t.test(n = 10, delta = 1, sd = 2, sig.level = 0.05, type = "two.sample", 
             alternative = "two.sided") 
# Fairly low power of ca. 18%

# By changing the first parameter from n to power, you can instead estimate the
# required n to achieve the required power (scaled between 0 and 1).
power.t.test(power = 0.8, delta = 1, sd = 2, sig.level = 0.05, 
             type = "two.sample", alternative = "two.sided")
# n=64 is needed

# By changing the type from two sample to paired, 
# you can obtain power for a paired t-test.
power.t.test(n = 20, delta = 1, sd = 2, sig.level = 0.05, type = "paired", 
             alternative = "two.sided")
# with n=20, the power is ca. 56%.

# The function power.t.test does not accommodate for unequal group size, nor for 
# the Welch t-test. If you want to obtain power for such scenario, you can either
# use other functions (e.g. power.welch.t.test from the MKmisc package),
# or you can use simulations of your study to assess the power.

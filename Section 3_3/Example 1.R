###############################################################################
## EXAMPLE 1: How to conduct various tests for contingency tables.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################

# Load the exact2x2 package, which contains the functions for the tests.
library(exact2x2)

# Example of a 2x2 contingency table
# It can represent, e.g. 12 diseased and 12 healthy animals, 
# with 8 of the diseased showing liver failure, and 4 not, whereas
# among the healthy animals, only 1 case of liver failure was observed
# and 11 animals were fine.
ct <- matrix(c(8, 4, 1, 11), nrow = 2, byrow = TRUE, dimnames=
               list(c("Healthy","Diseased"),c("Liver failure","No failure")))
print(ct)

# Or uncomment the following table, which does not indicate there is a clear 
# difference between the two groups:
# ct <- matrix(c(7, 5, 5, 7), nrow = 2, byrow = TRUE, dimnames=
#             list(c("Healthy","Diseased"),c("Liver failure","No failure")))
# print(ct)

# Fisher's test
fisher_test <- fisher.test(ct)
cat("Fisher's test p-value:", fisher_test$p.value, "\n")

# Lancaster's mid-p
lancaster_test <- exact2x2(ct, midp = TRUE)
cat("Lancaster's mid-p p-value:", lancaster_test$p.value, "\n")

# Boschloo's test
# Here the information on event frequencies is passed in the form of 4 numbers,
# rather than one table. And, rather than a contingency table 
# A B 
# C D
# one inputs A, A+B, C, C+D (i.e., A out of A+B events, and C out of C+D happened).

boschloo_test <- boschloo(ct[1,1],ct[1,1]+ct[1,2], 
                          ct[2,1],ct[2,1]+ct[2,2])
cat("Boschloo's test p-value:", boschloo_test$p.value, "\n")

# Chi-square 
chisq_test <- chisq.test(ct)


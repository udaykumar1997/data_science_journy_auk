# IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.

########################################################################
## Section 5: Bayesian Statistics
########################################################################

# lecture 1 doesn't have any code
# lecture 2
# Code: Monte Carlo simulation
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))
mean(outcome=="Disease")
outcome <- sample(c("Disease", "Healthy"), N)

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

# lecture 3 doesn't have any code
# lecture 4 doesn't have any code
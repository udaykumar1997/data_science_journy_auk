# IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.

########################################################################
## Section 5: Bayesian Statistics
########################################################################

# lecture 1 doesn't have any code
# refer to https://rafalab.github.io/dsbook/models.html#bayesian-statistics

# lecture 2
# refer to https://rafalab.github.io/dsbook/models.html#bayes-theorem

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
# refer to https://rafalab.github.io/dsbook/models.html#bayes-in-practice

# lecture 4 doesn't have any code
# refer to https://rafalab.github.io/dsbook/models.html#hierarchical-models

########################################################################
# exercise
########################################################################

# q2
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. Print this value to the console.
Pr_2 * Pr_1

# q3
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB <- ( Pr_BA * Pr_A ) / Pr_B
Pr_AB

# q6
# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
length(polls)
nrow(polls)
ncol(polls)

avg <- mean(polls$spread)

N <- length(polls$spread)
se <- sd(polls$spread)/sqrt(N)

results <- polls %>% summarize(avg = mean(spread), sd = sd(spread))
results <- summarize(data = polls, avg = mean(spread), sd = sd(spread)) # doesn't work

results <- polls %>% summarize(avg = mean(spread), sd = sd(polls$spread)/sqrt(N)) # correct

# q8
# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results$se
# str(results)
# str(sigma)

# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
#B <- ( (sigma^2) / ( (sigma^2) * (tau^2) ) )
B <- sigma^2 / (sigma^2 * tau^2)

# Calculate the expected value of the posterior distribution
B*mu + ((1-B)*Y)

# q8 correct ans
# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results$se

# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <- sigma^2 / (sigma^2 + tau^2)
B

# Calculate the expected value of the posterior distribution
B*mu + (1-B)*Y

# q9
# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

# Compute the standard error of the posterior distribution. Print this value to the console.
sePD <- sqrt(1/( ( 1/sigma^2 ) + ( 1/tau^2 ) ))
sePD

# q10
# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(Y - qnorm(0.975)*sigma, Y + qnorm(0.975)*sigma) # wrong ans
ci <- c(Y - qnorm(0.975)*se, Y + qnorm(0.975)*se) # wrong ans
ci <- c(B - qnorm(0.975)*sigma, B + qnorm(0.975)*sigma)
ci <- c(B - qnorm(0.975)*se, B + qnorm(0.975)*se) # wrong ans

# q10 correct ans
# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- B*mu + (1-B)*Y + c(-1,1)*qnorm(0.975)*sqrt( 1/ (1/sigma^2 + 1/tau^2))

# q11
# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)

# q12
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
results <- polls %>% summarize(avg = mean(spread), sd = sd(spread))

# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0

p_calc <- function(tau){
  B <- sigma^2 / (sigma^2 + tau^2)
  exp_value <- B*mu + (1-B)*Y 
  se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
  pnorm(0, exp_value, se) 
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- sapply(taus, p_calc)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus,ps) #correct ans

ncol(ps)
nrow(ps)
ncol(taus)
nrow(taus)
length(ps)
length(taus)
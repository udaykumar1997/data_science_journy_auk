# IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.

########################################################################
## 4.1 The Big Short: Interest Rates Explained
########################################################################

# Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
    defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
    sum(defaults * loss_per_foreclosure)
})
mean(losses)

# Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
    ggplot(aes(losses_in_millions)) +
    geom_histogram(binwidth = 0.6, col = "black")

# Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

# We solve for x=???lp1???p and calculate x:
x = - loss_per_foreclosure*p/(1-p)
x
# On a $180,000 loan, this equals an interest rate of:
x/180000

# Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))/x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

# Code: Monte Carlo simulation for 1% probability of losing money
# Note that your results will vary from the video because the seed is not set.

B <- 100000
profit <- replicate(B, {
    draws <- sample( c(x, loss_per_foreclosure), n, 
                     prob=c(1-p, p), replace = TRUE)
    sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

# Code: Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

# Code: Calculating number of loans for desired probability of losing money
# The number of loans required is:

z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

# Code: Monte Carlo simulation with known default probability
# This Monte Carlo simulation estimates the expected profit given a known probability of default p=0.04. Note that your results will differ from the video because the seed is not set.

B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
    draws <- sample( c(x, loss_per_foreclosure), n, 
                     prob=c(1-p, p), replace = TRUE) 
    sum(draws)
})
mean(profit)

# Code: Monte Carlo simulation with unknown default probability
# This Monte Carlo simulation estimates the expected profit given an unknown probability of default 0.03???p???0.05, modeling the situation where an event changes the probability of default for all borrowers simultaneously. Note that your results will differ from the video because the seed is not set.

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
    new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
    draws <- sample( c(x, loss_per_foreclosure), n, 
                     prob=c(1-new_p, new_p), replace = TRUE)
    sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

########################################################################
## exercise
########################################################################
# 1
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample(c(0,1),n,c(1-p_default,p_default),replace = TRUE)

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- sum(defaults)*loss_per_foreclosure
S

# 2
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans.  Ignore any warnings for now.

S<- replicate(B, {
    defaults <- sample( c(0,1), n, prob=c(1-p_default, p_default), replace = TRUE) 
    sum(defaults * loss_per_foreclosure)
})

# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)

# 4
# Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

# 5
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x <- 0
p_default*loss_per_foreclosure + (1-p_default)*x
x <- (0 - (p_default*loss_per_foreclosure))/(1-p_default)

# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.
x/180000

# 6
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Generate a variable `z` using the `qnorm` function
z <- qnorm(0.05)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure*( n*p_default - z*sqrt(n*p_default*(1 - p_default)))/ ( n*(1 - p_default) + z*sqrt(n*p_default*(1 - p_default)))

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x / 180000

########################################################################
## Final exercise
########################################################################

library(dslabs)
data("death_prob")

str(death_prob)
death_prob$prob[age=50]
death_prob$sex[death_prob$age==50]
death_prob$sex[sex=='Female']

death_prob[death_prob$age==50]

# 1a
death_prob$prob[death_prob$age==50]
death_prob$sex[death_prob$age==50]

# 1b
p<-0.003193
loss_per_claim<- -150000
profit_per_healthy_policy<-1150
n<-1
ev <- n*(loss_per_claim*p)+(profit_per_healthy_policy*(1-p))
ev

# 1c
se <- sqrt(n) * (abs(loss_per_claim - profit_per_healthy_policy))*sqrt(p*(1-p)) #wrong
se

# 1d
n<-1000
ev <- n*((loss_per_claim*p)+(profit_per_healthy_policy*(1-p)))
ev

# 1e
# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

se <- sqrt(n) * (abs(loss_per_claim - profit_per_healthy_policy))*sqrt(p*(1-p))
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
se

# 1f
B <- 1000000
n<-1000
p <- 0.003193
profit <- replicate(B, {
    draws <- sample( c(profit_per_healthy_policy, loss_per_claim), n, 
                     prob=c(1-p, p), replace = TRUE) 
    sum(draws)
})
mean(profit<0) # wrong answer

ev <- n*((profit_per_healthy_policy*(1-p))+(loss_per_claim*p))
se <- sqrt(n) * abs(profit_per_healthy_policy-loss_per_claim) * sqrt(p*(1-p))

abs(profit_per_healthy_policy - loss_per_claim)
abs(loss_per_claim - profit_per_healthy_policy)

pnorm(0, ev, se)

# 2a
death_prob$prob[death_prob$age==50]
death_prob$sex[death_prob$age==50]
p<-0.005013

# 2b
ev <- 700000
ev <- n*((profit_per_healthy_policy*(1-p))+(loss_per_claim*p))
profit_per_healthy_policy <- ((ev/n)-(loss_per_claim*p))/(1-p)
profit_per_healthy_policy

# 2c
se <- sqrt(n) * abs(profit_per_healthy_policy-loss_per_claim) * sqrt(p*(1-p))
se

# 2d
pnorm(0, ev, se)

# 3a
p<-0.015
loss_per_claim<- -150000
profit_per_healthy_policy<-1150
n<-1000

ev <- n*((profit_per_healthy_policy*(1-p))+(loss_per_claim*p))
ev

# 3b
se <- sqrt(n) * abs(profit_per_healthy_policy-loss_per_claim) * sqrt(p*(1-p))
se

# 3c
pnorm(0, ev, se)

# 3d
pnorm(1000000, ev, se)

# 3e
p <- seq(.01, .03, .001)

probabilityOfLosingMoney <- function(p) {
    ev <- n*((profit_per_healthy_policy*(1-p))+(loss_per_claim*p))
    se <- sqrt(n) * abs(profit_per_healthy_policy-loss_per_claim) * sqrt(p*(1-p))
    pnorm(0, ev, se)
}

prob <- sapply(p, probabilityOfLosingMoney)    # element-wise application of compute_prob to n
plot(p, prob)
prob[prob>0.9]
p[prob>0.9]

# 3f
p <- seq(.01, .03, .0025)
probabilityOfLosingMoney <- function(p) {
    ev <- n*((profit_per_healthy_policy*(1-p))+(loss_per_claim*p))
    se <- sqrt(n) * abs(profit_per_healthy_policy-loss_per_claim) * sqrt(p*(1-p))
    pnorm(-1000000, ev, se)
}

prob <- sapply(p, probabilityOfLosingMoney)    # element-wise application of compute_prob to n
plot(p, prob)
prob[prob>0.9]
p[prob>0.9]

# 4a
set.seed(25, sample.kind = "Rounding")

p<- 0.015
loss_per_claim<- -150000
profit_per_healthy_policy<- 1150
n<- 1000
ev <- n*((profit_per_healthy_policy*(1-p))+(loss_per_claim*p))
se <- sqrt(n) * abs(profit_per_healthy_policy-loss_per_claim) * sqrt(p*(1-p))

s <- sample(c(profit_per_healthy_policy,loss_per_claim),n,
       c((1-p),p), replace = TRUE)
sum(s)/(10^6)

# 4b
set.seed(27, sample.kind = "Rounding")
B<-10000
p<- 0.015
loss_per_claim<- -150000
profit_per_healthy_policy<- 1150
n<- 1000
same_day <- replicate(B, {
    s <- sample(c(profit_per_healthy_policy,loss_per_claim),n,
                c((1-p),p), replace = TRUE)
    sum(s)
})
mean(same_day < -1000000)

# 5a
p<- 0.015
loss_per_claim<- -150000
n<- 1000

ev <- ??
ev <- n*((profit_per_healthy_policy*(1-p))+(loss_per_claim*p))
profit_per_healthy_policy <- ((ev/n)-(loss_per_claim*p))/(1-p)
x <- profit_per_healthy_policy
profit_per_healthy_policy


a<- function(profit_per_healthy_policy){
    ev <- n*((profit_per_healthy_policy*(1-p))+(loss_per_claim*p))
    se <- sqrt(n) * abs(profit_per_healthy_policy-loss_per_claim) * sqrt(p*(1-p))
    print(pnorm(0, ev, se))
    print(pnorm(0, ev, se) < 0.05)
}

premiums<- seq((3268.06),3269,0.1)
#    c(3210)
probs <- sapply(premiums, a)
premiums[probs==0.05001081]

profit_per_healthy_policy <- 3268
x <- profit_per_healthy_policy

ev <- n*((profit_per_healthy_policy*(1-p))+(loss_per_claim*p))

# 5d
set.seed(28, sample.kind = "Rounding")
p<- 0.015
loss_per_claim<- -150000
profit_per_healthy_policy <- 3268
x <- profit_per_healthy_policy
n<- 1000
same_day <- replicate(B, {
    s <- sample(c(profit_per_healthy_policy,loss_per_claim),n,
                c((1-p),p), replace = TRUE)
    sum(s)
})
mean(same_day < 0)

# 6
set.seed(29, sample.kind = "Rounding")
B=10000
loss_per_claim<- -150000
profit_per_healthy_policy <- 3268
x <- profit_per_healthy_policy
n<- 1000
same_day <- replicate(B, {
    p<- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
    s <- sample(c(profit_per_healthy_policy,loss_per_claim),n,
                c((1-p),p), replace = TRUE)
    sum(s)
})
mean(same_day)
mean(same_day < 0)
mean(same_day < -1000000)
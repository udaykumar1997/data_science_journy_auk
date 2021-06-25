# IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.

########################################################################
## 3.1 Random Variables
########################################################################

# Code: Modeling a random variable
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

########################################################################
## 3.1 Sampling Models
########################################################################

# Monte Carlo simulation: Chance of casino losing money on roulette
# We build a sampling model for the random variable S that represents the casino's total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

# We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
    sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

# We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

########################################################################
## exercise
########################################################################

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

X <- sample(c(-1,17), n, replace = TRUE, prob = c(36/38,2/38))

# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green
17*p_green + -1*p_not_green

abs((17 - -1))*sqrt(p_green*p_not_green)

X <- sample(c(-1,17), 1000, replace = TRUE, prob = c(p_not_green,p_green))
S <- sum(X)

1000*(17*p_green + -1*p_not_green)

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1-pnorm(0, avg, se)

# 3
1/5
-0.25*(4/5) + 1*(1/5)
n<-44
c_ans<-1/5
in_ans<- 1-c_ans
se <- sqrt(n) * (1 - -0.25)*sqrt(c_ans*in_ans)

44*(-0.25*(in_ans) + 1*(c_ans))

n <- 44
X <- sample(c(-0.25,1), n, replace = TRUE, prob = c(in_ans,c_ans))
sum(X)

set.seed(21, sample.kind = "Rounding")
B<-10000
events <- replicate(B, 
                    sum(sample(c(-0.25,1), n, replace = TRUE, prob = c(in_ans,c_ans))))
mean(events>=8)

n<-44
c_ans<-1/4
in_ans<- 1-c_ans
44*(0*(in_ans) + 1*(c_ans))

p <- seq(0.25, 0.95, 0.05)
ev <- 44*(0*(1-p) + 1*(p))
ev>35

B<-10000
p <- seq(0.25, 0.95, 0.05)
length(p)
pp<-p[13]
events <- replicate(B, 
                    sum(sample(c(0,1), 44, replace = TRUE, prob = c(1-pp,pp))))
mean(events>=35)

#4
n<-1
c_ans<-5/38
in_ans<- 1-c_ans
se <- sqrt(n) * (6 - -1)*sqrt(c_ans*in_ans)
44*(-1*(in_ans) + 6*(c_ans))

n<-500
ev <- n*(-1*(in_ans) + 6*(c_ans))
se <- sqrt(n) * (6 - -1)*sqrt(c_ans*in_ans)

pnorm(0, ev, se)

n<-500
c_ans<-5/38
in_ans<- 1-c_ans
a<-sample(c(-1,6), n, replace = TRUE, prob = c(in_ans,c_ans))
mean(a) # wrong answer
sd(a)

B<-10000

events <- replicate(B, 
            mean(sample(c(-1,6), n, replace = TRUE, prob = c(in_ans,c_ans))))
mean(events) # wrong answer

c_ans<-5/38
in_ans<- 1-c_ans
n <- 500
B <- 10000
roulette_winnings <- function(n){
    X <- sample(c(-1,6), n, replace = TRUE, prob=c(c_ans, in_ans))
    mean(X)
}
S <- replicate(B, roulette_winnings(n))
mean(S)

set.seed(1, sample.kind = "Rounding")
B<-10000
n<-500
c_ans<-5/38
in_ans<- 1-c_ans
events <- replicate(B, 
                    mean(sample(c(-1,6), n, replace = TRUE, prob = c(in_ans,c_ans))))
mean(events)
set.seed(1, sample.kind = "Rounding")
sd(events)
se <- ((6 - -1)*sqrt(c_ans*in_ans)) / sqrt(n)
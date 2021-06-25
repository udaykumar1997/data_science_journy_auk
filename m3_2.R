# IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.

########################################################################
## 2.1 Continuous Probability
########################################################################

# Continuous Probability
# Code: Cumulative distribution function
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# Given a vector x, we can define a function for computing the CDF of x using:
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches


# Theoretical Distribution
# Code: Using pnorm() to calculate probabilities
# Given male heights x:
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# We can estimate the probability that a male is taller than 70.5 inches using:
1 - pnorm(70.5, mean(x), sd(x))

# Code: Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

data.frame(x, f = dnorm(x,1,4)) %>%
  ggplot(aes(x, f)) +
  geom_line()

# Monte Carlo Simulations
# Code: Generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

# Code: Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)

version
set.seed(x, sample.kind = "Rounding")
# IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.

########################################################################
# exercise
########################################################################

set.seed(16, sample.kind = "Rounding")
act_scores<-rnorm(10000,20.9,5.7)
sum(act_scores<=10)/10000
sum(act_scores>30)/10000
length(act_scores)
max(act_scores)

x<-seq(1,36)

data.frame(x, f_x=dnorm(x,20.9,5.7)) %>%
  ggplot(aes(x, f_x)) +
  geom_line()

set.seed(16, sample.kind = "Rounding")
act_scores<-rnorm(10000,20.9,5.7)
mm<-mean(act_scores)
ss<-sd(act_scores)
z_scores <- (act_scores-mm)/ss
sum(z_scores>2)/10000
sum(z_scores=2)
act_scores[z_scores=2]
qnorm(0.975, mm, ss)

set.seed(16, sample.kind = "Rounding")
act_scores<-rnorm(10000,20.9,5.7)
mm<-mean(act_scores)
ss<-sd(act_scores)
qnorm(0.95, 20.9, 5.7)
aaa <- function(x){pnorm(x,mm,ss)}
sapply(1:36,aaa)


act_scores<-rnorm(10000,20.9,5.7)
mm<-mean(act_scores)
ss<-sd(act_scores)
sample_quantiles<-quantile(act_scores)

set.seed(16, sample.kind = "Rounding")
act_scores<-rnorm(10000,20.9,5.7)
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles<-qnorm(p, 20.9, 5.7)
sample_quantiles<-quantile(act_scores)
plot(theoretical_quantiles, sample_quantiles)
data.frame(act_scores, theoretical_quantiles<-qnorm(p, 20.9, 5.7)) %>%
  ggplot(sample=aes(theoretical_quantiles)) +
  geom_qq(aes(sample=sample_quantiles))
# IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.

########################################################################
## Section 3: Confidence Intervals and p-Values
########################################################################

# Code: geom_smooth confidence interval example
# The shaded area around the curve is related to the concept of confidence intervals.

# sample function test
p <- 0.45
N <- 1000
sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p)) # picks either 0 or 1, N times, with replace 
sample(c(0,1,2,3,4,5,6,7,8,9), size = 3, replace = FALSE) # picks 3 numbers from 0 to 8, without replace i.e. numbers cannot repeat
sample(c(0,1,2,3,4,5,6,7,8,9), size = 3, replace = TRUE) # picks 3 numbers from 0 to 8, with replace i.e. numbers can repeat
sample(c(0,1,2,3,4,5,6,7,8,9), size = 3, replace = FALSE, prob = c(0.9,0.1)) # wrong stmt, the number of probabilities should be equal to the number of items in the first column

library(tidyverse)

data("nhtemp")
str(nhtemp)
class(nhtemp)
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
    ggplot(aes(year, temperature)) +
    geom_point() +
    geom_smooth() +
    ggtitle("Average Yearly Temperatures in New Haven")

# Code: Monte Carlo simulation of confidence intervals
# Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

# Code: Solving for z with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

# Code: Monte Carlo simulation
# Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
B <- 10000
inside <- replicate(B, {
    X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
    X_hat <- mean(X)
    SE_hat <- sqrt(X_hat*(1-X_hat)/N)
    between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

# own code
B <- 10000
inside <- replicate(B, {
    X <- sample(c(0,1), size = N, replace = TRUE)
    X_hat <- mean(X)
    X_hat
    SE_hat <- sqrt(X_hat*(1-X_hat)/N)
    SE_hat
    between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

# Code: Confidence interval for the spread with sample size of 25
# Note that to compute the exact 95% confidence interval, we would use c(-qnorm(.975), qnorm(.975)) instead of 1.96.
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)

# Code: Computing a p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))

########################################################################
## exercise
########################################################################

#### q1
# Load the data
library(dslabs)
data(polls_us_election_2016)

str(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(enddate>='2016-10-31' & !is.na(enddate))

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1]/100
#X_hat <- polls$adjpoll_clinton[1]/100
X_hat

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- (sqrt(X_hat*(1-X_hat))/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
#ci <- c(-qnorm(.975,mean=X_hat, sd=se_hat), qnorm(.975,mean=X_hat, sd=se_hat)) # incorrect ans
#ci <- c(-qnorm(.975), qnorm(.975)) # incorrect ans
#ci <- c(-qnorm(.95), qnorm(.95)) # incorrect ans
#ci <- c(X_hat - 2*se_hat, X_hat + 2*se_hat) # incorrect ans
#ci <- (2*X_hat - 1) + c(-2, 2)*2*se_hat # incorrect ans
ci<- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat) 

#### q2
# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
polls <- mutate(polls,
                X_hat = rawpoll_clinton/100,
                se_hat = sqrt(X_hat*(1-X_hat)/samplesize),
                lower = X_hat - qnorm(0.975)*se_hat,
                upper = X_hat + qnorm(0.975)*se_hat)

pollster_results <- select(polls,pollster,enddate,X_hat,se_hat,upper,lower)
str(pollster_results)

# answer
pollster_results <- polls %>% 
    mutate(X_hat = polls$rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize), lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) %>% 
    select(pollster, enddate, X_hat, se_hat, lower, upper)
str(pollster_results)

#### q3
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
p<-0.482
pollster_results <- mutate(pollster_results, hit = between(p, lower, upper)) # doesn't work
pollster_results <- mutate(pollster_results, hit = (p>=lower & p<=upper))
xx<-mean(pollster_results$hit)
avg_hit <- pollster_results %>% summarize(mean(hit))

avg_hit <- pollster_results %>% mutate(hit = (lower<=0.482 & upper>=0.482)) %>% summarize(mean(hit))


#### q4
xxx


#### q5
p<-0.482
str(polls)

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- 2*X_hat[1]-1
d_hat

p <- (d_hat+1)/2

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- p

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat))/N

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat) 

#### q5 answer
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")  %>%
    mutate(d_hat = rawpoll_clinton/100 - rawpoll_trump/100)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1]
d_hat

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)


#### q6
# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)
str(polls)
# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
polls <- mutate(polls,
                X_hat = (d_hat+1)/2,
                se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize),
                lower = d_hat - qnorm(0.975)*se_hat,
                upper = d_hat + qnorm(0.975)*se_hat)
pollster_results <- select(polls, pollster, enddate, d_hat, lower, upper)

pollster_results <- polls %>% mutate(X_hat = (d_hat+1)/2, se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize), lower = d_hat - qnorm(0.975)*se_hat, upper = d_hat + qnorm(0.975)*se_hat) %>% select(pollster, enddate, d_hat, lower, upper)

#### q7
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)
str(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit = (lower<=0.021 & upper>=0.021)) %>% summarize(mean(hit))


#### q8
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)
str(polls)
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls %>% mutate(error = d_hat - 0.021) %>% ggplot(aes(x = error, y = pollster)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### q9
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls %>% mutate(error = d_hat - 0.021) %>% group_by(pollster) %>% filter(n() >= 5) %>% ggplot(aes(x = error, y = pollster)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
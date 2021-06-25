# IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.

########################################################################
## Section 1: Parameters and Estimates
########################################################################

library(tidyverse)
library(dslabs)
take_poll(250)    # draw 25 beads

# DC1 assessment
# 5
# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0,1,len=100)

# Create a variable `se` that contains the standard error of each sample average
se <- sqrt((p*(1-p))/N)

# Plot `p` on the x-axis and `se` on the y-axis
plot(p,se)

# 6
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for (N in sample_sizes) {
  se <- sqrt((p*(1-p))/N)
  plot(p,se,ylim=c(0,0.1))
}


for(N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0,0.5/sqrt(25)))
}

# 7
# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.
2*sqrt(p*(1-p)/N)
########################################################################
## 3.0.1 INDEXING                                                     ##
########################################################################
# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000
# creating a logical vector that specifies if the murder rate in that state is less than or equal to 0.71
index <- murder_rate <= 0.71
# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]

########################################################################
## 3.0.2 INDEXING FUNCTIONS                                           ##
########################################################################
library(dslabs)
data(murders)

x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)    # returns indices that are TRUE

# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate <- murders$total / murders$population * 100000
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state

########################################################################
## 3.0.3 VECTORS                                                      ##
########################################################################
library(dslabs)
data(movielens)
str(movielens)

# We may create vectors of class numeric or character with the concatenate function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2]
codes[c(1,3)]
codes[1:2]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["canada"]
codes[c("egypt","italy")]

x <- c(1, "canada", 3)


########################################################################
## 3.1 BASIC DATA WRANGLING                                           ##
########################################################################

# installing and loading the dplyr package
install.packages("dplyr")
library(dplyr)

# adding a column with mutate
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filter
filter(murders, rate <= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)

# using the pipe
ola <- (murders %>% select(state, region, rate) %>% filter(rate <= 0.71) %>% data.frame())
ola
ola$region

########################################################################
## 3.2 CREATING DATA FRAMES                                           ##
########################################################################

# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)
					 
########################################################################
## 3.3 BASIC PLOTS			                                          ##
########################################################################

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
hist(murders$rate)

# boxplots of murder rates by region
boxplot(rate~region, data = murders)

########################################################################
## 3.4 ASSESMENT related	                                          ##
########################################################################

library(dslabs)
data(heights)
options(digits = 3)

str(heights)

sum(heights$height)/length(heights$height)
average_heights <- mean(heights$height)

ind <- heights$height > average_heights
sum(ind)
length(ind)

ind2 <- filter(heights,
                    heights$height > average_heights &
                      heights$sex=="Female")
str(above_avg)
length(above_avg$height)

female_count <- heights$sex=="Female"
mean(female_count)

mi<-min(heights$height)
ma<-max(heights$height)

#Uxse the match() function to determine the index of the first individual with the minimum height.
match(50,heights$height)
heights$sex[match(50,heights$height)]
heights$sex[1032]

mi:ma
x<-50:82.7
x<-min(heights$height):max(heights$height)
x %in% heights$height
sum(!(x %in% heights$height))

heights2 <- mutate(heights, ht_cm = heights$height*2.54)
head(heights2)
mean(heights2$ht_cm)
sum(heights2$sex=="Female")

fem<-heights2$sex=="Female"
mean(heights2$ht_cm[fem])

library(dslabs)
data(olive)
head(olive)
str(olive)

plot(olive$palmitic ,olive$palmitoleic)
hist(olive$eicosenoic)

boxplot(palmitic~region, data=olive)

########################################################################
## 4.2 Basic Conditionals	                                          ##
########################################################################

# an example showing the general structure of an if-else statement
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

# an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}

# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}

# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

########################################################################
## 4.3 Functions			                                          ##
########################################################################

# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))

# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

# the general form of a function
my_function <- function(VARIABLE_NAME){
  perform operations on VARIABLE_NAME and calculate VALUE
  VALUE
}

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

########################################################################
## 4.4 For Loops			                                          ##
########################################################################

# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

# a very simple for-loop
for(i in 1:5){
  print(i)
}

# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

# creating a plot for our summation function
n <- 1:m
plot(n, s_n)

# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))

# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)

#test

library(dslabs)
library(dplyr)
data(heights)
ifelse(heights$sex == "Female", 1, 2) %>% sum()

ifelse(heights$height>72, heights$height, 0) %>% mean()

inches_to_ft <- function(x){
  x/12
}
inches_to_ft(60)

ifelse(heights$height<60, 1, 0) %>% sum()
########################################################################
## 3.1 Summarizing with dplyr - dplyr		 				          ##
########################################################################

library(tidyverse)
library(dslabs)
data(heights)

# compute average and standard deviation for males
s <- heights %>%
    filter(sex == "Male") %>%
    summarise(average = mean(height), standard_deviation = sd(height))

# compute average and standard deviation for males, without the use of summarise function
s <- heights %>% filter(sex == "Male")
su <- c(average = mean(s$height), standard_deviation = sd(s$height), minimum = min(s$height))

su <- heights %>% filter(sex == "Male") %>%
    c(average = mean(height), standard_deviation = sd(height), minimum = min(height))#DOES NOT WORK
    
# access average and standard deviation from summary table
s$average
s$standard_deviation

# compute median, min and max
heights %>%
    filter(sex == "Male") %>%
    summarize(median = median(height),
              minimum = min(height),
              maximum = max(height))
# alternative way to get min, median, max in base R
quantile(heights$height, c(0, 0.5, 1))

# NOTE: The following code will NOT generate an error if using dplyr 1.0 or later

# generates an error: summarize can only take functions that return a single value
heights %>%
    filter(sex == "Male") %>%
    summarize(range = quantile(height, c(0, 0.5, 1)))
	
########################################################################
## 3.1 Summarizing with dplyr - The Dot Placeholder		 		      ##
########################################################################

library(tidyverse)
library(dslabs)
data(murders)

murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))
murders %>% summarize(avg = mean(murder_rate))

# calculate US murder rate, generating a data frame
us_murder_rate <- murders %>%
    summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

str(us_murder_rate)
str(us_murder_rate$rate)

# extract the numeric US murder rate with the dot operator
us_murder_rate %>% .$rate

# calculate and extract the murder rate with one pipe
us_murder_rate <- murders %>%
    summarize(rate = sum(total) / sum(population) * 100000) %>%
    .$rate

str(us_murder_rate)

########################################################################
## 3.1 Summarizing with dplyr - Group By				 		      ##
########################################################################

# libraries and data
library(tidyverse)
library(dslabs)
data(heights)
data(murders)

# compute separate average and standard deviation for male/female heights
heights %>%
    group_by(sex) %>%
    summarize(average = mean(height), standard_deviation = sd(height))

# compute median murder rate in 4 regions of country
murders <- murders %>%
    mutate(murder_rate = total/population * 100000)
murders %>%
    group_by(region) %>%
    summarize(median_rate = median(murder_rate))
	
########################################################################
## 3.1 Summarizing with dplyr - Sorting Data Tables		 		      ##
########################################################################

# libraries and data
library(tidyverse)
library(dslabs)
data(murders)

# set up murders object
murders <- murders %>%
    mutate(murder_rate = total/population * 100000)
    
# arrange by population column, smallest to largest
murders %>% arrange(population) %>% head()

# arrange by murder rate, smallest to largest
murders %>% arrange(murder_rate) %>% head()

# arrange by murder rate in descending order
murders %>% arrange(desc(murder_rate)) %>% head()

# arrange by region alphabetically, then by murder rate within each region
murders %>% arrange(region, murder_rate) %>% head()

# show the top 10 states with highest murder rate, not ordered by rate
murders %>% top_n(10, murder_rate)

# show the top 10 states with highest murder rate, ordered by rate
murders %>% arrange(desc(murder_rate)) %>% top_n(10)
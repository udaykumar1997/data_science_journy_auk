# IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.

########################################################################
## 1.1 Introduction to Discrete Probability
########################################################################

beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

########################################################################
# sample function test
p <- 0.45
N <- 1000
sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p)) # picks either 0 or 1, N times, with replace 
sample(c(0,1,2,3,4,5,6,7,8,9), size = 3, replace = FALSE) # picks 3 numbers from 0 to 8, without replace i.e. numbers cannot repeat
sample(c(0,1,2,3,4,5,6,7,8,9), size = 3, replace = TRUE) # picks 3 numbers from 0 to 8, with replace i.e. numbers can repeat
sample(c(0,1,2,3,4,5,6,7,8,9), size = 3, replace = FALSE, prob = c(0.9,0.1)) # wrong stmt, the number of probabilities should be equal to the number of items in the first column
########################################################################

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions


set.seed(1)
?set.seed
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

beads <- rep(c("red", "blue"), times = c(2,3))
beads
mean(beads == "blue")

########################################################################
## 1.1 Assignment
########################################################################

set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5
beads <- rep(c("cyan", "magenta", "yellow"), times = c(3,5,7))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random
pc = mean(beads == "cyan")
pnc = mean(beads != "cyan")
beads[-1] # beads list, without the 1st element
pcnc = mean(beads=="cyan")*mean(beads[-1]!="cyan") # probability that the first draw is cyan and that the second draw is not cyan, WITHOUT REPLACEMENT I.E. PROBABILITY IS DEPENDENT ON THE PREVIOUS TRIAL

pcnc = mean(beads=="cyan")*mean(beads!="cyan") # probability that the first draw is cyan and that the second draw is not cyan, WITH REPLACEMENT I.E. PROBABILITY IS INDEPENDENT OF THE PREVIOUS TRIAL

# datacamp stuff
cyan <- 3
magenta <- 5
yellow <- 7

# The variable `p_1` is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable `p_2` as the probability of not choosing a cyan ball on the second draw without replacement.
p_2 <- (magenta + yellow) / ((cyan-1) + magenta + yellow)

# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.
p_1*p_2

# Assign a variable 'p_2' as the probability of not choosing a cyan ball on the second draw with replacement.

# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.

########################################################################
## 1.2 Combinations and Permutations
########################################################################

# Code: Introducing paste() and expand.grid()
# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

# Code: Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)
mean(kings %in% deck)

# Code: Permutations and combinations
# Correction: The code shown does not generate all 7 digit phone numbers because phone numbers can have repeated digits. It generates all possible 7 digit numbers without repeats.

library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

# Code: Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# Code: Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

# Code: Monte Carlo simulation of natural 21 in blackjack
# Note that your exact values will differ because the process is random and the seed is not set.

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

# Code: The birthday problem
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

########################################################################
## 1.2 sapply
########################################################################

# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

# Code: Element-wise operation over vectors and sapply
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

# Code: Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
n <- 1:60
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob


B <- 10^seq(1, 5, len = 10)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

# testbed, for some assignment

# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two steps for B iterations: (1) generating a random four-game series `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for the Celtics.

funres <- function(B){
  celtic_wins <- replicate(B,{
    simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
    any(simulated_games=="win")
  })
  mean(celtic_wins)
}

# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.
funres(B)

# Code: Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

# Code: Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching

########################################################################
## exercise
########################################################################

# Assign a variable 'n' as the number of remaining games.
n<-6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes<-c(0,1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l<-sample(outcomes,n, replace = TRUE)
l
ok<-rep(list(outcomes),n)

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities<-expand.grid(ok)

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results<-rowSums(possibilities)>4

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)

# answers
# Assign a variable 'n' as the number of remaining games.
n <- 6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- c(0,1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`. 
l <- rep(list(outcomes), n)

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- expand.grid(l)

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities)>=4

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)

# q2 - my answer
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results<-replicate(B,
                   {sample(c(0,1),6,replace=1)}
)
# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console.
wins <- sum(results)>=4

mean(wins)

# q2 actual answer
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results <- replicate(B, {
  cavs_wins <- sample(c(0,1), 6, replace = TRUE)
  sum(cavs_wins)>=4 
})

# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console. 
mean(results)

library(gtools)
library(tidyverse)
install.packages("dplyr")
install.packages("tidyverse")

expand.grid(1:3,1:8)

l<-permutations(8,3)
nrow(l)

l<-permutations(3,3)
nrow(l)

6/336

B <- 10000
set.seed(1)
winnersList <- replicate(10000,{
  runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
  winners <- sample(runners,3,replace=0)
  condi<-("Jamaica" == winners)
  sum(condi)==3
})
mean(winnersList)

permutations(6,1)    # order matters
combinations(6,1)    # order does not matter
combinations(6,2)
combinations(2,1)

nrow(permutations(6,1)) * nrow(permutations(6,2)) * nrow(permutations(2,1))
nrow(combinations(6,1)) * nrow(combinations(6,2)) * nrow(combinations(2,1))
nrow(combinations(6,1)) * nrow(combinations(6,2)) * nrow(combinations(3,1))
nrow(combinations(6,1)) * nrow(combinations(6,3)) * nrow(combinations(3,1))

entreeChoice <- function(e){
  nrow(combinations(e,1)) * nrow(combinations(6,2)) * nrow(combinations(3,1))}
sideChoice <- function(s){
  nrow(combinations(6,1)) * nrow(combinations(s,2)) * nrow(combinations(3,1))}

e<-1:12
sapply(e,entreeChoice)

s<-2:12
sapply(s,sideChoice)

head(esoph)
library(tidyverse)

str(esoph)

nrow(esoph)
all_cases <- sum(esoph$ncases)
all_controls<- sum(esoph$ncontrols)

levels(esoph$alcgp)

ola <- (esoph %>% filter(alcgp == "120+") %>% data.frame())
all_cases <- sum(ola$ncases)
all_controls<- sum(ola$ncontrols)
all_cases/(all_cases+all_controls)

ola <- (esoph %>% filter(ncases >= "0-39g/day") %>% data.frame())
all_cases <- sum(ola$ncases)
all_controls<- sum(ola$ncontrols)
all_cases/(all_cases+all_controls)

ola1 <- (esoph %>% filter(ncases >= 1) %>% data.frame()) #for cases
ola2 <- (esoph %>% filter(ncontrols >= 1) %>% data.frame()) #for control
mean(ola2$tobgp >= "10-19")
a <- sum(ola2$ncontrol)
b <- sum(ola2$ncontrol[ola2$tobgp >= "10-19"])
b/a

a <- sum(ola2$ncases)
b <- sum(ola2$ncases[ola2$alcgp == "120+"])
c <- sum(ola2$ncases[ola2$tobgp == "30+"])
b/a
c/a
d <- sum(ola2$ncases[ola2$tobgp == "30+" & ola2$alcgp == "120+"])
d/a
e <- sum(ola2$ncases[ola2$tobgp == "30+" | ola2$alcgp == "120+"])
e/a


a <- sum(ola2$ncontrol)
b <- sum(ola2$ncontrol[ola2$alcgp == "120+"])
c <- sum(ola2$ncontrol[ola2$tobgp == "30+"])
b/a
c/a
d <- sum(ola2$ncontrol[ola2$tobgp == "30+" & ola2$alcgp == "120+"])
d/a
e <- sum(ola2$ncontrol[ola2$tobgp == "30+" | ola2$alcgp == "120+"])
e/a
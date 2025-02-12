#hash symbol: the line is not executed

#assigning values to variables
a <- 5
b <- 6

#making algorithmic operations
a + b
c <- a + b
#print the result
print(c)

#functions
c <- NA
sum (a,b)
c <- sum(a,b)

#vectors
name <- c("Greg", "Paul")
print(name)

#data frames
name <- c("Greg", "Paul", "Kim")
age <- c(47, 52, 34)
gender <- c("M", "M", "F")

friends <- data.frame(name, age, gender)

##

#navigating data frames
#look at a variable
friends$name

#look at a specific row/column combination
friends [1,1]
friends [ ,2]
friends [1:3,1]

#select a subset with a criteria
friends[friends$age<50, ]
young_friends <- friends[friends$age<50, ]

young_friends <- friends[friends$age<50, 1:2]


#install a package (just once!)
install.packages("tidyverse")
#activate the package in this session (to do at every session)
library(tidyverse)

#use the tidyverse to subset with a criteria
friends %>%
  select(name, age) %>%
  filter (age<50)

##

team_A <- 3
team_B <- 1
#if statement to check if team A score is higher than team B
if (team_A > team_B){
  print ("Team A wins")
}

#with the else statement we set an outcome when the if statement is not true

team_A <- 2
team_B <- 4

if (team_A > team_B){
  print ("Team A wins")
} else {
  print ("Team B wins")
}

#with the for loop we realise an action for each item in a vector
teams <- c("team_A","team_B")

for (i in teams){
  print(i)
}

#let's combine data frame, loops and if
name <- c("team_A", "team_B", "team_C", "team_D", "team_E")
score <- c(2, 5, 4, 1, 8)

teams <- data.frame(name, score)

#let's check each team to see if its score is higher than 3
iterations <- c(1:5)

for (n in iterations){
  if (teams [n, 2] > 3) {
  #cat prints a combination of textual and numerical argument into text  
  cat(teams [n, 1], " wins! \n")}
  else
  cat(teams [n, 1], " loses! \n")
}

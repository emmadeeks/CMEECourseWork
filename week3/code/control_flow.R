#!/usr/bin/env Rscript 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: control_flow.R
#Desc: Illustrates the use of if statements, while loops and for loops.
#Arguments: No input
#Outputs: The results from each loop illustrated
#Date: Oct 2019  

## If statement

a <- TRUE # assigns a to variable TRUE 
if (a == TRUE){
  print ("a is TRUE") # If a is equal to true then print but if not print false
} else {
  print ("a is FALSE")
}

## If statement on a single line
z <- runif(1) ## uniformly distributed random number
if (z <= 0.5) {print ("Less than a half")}

## For loop using a sequence
for (i in 1:10){
  j <- i * i
  print(paste(i, " squared is", j)) # prints result of calculation 
}

## For loop over vector of strings
for(species in c('Heliodoxa rubinoides',
                 'Boissonneaua jardini',
                 'Sula nebouxii')){
  print(paste('This species is', species)) # prints species
}

## for loop using a vector
v1 <- c("a", "bc", "def")
for (i in v1){
  print(i)
}

## While loop
i <- 0
while (i<10){
  i <- i+1
  print(i^2)
}


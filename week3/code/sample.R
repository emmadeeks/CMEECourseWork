#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: sample.R
#Desc: Example script of using vectorization involving lapply and sapply. 
#Also times the different functions under a vectorized and looped approach to lapply and sapply.
#Arguments: No input	
#Outputs: The result of the functions using loops and vectorisation as well as the time taken to run each function with apply,
#vectorization and preallocation
#Date: Oct 2019  

######### Functions ##########
# Function: Example script of using vectorization involving lapply and sapply. 


## A function to take a sample of size n from a population "popn" and return its mean
myexperiment <- function(popn,n){
  pop_sample <- sample(popn, n, replace = FALSE)
  return(mean(pop_sample))
}

## Calculate means using a for loop without preallocation:
loopy_sample1 <- function(popn, n, num){
  result1 <- vector() #Initialize empty vector of size 1
  for(i in 1:num){
    result1 <- c(result1, myexperiment(popn, n))
  }
  return(result1)
}

## To run "num" iterations of the experiment using a for loop on a vector with preallocation:
loopy_sample2 <- function(popn, n, num){
  result2 <- vector(,num) #Preallocate expected size
  for(i in 1:num){
    result2[i] <- myexperiment(popn, n)
  }
  return(result2)
}

## To run "num" iterations of the experiment using a for loop on a list with preallocation:
loopy_sample3 <- function(popn, n, num){
  result3 <- vector("list", num) #Preallocate expected size
  for(i in 1:num){
    result3[[i]] <- myexperiment(popn, n)
  }
  return(result3)
}


## To run "num" iterations of the experiment using vectorization with lapply:
lapply_sample <- function(popn, n, num){
  result4 <- lapply(1:num, function(i) myexperiment(popn, n))
  return(result4)
}

## To run "num" iterations of the experiment using vectorization with lapply:
sapply_sample <- function(popn, n, num){
  result5 <- sapply(1:num, function(i) myexperiment(popn, n))
  return(result5)
}

popn <- rnorm(1000) #Generate the population
hist(popn)
n <- 20 # sample size for each experiment
num <- 100 # Number of times to rerun the experiment

n <- 20 # sample size for each experiment
num <- 1000 # Number of times to rerun the experiment


#Times the time taken for script to run 
print("The loopy, non-preallocation approach takes:" )
print(system.time(loopy_sample1(popn, n, num)))

print("The loopy, but with preallocation approach takes:" )
print(system.time(loopy_sample2(popn, n, num)))

print("The loopy, non-preallocation approach takes:" )
print(system.time(loopy_sample3(popn, n, num)))

print("The vectorized sapply approach takes:" )
print(system.time(sapply_sample(popn, n, num)))

print("The vectorized lapply approach takes:" )
print(system.time(lapply_sample(popn, n, num)))

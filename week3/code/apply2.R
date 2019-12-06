#!/usr/bin/env Rscript 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: apply2.R
#Desc: Function of if statement that states if a number is lower than 0 multiple it by 100
#Arguments: Manual input not essential but can put number/matrix for function to be applied over	
#Outputs: Matrix of random numbers function has been applied over
#Date: Oct 2019  

#How to use apply to define your own functions. 
#This function is an if statement that states if a number is lower than 0 multiple it by 100 
#and return the output
#Output: Matrix of random numbers that the function has been applied over.

SomeOperation <- function(v){ #What does this function?
  if (sum(v) > 0){ # if the sum of the input number is greater than zero 
    return (v * 100) # Multiple by 100 
  }
  return(v) # Return the mutiplied number 
}

M <- matrix(rnorm(100), 10, 10) # makes matrix of random numbers 10 by 10
print (apply(M, 1, SomeOperation)) #Use apply to apply the function above on every number in the matrix 

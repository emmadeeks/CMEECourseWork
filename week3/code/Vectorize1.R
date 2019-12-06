#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: Vectorize.R
#Desc: A vectorisation example which compares the time taken to run a vectorisation function compared to a loop
#Arguments: No input
#Outputs:Time taken for loop and in build vectorisation function to run
#Date: Oct 2019  

#A vectorisation example which compares the time taken to run 
#a vectorisation function compares to aa loop 
M <- matrix(runif(1000000),1000,1000)

#Function sums all elements in a matrix whilst using the ocordinates of the matrix via a nested for loop 
SumAllElements <- function(M){
  Dimensions <- dim(M)
  Tot <- 0
  for (i in 1:Dimensions[1]){
    for (j in 1:Dimensions[2]){
      Tot <- Tot + M[i,j]
    }
  }
  return (Tot)
}

#times how long a looped and vectorised functiont akes and compares them 
print("Using loops, the time taken is:")
print(system.time(SumAllElements(M)))

print("Using the in-built vectorized function, the time taken is:")
print(system.time(sum(M)))

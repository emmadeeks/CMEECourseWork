#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: preallocate.R
#Desc: Illustrating the speed of allocation with one loop having preallocated values and one not
#Arguments: No input	
#Outputs: Speeds of two for loops
#Date: Oct 2019  

#Illustrating the speed of preallocation
# pre-allocate is quicker as it is more specific in arguments 
a <- NA
for (i in 1:10) {
  a <- c(a, i)
  print(a)
  print(object.size(a))
}

a <- rep(NA, 10)
for (i in 1:10) {
  a[i] <- i
  print(a)
  print(object.size(a))
}


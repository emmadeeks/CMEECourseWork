#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: next.R
#Desc: A for loop that runs through 1-10 and prints every other number or number that isnt equal to 2
#Arguments: No input
#Outputs: i in for loop
#Date: Oct 2019  

### print every other number in look 
## print a number that isnt equal to 2 
for (i in 1:10) {
  if ((i %% 2) == 0)
    next # pass to next iteration of loop
  print(i)
}

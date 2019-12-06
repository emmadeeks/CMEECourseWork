#!/usr/bin/env Rscript 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: browse.R
#Desc: Function that shows a different way of debugging by running a simulation of exponential 
#growth and returning a vector of generations before plotting it. Shows how to use break to run through the code line by line and debug.
#Arguments: Once run you can input code to navigate the break function
#Outputs: first iteration of the for loop and the console will enter the browser mode then allows manual debugging
#Date: Oct 2019  

#Runs a simulation of exponential growth 
#Runs a vector of length generations 

Exponential <- function(NO = 1, r=1, generations = 10){ #inputs into function
  N <- rep(NA, generations) #Creates a vector 
  N[1] <- NO
  for (t in 2:generations){
    N[t] <- N[t-1] * exp(r)
    browser() #This opens the debugger and takes you to the interactive bedugging part 
  }
  return(N)
}

plot(Exponential(), type = "1", main = "Exponential growth")
#Runs the first iteration of the for loop and the console will enter the browser mode
#Once in browser mode you can debug like in python by running through each line of code 
#E.g. n: single step, c: exit browser and continue, Q: exit browser and abort, return to top level
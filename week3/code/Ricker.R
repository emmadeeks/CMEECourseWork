#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: Ricker.R
#Desc: A vectorization challenge that runs a simulation of the Ricker model and returns a vector of length generations
#Arguments: No input	
#Outputs: Plots Ricker model
#Date: Oct 2019  

#This defines the ricker function 
# Runs a simulation of the Ricker model
# Returns a vector of length generations
Ricker <- function(N0=1, r=1, K=10, generations=50)
{

  N <- rep(NA, generations)    # Creates a vector of NA

  N[1] <- N0
  for (t in 2:generations)
  {
    N[t] <- N[t-1] * exp(r*(1.0-(N[t-1]/K)))
  }
  return (N)
}

#### PLOT MODEL 
plot(Ricker(generations=10), type="l") #plots the ricker model 

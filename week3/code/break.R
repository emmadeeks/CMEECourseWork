#!/usr/bin/env Rscript 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: break.R
#Desc: Script showing how to break out of loop
#Arguments: No input
#Outputs: Depending on if value is equal to 10 will output what number is equal to
#Date: Oct 2019  


#Breaking out of loops, often need to break out of loops when a certain conditions is met 
i <- 0 #Initatlise i
while(i <Inf) { 
  if (i == 10) {
    break
  } # Break out of the while loop!
  else {
    cat("i equals ", i, "\n") # says what i is equal to in iterations from 0 to 10 
    i <- i +1 #Update i
  }
}

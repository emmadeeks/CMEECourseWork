#!/usr/bin/env Rscript 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: boilerplate.R
#Desc: Illustrates how R functions work
#Arguments: Two arguments defined within script	
#Outputs: Class of inputs
#Date: Oct 2019  


#Illustrates how R functions work and takes as input 
#Already defined inputs and returns the class of these inputs

MyFunction <- function(Arg1, Arg2){

  # Statements involving Arg1, Arg2:
  print(paste("Argument", as.character(Arg1), "is a", class(Arg1))) # print Arg1's type
  print(paste("Argument", as.character(Arg2), "is a", class(Arg2))) # print Arg2's type

  return (c(Arg1, Arg2)) #this is optional, but very useful
}

MyFunction(1,2) #test the function
MyFunction("Riki","Tiki") #A different test

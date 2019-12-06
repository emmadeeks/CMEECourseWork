#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: get_TreeHeight.R
#Desc: Function that calculates heights of trees given distance of each tree from its base and angle to its top, using the trigonometric formula
#Arguments: You must input the name of the csv file but not the .csv at the end e.g. if your trees data is found in the data directory and is called
# trees.csv you only need to put trees into the command line e.g. Rscript get_TreeHeight.R trees
#Outputs: Heights of the tree, same units as distance
#Date: Oct 2019  

#Define function TreeHeight 
TreeHeight <- function(degrees, distance){
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  print(paste("Tree height is:", height))
  return (height)
}

#Makes the user input by making the commandArgs true 
arg1 <- commandArgs(TRUE)
#Takes to the directory 
ts <- read.csv(paste("../data/", arg1[1],".csv",  sep=""))

#assigns a to the heights of the trees in the ts data given the indexed inputs
a <- TreeHeight(ts$Angle.degrees, ts$Distance.m)
ts$Height.m <- a #appends this new vector of heights to the data frame as a new column called Heights.m

#writes a csv of the newdataframe and calls it the system argument follows by heights and csv. 
write.csv(ts, paste("../results/",arg1[1],"_heights.csv", sep = ""), row.names = FALSE)

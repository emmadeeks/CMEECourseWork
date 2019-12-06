#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: TreeHeight.R
#Desc: ACalculates heights of trees given distance of each tree from its base and angle to its top, using the trigonometric formula
#Arguments: Two arguments, degrees: The angle of elevation of tree; distance: The distance from base of tree (e.g., meters). Or it can take the relative path of a data file and calculate the tree height. 
#As a default it uses the trees.csv file in data
#Outputs: TreeHts.csv in results containing tree heights data appended onto trees.csv
#Date: Oct 2019  


# This function calculates heights of trees given distance of each tree
# from its base and angle to its top, using  the trigonometric formula
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"
ts <- read.csv("../data/trees.csv", header = TRUE)


TreeHeight <- function(degrees, distance){
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  #print(paste("Tree height is:", height))

  return (height)
}

#applied function on the ts data using the angle and distance columns as iputs 
a <- TreeHeight(ts$Angle.degrees, ts$Distance.m)
ts$Height <- a #adds a new column to ts 

write.csv(ts, "../results/TreeHts.csv") #saves as csv 

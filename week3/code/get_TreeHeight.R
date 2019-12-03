#!/usr/bin/env Rscript

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

a <- TreeHeight(ts$Angle.degrees, ts$Distance.m)
ts$Height <- a

write.csv(ts, paste("../results/",arg1[1],"_heights.csv", sep = ""), row.names = FALSE)

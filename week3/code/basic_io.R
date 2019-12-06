#!/usr/bin/env Rscript 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: basic_io.R
#Desc: Script to illustrate the different ways to write in a file and different parameters
#Arguments: No manual input required but uses trees.csv data from data directory
#Outputs: csv file names MyData in global environment
#Date: Oct 2019  

# A simple script to illustrate R input-output.
# Run line by line and check inputs putput to
# understand what is happening

# import with headers
MyData <- read.csv("../data/trees.csv", header = TRUE)

# Write it out as a new file
write.csv(MyData, "../results/MyData.csv")

# Append to it
write.table(MyData[1,], file = "../results/MyData.csv", append=TRUE)

# Write row names
write.csv(MyData, "../results/MyData.csv", row.names=TRUE)

# Ignore column names
write.table(MyData, "../results/MyData.csv", col.names=FALSE)


#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: GPDD_Data.R
#Desc: Loads and plots the species abundance worldwide using the maps package and saves it as a pdf	
#Arguments: Uses GPDDFiltered.RData in data
#Outputs: Species abundance worldwide
#Date: Oct 2019  

#Map of the world American
#Load the data
load("../data/GPDDFiltered.RData")
#require the maps package 
library("maps")

#select database of the world and include all of the regions
map(database = "world", regions = ".")
#plot the data and select the plot colour, size and shape 
points(gpdd$long, gpdd$lat, pch = 16, col = "green", cex = 1)

#

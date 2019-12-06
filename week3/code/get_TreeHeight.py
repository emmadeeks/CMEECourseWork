#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: DataWrangTidy.R
#Desc: Function that calculates heights of trees given distance of each tree from its base and angle to its top, using the trigonometric formula
#Arguments: Degrees and distance but not required manually as there is a default of 'trees.csv' from data
#Outputs: Heights of the tree, same units as distance
#Date: Oct 2019  

"""
This function calculates heights of trees given distance of each tree
from its base and angle to its top, using  the trigonometric formula

height = distance * tan(radians)

INPUTS
system arguments: 
None given: Takes as default trees.csv from data 
Argument given: has to specific path to data but uses as input based that it is in same layout as trees.csv

ARGUMENTS
degrees:   The angle of elevation of tree
distance:  The distance from base of tree (e.g., meters)

OUTPUT
The heights of the tree, same units as "distance"
"""
import math
import sys
import csv
import scipy as sc
import pandas as pd
import numpy as np
####################################################
# Define functions
####################################################


def Treeheight(degrees, distance):
    """ Defining tree height function """
    radians = (degrees * math.pi) / 180
    height = distance * math.tan(radians)
    return height
########################################################
# Importing csv
########################################################

# take system argument defined file name if there is one otherwise use the default trees.csv
if len(sys.argv) >1:
    trees = pd.read_csv(sys.argv[1]) #reads csv of system argument 
else:
    trees = pd.read_csv('../data/trees.csv', header =0) #reads in default data with header 

Treeheight(30, 40)

angle = trees["Angle.degrees"] #Subsets trees data to angle column and saves to a variable
distance = trees["Distance.m"] #Subsets trees data to distance column and saves to a variable 

h = np.array([]) #empty array to append heights to within below for loop

for i in range(0,len(angle)): #Goes through the length of the angle variable to get length of data 
    height = Treeheight(angle[i],distance[i]) #applies function to in each variable 
    h = np.append(h,height) #Appends the calculated height the array below the last calculation 
trees["Height.m"] = h #adds variable of height to trees csv and gives it the title Height.m 
trees.to_csv('../results/pythontrees_extra.csv')
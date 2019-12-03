#!/usr/bin/env python3

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
import math 
import os 
import sys
import csv
import scipy as sc
import pandas as pd 
import numpy as np


def TreeHeight(degrees, distance):
    radians = (degrees * math.pi) / 180
    height = distance * math.tan(radians)
    print("Tree height is:", height)
    return height 

TreeHeight(30,40)

if len(sys.argv) >1:
    trees = pd.read_csv(sys.argv[1])
else:
    trees = pd.read_csv('../data/trees.csv', header =0)

angle = trees["Angle.degrees"]
distance = trees["Distance.m"]
h = np.array([])
for i in range(0,len(angle)):
    height = TreeHeight(angle[i],distance[i])
    h = np.append(h,height)
trees["Height.m"] = h
trees.to_csv('../results/pythontrees.csv')


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


def TreeHeight(degrees, distance):
    radians = (degrees * math.pi) / 180
    height = distance * math.tan(radians)
    print("Tree height is:", height)
    return height

a = [1,2,3,4]

b = [4,5,6,7]

if len(sys.argv) > 1:
    with open(sys.argv[1]) as csv_file #asigns the read in files to variables 
    csvReader = csv.reader(TreeHeight, delimiter=',')
else:
    ts = open('../data/trees.csv', 'r')
    csvread = csv.reader(ts)


ts = open('../data/trees.csv', 'r')
csvread = csv.reader(ts)
ts[0][1]





deg = ts.loc[ : , 'Angle.degrees']
dis = ts.loc[ : , 'Distance.m']
TreeHeight(deg, dis)
ts['Height'] = ts.index
TreeHeight(ts[1],ts[2])

ts = open('../data/trees.csv', 'r')
csvread = csv.reader(ts)



TreeHeight(
ts.iloc[:1, :2]
a = ts.loc[:'Angle.degrees', :'Distance.m']
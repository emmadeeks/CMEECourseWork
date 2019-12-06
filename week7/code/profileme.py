#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: profileme.py
#Desc:  an illustrative programme about how to porfile functions and scripts 
#Arguments: no input 
#Outputs: Profiles of the three functions  defined in the script including running time etc and certain 'slow' parts
#Date: Oct 2019 


""" script that demonstrates profiling and how to time functions """
def my_squares(iters):
    """ takes as input iterations and mutiplies each iteration by 2 """
    out = []
    for i in range(iters):
        out.append(i**2)
    return out 

def my_join(iters, string):
    """ Takes as input iteration and a string and joins them """
    out = ''
    for i in range(iters):
        out += string.join(", ")
    return out

def run_my_funcs(x,y):
    """ takes as input x and y, mutliples x by two and joins x and y 
    together """ 
    print(x,y)
    my_squares(x)
    my_join(x,y)
    return 0  

# Runs all the functions using 10000 and the string 
run_my_funcs(10000000,"My string")
#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: profileme2.py
#Desc:  Similar script to profileme but the functions (which use to have loops)
# Now have list comprehensions instead and the .join has beenr eplaced with an explicit string concatenation 
#Arguments: no input 
#Outputs: Profiles of the three functions  defined in the script including running time etc and certain 'slow' parts
#Date: Oct 2019 


""" Similar script to profileme but the functions (which use to have loops)
Now have list comprehensions instead and the .join has beenr eplaced with an explicit string concatenation 
"""

def my_squares(iters):
    """ takes as input iterations and mutiplies each iteration by 2 """
    out = [i ** 2 for i in range(iters)]
    return out 

def my_join(iters, string):
    """ Takes as input iteration and a string and joins them """
    out = ''
    for i in range(iters):
        out += ", "+ string
    return out 

def run_my_funcs(x,y):
    """ takes as input x and y, mutliples x by two using my_squares and joins x and y 
    together using my_join with y as the string  """ 
    print(x,y)
    my_squares(x)
    my_join(x,y)
    return 0 

run_my_funcs(10000000,"My string")
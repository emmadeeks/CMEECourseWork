#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: timeitme.py
#Desc:  A script that times loops vs comprehension to see which is faster using timeitme and profileme2
#Arguments: No input
#Outputs: time results of loops verses comprehensions 
#Date: Oct 2019 

##############################################################################
# loops vs. list comprehensions: which is faster?
##############################################################################
""" A script that times loops vs comprehension to see which is faster """
iters = 1000000

import timeit 

from profileme import my_squares as my_squares_loops

from profileme2 import my_squares as my_squares_lc 


##############################################################################
# loops vs. the join method for strings: which is faster?
##############################################################################


mystring = "my string"

from profileme import my_join as my_join_join

from profileme2 import my_join as my_join 

#%timeit(my_join_join(iters, mystring))
#%timeit(my_join(iters, mystring))

import time
start = time.time()
my_squares_loops(iters)
print("my_squares_loops takes %f s to run." % (time.time() - start))

start = time.time()
my_squares_lc(iters)
print("my_squares_lc takes %f s to run." % (time.time() - start))

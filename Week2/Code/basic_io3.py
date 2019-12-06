#!/usr/bin/env python3


#Author: Emma Deeks ead19@imperial.ac.uk
#Script: basic_i03.py
#Desc:  Uses pickle to store an object from a dictionary before loading data again
#Arguments: No manual input but reads in testp.p
#Outputs: testp.p to sandbox directory with dictionary
#Date: Oct 2019 

""" This script uses pickle to store an object from a dictionary before loading data again """

#############################
# STORING OBJECTS
#############################
# To save an object (even complex) for later use 
my_dictionary = {"a key": 10, "another key": 11} # Creates a dictionary 

import pickle

f = open('../sandbox/testp.p','wb') ## note the b: accept binary files
pickle.dump(my_dictionary, f) # Illustrating use of pickle 
f.close()

## Load the data again
f = open('../sandbox/testp.p','rb')
another_dictionary = pickle.load(f)
f.close()

print(another_dictionary)
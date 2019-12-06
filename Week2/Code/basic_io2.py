#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: basic_i02.py
#Desc:  Saves a list of numbers to a text file with newline 
#Arguments: No manual input but reads in test.txt
#Outputs: testout.txt file to sandbox directory
#Date: Oct 2019 

""" This script saves a list of numbers to a text file with newlines """ 

# Save the elements of a list to a file 
list_to_save = range(100)

f = open('../sandbox/testout.txt','w')
for i in list_to_save:
    f.write(str(i) + '\n') ## Add a new line at the end

f.close() # Saves a list of numbers to a text file with newline 
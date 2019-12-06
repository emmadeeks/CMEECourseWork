#!/usr/bin/env python3


#Author: Emma Deeks ead19@imperial.ac.uk
#Script: basic_i01.py
#Desc:  Returns the contents of 'test.txt' in different formats e.g. excludes second lines
#Arguments: No manual input but reads in test.txt
#Date: Oct 2019 


""" This script returns the contents of 'test.txt' in different formats e.g. excludes second lines, 
Illustrates the use of for loops and writing files """

# Open a file for reading
f = open('../sandbox/test.txt', 'r')
# use "implicit" for loop:
# if the object is a file, python will cycle over lines
for line in f:  #Prints the lines in the file to a screen
    print(line)

# close the file
f.close()

# Same example, skip blank lines
f = open('../sandbox/test.txt', 'r')
for line in f:
    if len(line.strip()) > 0: #This is only printing lines with information on 
        print(line)

f.close()
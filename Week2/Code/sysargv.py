#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: sysargv.py
#Desc: Exercise of the sys.argv module which contains the names of the argument variables in the current script
#Arguments: No input
#Outputs: Name, number of arguments and the arguments of the script
#Date: Oct 2019 

""" Exercise of the sys.argv module which contains the names of the argument variables in the current script and prints them in different ways """

import sys
print("This is the name of the script: ", sys.argv[0]) # Prints acttal system argument 
print("Number of arguments: ", len(sys.argv)) #prints the length of the system argument 
print("The arguments are: " , str(sys.argv)) # Turns system arguments into strings 
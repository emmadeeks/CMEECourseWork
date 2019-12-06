#!/usr/bin/env python3 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: boilerplate.py
#Desc:  Example exercise on writing python programmes. Programme prints 'This is a boilerplate'). 
# Also illustrates use of docstrings
#Arguments: No input
#Outputs: 'This is a boilerplate'
#Date: Oct 2019 

"""This script illustrates how to writing python programmes using main arguments to import functions to modules """

__appname__ = '[boilerplate]'
__author__ = 'Emma Deeks (ead19@ic.ac.uk)'
__version__ = '0.0.1'
__licence__ = "License for this code/program"

## imports ##
import sys # Module to interface our program with the operating system 

## functions ##
def main(argv):  # this is defining a function 
    """ Main entry point of the program """ #Only appears in troubleshooting 
    print('This is a boilerplate') # NOTE: indented using two tabs or 4 spaces
    return 0
# Makes file usable as a script as well as a importable module 
if __name__ == "__main__":   # Turning into a programme- executable externally 
    """Makes sure the "main" function is called from command line"""  
    status = main(sys.argv)
    sys.exit(status)

#Import this module in python or ipython shell 

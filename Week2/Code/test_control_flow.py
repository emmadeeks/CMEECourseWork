#!/usr/bin/env python3 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: sysargv.py
#Desc: Exercise of the sys.argv module which contains the names of the argument variables in the current script
#Arguments: No input
#Outputs: Prints doctests
#Date: Oct 2019 

"""Some functions exemplifying the use of control statements"""

__author__ = 'Emma Deeks (ead19@ic.ac.uk)'
__version__ = '0.0.1'

## imports ##
import sys # Module to interface our program with the operating system 
import doctest #Import the doctest module 

def even_or_odd(x=0): # if not specified, x should take value 0.
    """Find whether a number x is even or odd. These are doctests 

    >>> even_or_odd(10)
    '10 is Even!'

    >>> even_or_odd(5)
    '5 is Odd!'

    whenever a float is provided then the closest integer is used:
    >>> even_or_odd(3.2)
    '3 is Odd!'

    in case of negative numbers, the positive is taken: 
    >>> even_or_odd(-2)
    '-2 is Even!'

    """

    #Define function to be tested 
    if x % 2 == 0: #The conditional if
        return "%d is Even!" % x
    return "%d is Odd!" % x

# In this function it sets the main module and gives the defined function inputs to test 
def main(argv):
    """ Main entry point of the program """ 
    print(even_or_odd(22))
    print(even_or_odd(33))
    return 0

if (__name__ == "__main__"):
    """Makes sure the "main" function is called from command line"""  
    status = main(sys.argv)
    print(even_or_odd.__doc__) #Allows for doctests 
    doctest.testmod() 
############################################


#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: using_name.py
#Desc: An if else statement within a python programme
#Arguments: No input
#Outputs: Prints whether the programme is being imported from another module of being run by itself
#Date: Oct 2019  

""" an if else statement within a python programme that states whether 
a python programme has been imported or being run by itself """

if __name__ == '__main__':
    """ If the programme is the main argument then it will print that its being 
    run by itself but if it is not the main then it is imported """ 
    
    print('This program is being run by itself')
else: 
    print('I am being imported from another module')
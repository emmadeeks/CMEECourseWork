#!/usr/bin/env python3
#Author: Emma Deeks ead19@imperial.ac.uk
#Script: debugme.py
#Desc: A script with a bug that was corrected with debugging
#Arguments: No input but make sure %pdb is turned on before running script 
#Outputs: uses ipdb to trace bug
#Date: Oct 2019 

""" A script with a bug that needs to be corrected with debugging """

#### NOTE: FOR THIS SCRIPT TO SUCCESSFULLY RUN YOU MUST TURN ON %PDB 
#Defines a function that creates a bug 
def createabug(x):
    """ function that has a bug """
    y = x**4 # This is likely the bug as there are two ** 
    z = 0.
    import ipdb; pdb.set_trace()
    y = y/z
    return y 

createabug(25)

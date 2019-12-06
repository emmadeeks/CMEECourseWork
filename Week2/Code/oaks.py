#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: oaks.py
#Desc: Uses for loops and list comprehensions to find taxa that are oak trees from a list of species.
#Arguments: No input
#Outputs: Oak species from list
#Date: Oct 2019 

""" Uses for loops and list comprehensions to find taxa that are oak trees from a list of species. """

## Finds just those taxa that are oak trees from a list of species 

taxa = [ 'Quercus robur',
         'Fraxinus excelsior',
         'Pinus sylvestris',
         'Quercus cerris',
         'Quercus petraea',
       ]

#find all species that start with quercus 
def is_an_oak(name):
    """ Find all the species that start with the name quercus """
    return name.lower().startswith('quercus ')

##Using for loops
oaks_loops = set() #creates an empty object
for species in taxa: #calls upon the taxa list 
    if is_an_oak(species): # calls the function and if it i add the the empty set 
        oaks_loops.add(species)
print("List of oaks found in taxa: \n", oaks_loops) #fills out species 

##Using list comprehensions
oaks_lc = set([species for species in taxa if is_an_oak(species)]) 
print("List of oaks found in taxa: \n", oaks_lc)

##Get names in UPPER CASE using for loops 
oaks_loops = set()
for species in taxa: 
    if is_an_oak(species): 
        oaks_loops.add(species.upper())
print("List of oaks found in taxa: \n", oaks_loops)

##Get names in UPPER CASE using list comprehensions 
oaks_lc = set([species.upper() for species in taxa if is_an_oak(species)])
print("List of oaks found in taxa: \n", oaks_lc)
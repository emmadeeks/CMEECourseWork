#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: lc1.py
#Desc: List comprehensions compared to for loops
#Arguments: No input 
#Outputs: Three lists containing latin names, common names and mean body masses for each species of birds in a given list of birds
#Date: Oct 2019 


# Creates a list of bird species, common name and body mass
birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

""" Uses List comprehensions to create three lists containing
latin names, common names and mean body masses for each species of birds """

#List comprehension for list with latin names 
first_words = {w[0] for w in birds}  #Comprehension that indexes the first line of the list
print("The Latin names of the birds using comprehensions: \n", first_words, "\n")

#List comprehension for list with common names 
first_words = {w[1] for w in birds} # common name is second in list 
print("The common names of the birds using comprehensions:\n ", first_words, "\n")


#List comprehension for list with body mass 
first_words = {w[2] for w in birds} # The body mass is third in the list 
print("The body mass of the birds using comprehensions: \n", first_words, "\n")


#For loop indexing a list of latin names from a list of birds
first_words = set() # This is to be appended with the desired latin names, common names and body mass 
# This is then printed to screen after for loop has run through index of list 
for w in birds: 
    first_words.add(w[0])
print("The Latin names of the birds using for loops: \n", first_words, "\n")

#For loop indexing a list of common names from a list of birds
first_words = set()
for w in birds: 
    first_words.add(w[1])
print("The common names of the birds using for loops: \n", first_words, "\n")


#For loop indexing a list of body mass from a list of birds
first_words = set() #Creates empty set that can be appened to in for loop 
for w in birds: 
    first_words.add(w[2])
print("The body mass of the birds using for loops: \n", first_words, "\n")
#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: blackbirds.py
#Desc:  Blackbirds practical that uses regex to output Kingdom, Phylum and Species of a csv of blackbird species, phylums and kingdoms
#Arguments: no input 
#Outputs: Species, kingdom and phylum of the blacbirds.txt file of each bird
#Date: Oct 2019 
import re

""" Blackbirds practical that uses regex to output Kingdom, Phylum and Species of a csv of blackbird species, phylums and kingdoms """

# Read the file (using a different, more python 3 way, just for fun!)
with open('../data/blackbirds.txt', 'r') as f:
    text = f.read()

# replace \t's and \n's with a spaces:
text = text.replace('\t',' ')
text = text.replace('\n',' ')
# You may want to make other changes to the text. 

# In particular, note that there are "strange characters" (these are accents and
# non-ascii symbols) because we don't care for them, first transform to ASCII:

text = text.encode('ascii', 'ignore') # first encode into ascii bytes
text = text.decode('ascii', 'ignore') # Now decode back to string

# Now extend this script so that it captures the Kingdom, Phylum and Species
# name for each species and prints it out to screen neatly.

# Hint: you may want to use re.findall(my_reg, text)... Keep in mind that there
# are multiple ways to skin this cat! Your solution could involve multiple
# regular expression calls (easier!), or a single one (harder!)

#Each of the variables  named Kingdom, Phylum and Species finds the key word of the variable and the associated word 
# after it e.g. Kingdom and Phylum have the assicated kingdom and phylums
# Species finds two words after it 
Kingdom = re.findall(r"Kingdom\s+\w+", text)
Phylum = re.findall(r"Phylum\s+\w+", text)
Species = re.findall(r"Species\s+\w+\s+\w+", text)

# For loop go through each of the three variables and prints the associate phylums, Kingdoms of that species to the screen
# Uses the length of the species variable to get the correct number of species for the iterations of the loop
# Indexs the phylum and kingdom variables so all of them are in the same iterations 
for i in range(len(Species)):
    print (Species[i], "is in:")
    print (Phylum[i])
    print (Kingdom[i], "\n")




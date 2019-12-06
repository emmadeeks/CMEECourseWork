#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: tuple.py
#Desc: Script to show use of tuples
#Arguments: No input
#Outputs: Prints the latin name, common name and mass of a list of birds on separate lines
#Date: Oct 2019 

birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
        )

# Birds is a tuple of tuples of length three: latin name, common name, mass.
# write a (short) script to print these on a separate line or output block by species 
# Hints: use the "print" command! You can use list comprehensions!
""" For loop that prints the latin name, common name and mass on seperate lines of a tuple of birds """


for i in birds: 
   print("The latin name is:",i[0]) #Indexes the list to extract the latin name, common name and mass
   print("The common name is:",i[1])
   print("The weight is:",i[2], '\n')
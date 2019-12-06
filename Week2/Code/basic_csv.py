
#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: basic_csv.py
#Desc:  Reads in the testcsv.csv file containing phylogeny information and appends a tuple with the name of a species.
#Arguments: No manual input but uses testcsv.csv from data directory
#Date: Oct 2019 
""" This script reads in the testcsv.csv file containing phylogeny information and appends a tuple with the name of a species."""
import csv

# Read a file containing:
# 'Species','Infraorder','Family','Distribution','Body mass male (Kg)'
f = open('../data/testcsv.csv','r')
csvread = csv.reader(f)
temp = []
# for loop that reads each row in the testcsv and prints the species in that row
for row in csvread: # 
    temp.append(tuple(row))  # Creates a tuple with species information 
    print(row) # Prints relevant information to computer 
    print("The species is", row[0]) #indexes specific row 

f.close()

# write a file containing only species name and Body mass
f = open('../data/testcsv.csv','r') # this 'r' means read the file
g = open('../data/bodymass.csv','w') # this 'w' means write the file

csvread = csv.reader(f) 
csvwrite = csv.writer(g) # asign these to a varianle 
for row in csvread:
    print(row)
    csvwrite.writerow([row[0], row[4]]) # indexes only species name and body mass from testcsv and appends it to the new file

f.close() # close files 
g.close()
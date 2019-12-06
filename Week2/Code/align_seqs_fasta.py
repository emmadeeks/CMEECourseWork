
#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: align_seqs_fasta.py
#Desc:  Same basic script as 'align_seqs.py' but aligns fasta files.
#Arguments: Option to manually input 2 fasta files to be aligned or to have a default where there are no inputs
#Date: Oct 2019 

"""This programme aligns sequences and outputs the best match and score, It takes as input fasta files"""

__appname__ = '[Align sequences]'
__author__ = 'Emma Deeks (ead19@ic.ac.uk)'
__version__ = '0.0.1'
__license__ = "License for this code/program"

import sys
import os

path = "../data" # this creates a variable called path which goes into the data directory 
os.chdir(path) # This changes the directory to the data directory 

def parse_fasta(fastafile): #This defines a function to strip fasta files of headers
     """ Defining parse_fasta input as fasta file, this function strips fasta files of their
     headers if they are found to have them in order for the alignment scores to be correct
    """
    fastastr = "" #creates empty variable
    header = True # defines that there is a header
    with open(fastafile, "r") as f: # reads in fasta file as f
        for row in f:  #goes through each row in file 
            if header: # if there is no header then do nothing
                header = False
            else: 
                fastastr += row.strip("\n") #if there is a header strip it and leave a newline
    return fastastr #this empty bariable becomes the ammended fasta fie

if len(sys.argv) > 1: #if there are more than one sys.arguments
    seq1 = parse_fasta(sys.argv[1]) #assigns these system arguments as variables
    seq2 = parse_fasta(sys.argv[2]) #Both inputs have become seq1 and 2 
else:
    seq1 = parse_fasta("407228412.fasta") # if no system arguments are put in then take these two files 
    seq2 = parse_fasta("407228326.fasta")  # as defaults 
  
# Assign the longer sequence s1, and the shorter to s2
# l1 is length of the longest, l2 that of the shortest
 
l1 = len(seq1)
l2 = len(seq2)
if l1 >= l2:
    s1 = seq1
    s2 = seq2
else:
    s1 = seq2
    s2 = seq1
    l1, l2 = l2, l1 # swap the two lengths

# A function that computes a score by returning the number of matches starting
# from arbitrary startpoint (chosen by user)
def calculate_score(s1, s2, l1, l2, startpoint):
    """
    This function is calculating the scores by a series of if statements
    """
    matched = "" # to hold string displaying alignements
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]: # if the bases match
                matched = matched + "*"
                score = score + 1
            else:
                matched = matched + "-"
    return score

# Test the function with some example starting points:
# calculate_score(s1, s2, l1, l2, 0)
# calculate_score(s1, s2, l1, l2, 1)
# calculate_score(s1, s2, l1, l2, 5)

# now try to find the best match (highest score) for the two sequences
my_best_align = None
my_best_score = -1

for i in range(l1): # Note that you just take the last alignment with the highest score
    z = calculate_score(s1, s2, l1, l2, i)
    if z > my_best_score:
        my_best_align = "." * i + s2 # think about what this is doing!
        my_best_score = z 

print("Sequences aligned!")

path = "../code" #Changing back to the code directory 
os.chdir(path)

f = open('../results/best_fasta_alignment.txt', 'w+') #Write a new text file 
my_best_score = str(my_best_score)  # Converts my_best_score to a string
f.write ("Best Alignment: ")
f.write (my_best_align) #Alignement 1 
f.write("\n") # New line 
f.write(seq2) # Alignment 2 
f.write ("Best Score: ")
f.write (my_best_score) # Score 
f.close()


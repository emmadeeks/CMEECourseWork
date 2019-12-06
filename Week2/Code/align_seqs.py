
#!/usr/bin/env python3
#Author: Emma Deeks ead19@imperial.ac.uk
#Script: align_seqs.py
#Desc:  Converts the initial 'align_seqs.py' script to a programme that 
# takes DNA sequences as an input from a single external file and saves the best alignment along with the score into a text file.
#Arguments: No manual input but in script takes single external csv file with DNA sequences
#Date: Oct 2019 
"""This programme aligns sequences and outputs the best match and score"""

__appname__ = '[Align sequences]'
__author__ = 'Emma Deeks (ead19@ic.ac.uk)'
__version__ = '0.0.1'
__license__ = "License for this code/program"

import sys
# Assign the longer sequence s1, and the shorter to s2
# l1 is length of the longest, l2 that of the shortest
exec(open("../data/sequences.csv").read(), globals()) #This executes a scripts and puts all the variables into the global scope.
                                                       #This means the rest of the programme can read in te variabels and use them 

l1 = len(seq1)  # data wrangling to get the sequences into a usable format with the longest sequence being s1 
l2 = len(seq2) # s2 is always sequence that is compared so is shortest 
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

    # some formatted output
    print("." * startpoint + matched)        
    print("." * startpoint + s2)
    print(s1)
    print(score) 
    print(" ")

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
with open('../results/best_alignment.txt', 'w') as f:  # Writes text file 
    print(my_best_align, file=f)
    print(s1, file=f)
    print("Best score:",my_best_score, file=f) 


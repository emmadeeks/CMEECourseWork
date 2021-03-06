
#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: align_seqs_better.py
#Desc:  Aligns sequences and outputs best match and score 
#saves the output into a text file called Improved_fasta_alignment.txt
#Arguments: Option to manually input 2 fasta files to be aligned or to have a default where there are no inputs
#Date: Oct 2019 

"""This programme aligns sequences and outputs the best match and score.
Takes as input two files or if not files are inputed will default use two input files
This programme returns the highest alignments in a text file called 'Improved
fasta alignment.txt. This script also takes into account any fasta file headings that might 
impact the score """

__author__ = 'Emma Deeks (ead19@ic.ac.uk)'


import sys
import os   #Changes root directory

path = "../data"   #sets variable of directory change 
os.chdir(path) #Changes the directory 

def parse_fasta(fastafile): #function named parse_fasta which defines the input as fastafile
   """ Defining parse_fasta input as fasta file, this function strips fasta files of their
     headers if they are found to have them in order for the alignment scores to be correct
    """
    fastastr = ""  #Sets fastastr as an empty string 
    header = True #Includes a header 
    """ Opens input """
    with open(fastafile, "r") as f: #opens input and reads in as f 
        for row in f:     #Goes through input file which is f 
            if header:  # Changes header to false so when the forloop goes through it ignores header
                header = False 
            else:  #If there is not a header the for loop strips the line and removes the newline character
                fastastr += row.strip("\n")
    return fastastr 

if len(sys.argv) > 1:
    seq1 = parse_fasta(sys.argv[1]) #asigns the read in files to variables 
    seq2 = parse_fasta(sys.argv[2]) # 
else:
    seq1 = parse_fasta("407228412.fasta") #If variables are not defined take the default 
    seq2 = parse_fasta("407228326.fasta")
  
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
my_best_score = {} #Makes an empty dictionary to high score alignments 

for i in range(l1): # Note that you just take the last alignment with the highest score
    z = calculate_score(s1, s2, l1, l2, i)
    my_best_align = "." * i + s2 # think about what this is doing!
    if z in my_best_score.keys():
        my_best_score[z].append(my_best_align) #if else statement taking the highest score and putting it in the dictinary
    else:
        my_best_score[z] = [my_best_align] #if new score is equal to high score add to dictionary 


vals = max(my_best_score.keys())

print("Sequences aligned!")


#dump <- bests, pickle_bests 
#pickle bests.close()

path = "../Code"
os.chdir(path)

# Writes a new text file into the results directory with the best sequence alignment 

f = open('../results/improved_fasta_alignment.txt', 'w+')
f.write ("Best Alignment: "+"\n") # Reads 'Best Alignment' then newspace
for x in my_best_score[vals]:
    f.write(x)
f.write ("\n" + seq2 + "\n") # adds in the first sequence
f.write ("Best Score: "+"\n" + str(vals)) # Adds in the second sequence 

f.close()



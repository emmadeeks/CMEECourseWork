#!/bin/bash

# Author: Emma Deeks
# Script: run_get_TreeHeight.sh
# Desc: runs both of the python and R scripts of get_TreeHeights from the command line.
#gets Tree Heights from input file and saves
# Arguments: 1 -> .csv file
# Date: 30 October 2019

## 
echo "Creating .csv file with calculated tree heights"

#runs both of the python and R scripts of get_TreeHeights from the command line 
args <- commandArgs()
print(args)

Rscript get_TreeHeight.R trees

python3 get_TreeHeight.py trees

echo "Done!"
exit
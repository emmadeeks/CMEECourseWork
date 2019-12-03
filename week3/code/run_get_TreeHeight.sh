#!/bin/bash
# Author: Emma Deeks
# Script: run_get_TreeHeight.sh
# Desc: gets Tree Heights from input file and saves
# Arguments: 1 -> .csv file
# Date: 30 October 2019

echo "Creating .csv file with calculated tree heights"

args <- commandArgs()
print(args)

Rscript get_TreeHeight.R trees

python3 get_TreeHeight.py trees

echo "Done!"
exit
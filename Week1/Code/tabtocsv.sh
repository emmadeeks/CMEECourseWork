#!/bin/bash
#Author: Emma Deeks ead19@imperial.ac.uk
#Script: tabtocsv.sh
#Desc: Substitute the tabs in the file with commas 
#saves the output into a csv file 
#Arguments: 1 -> tab delimited file 
#Date: Oct 2019 

echo "Creating a comma delimited version of $..."
cat $1 | tr -s "\t" "," >> $1.csv
echo "Done!"
exit 
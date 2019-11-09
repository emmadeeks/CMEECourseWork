#!/bin/bash
#Author: Emma Deeks ead19@imperial.ac.uk
#Script: csvtospace.sh
#Desc: Converts comma seperated values to space seperated values 
#saves the output into a seperately file csvtospace_converted.txt 
#Arguments: 1 -> tab delimited file 
#Date: Oct 2019 

for f in `ls ../data/Temperatures/*.csv`; #goes to the directory containing the csv files 
    do
        echo "Creating a comma delimited version of ${f} ..."; 
        a=`echo ${f}| cut -f 3 -d "."`  # cuts the .csv from the end value so instead of .csv.txt it is just .txt. defines a as the new variable
        echo ${a} 
        cat ${f} | tr -s "," "\ " >> ..${a}.txt #replaces commas with spaces and outputs it to a text file 
        echo "Done!";
    done
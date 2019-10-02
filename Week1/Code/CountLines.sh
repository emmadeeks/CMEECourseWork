#!/bin/bash 

NumLines=`wc -l < $1` #This is a L not a 1 
echo "The file $1 has $NumLines lines"
echo
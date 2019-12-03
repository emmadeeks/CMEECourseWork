#!/bin/bash
# File that outputs the result of two files merged into one
cat $1 > $3
cat $2 >> $3
echo "Merged File is "
cat $3 

#exit

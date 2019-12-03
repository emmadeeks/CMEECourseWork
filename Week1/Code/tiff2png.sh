#!/bin/bash
# For loop that converts any tif file into a jpeg
for f in *.tif;
    do
        echo "Converting $f";
        convert "$f" "$(basename "$f" .tif).jpg"; 
    done 
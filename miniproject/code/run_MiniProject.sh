#!/bin/bash 
#Author: Emma Deeks ead19@imperial.ac.uk
#Script: run_MiniProject.sh
#Desc: One file to run all the mini project scripts
#Arguments: 4 files: preparing_data.py, fitting_script.R, 
# plotting_script.py, miniproject_write.tex
#Date: 26th February 2020

### Run data exploration script
python3 "preparing_data.py"

### Run NLLS fitting script 
Rscript "fitting_script.R"

### Run model analysis script 
python3 "plotting_script.py"

### Run LaTeX compiling script
pdflatex "miniproject_write.tex"

echo "Report compiled"
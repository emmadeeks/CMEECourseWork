#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: TestR.py
#Desc:  runs Rscript through python using os
#Arguments: no inputs 
#Outputs: Outputs output of TestR.R to results directory called TestR.Rout
#Date: Oct 2019 

""" Practical on using os- open and run rscript in python """


import subprocess

subprocess.Popen("Rscript --verbose TestR.R > ../results/TestR.Rout 2> ../results/TestR_errFile.Rout", shell=True).wait()
#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: run_LV.py
#Desc:  Profiles and times all the LV scripts using timeit and CPorfile. 
#Arguments: Does not require inputs but takes manual inputs of r, a, z and e 
#Outputs: Profiles of the three functions  defined in the script including running time etc and certain 'slow' parts
#Date: Oct 2019 

##############################################################################
# loops vs. list comprehensions: which is faster?
##############################################################################

""" Imports the functions from LV1 and Lv2 and runs and compares their times """
""" Also uses Cprofile to profile the two functions """
""" an attempt was made to time and profile LV3 but it has no functions so its just running a whole script """

import timeit 
import scipy as sc
import time
import sys
import cProfile

from LV1 import dCR_dt as LV1_func
from LV2 import dCR_dt as LV2_func
from LV3 import LV3 as LV3_script

r = float(sys.argv[1])
a = float(sys.argv[2])
z = float(sys.argv[3])
e = float(sys.argv[4])

R0 = sc.array([10,5])

start = time.time()
LV1_func(R0)
print(" \nLV1 function takes %f s to run. \n " % (time.time() - start))

start = time.time()
LV2_func(R0)
print("LV2 function takes %f s to run. \n " % (time.time() - start))

start = time.time()
LV3_script
print("LV3 script takes %f s to run. \n " % (time.time() - start))

#profile function and scripts 
cProfile.run('LV1_func(R0)')
cProfile.run('LV2_func(R0)')
cProfile.run('LV3_script')
#!/usr/bin/python

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: Vectorize2.py
#Desc: Two scripts, one stochastic Ricker model and an improved version of this model which is vectorised, 
#this script is the python version of 'Vectorize2.R'
#Arguments: No input
#Outputs:Speed comparison of both scripts
#Date: Oct 2019  

""" Two scripts, one stochastic Ricker model and an improved version of this model which is vectorised, 
this script is the python version of 'Vectorize2.R'"""

#Import random for generating random distributions 
#Import numpy for functions for maths- number python
#Import time for timing two functions 
import random
import numpy as np
import time 

#The Stochastic Ricker Model defined by Samraat in R and converted to Python

def stochrick():
  """ defines the stochrick richer model """
    p0= [] #Start population
    for i in range(0,1000):
        nextvalue = random.uniform(0.5,1.5)
        p0.append(nextvalue) #Generate a list of 1000 random numbers from 0.5-1.5 
    r=1.2 #variable r with 1.2- intrinsic growth rate 
    K=1  #Carrying capacity 
    sigma=0.2 #The SD in the random distribution generated- setting the SD for stochasicity
    numyears=100 #defining length of model 
    N = np.zeros((numyears,len(p0))) #Generate a list of 100 years 
    N[0] = p0 #matrix of population with year- start population 
    for pop in range(0,len(p0)):#loop through the populations

        for yr in range(1,numyears): #for each pop, loop through the years

            N[yr,pop] = N[yr-1,pop] * np.exp(r * (1 - N[yr - 1,pop] / K) + np.random.normal(0.5,sigma)) #Ricker model

    return(N)


###### Improved function ########

def stochrickvect():
  """ defines the vectorised Stochastic Ricker Model """
    p0= []
    for i in range(0,1000):
        nextvalue = random.uniform(0.5,1.5)
        p0.append(nextvalue)
    r=1.2
    K=1
    sigma=0.2
    numyears=100
    N = np.zeros((numyears,len(p0)))
    N[0] = p0
    for yr in range(1,numyears): #for each pop, loop through the years

            N[yr,] = N[yr-1,] * np.exp(r * (1 - N[yr - 1,] / K) + np.random.normal(0.5,sigma))

    return(N)

#This is vectorised because instead of indexing with population and years 
#The improved version just vecotrises the function by only using year and then 
#the model just runs the whole way down each column as oppose to 

# Comapares times of the ricker model 
start_time = time.time()
stochrick()
runningtime = time.time() - start_time
print("Stochastic Ricker takes:", runningtime)

# Comapres times of the vectorised ricker model 
# THIS IS MUCH QUICKER 
start_time = time.time()
stochrickvect()
runningtime = time.time() - start_time
print("Vectorized Stochastic Ricker takes:", runningtime)

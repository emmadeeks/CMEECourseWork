
#!/usr/bin/python
#A vectorisation example which compares the time taken to run 
#a vectorisation function compares to aa loop 
import numpy as np
import time 
M = np.random.uniform(1000000,size=(1000,1000)) #Make a matrix of random numbers 1000 by 1000 

#defines a function called SumAllElements that takes as input M

def SumAllElements(M):
  Dimensions = M.shape #saves the dimensions of M using .shape to a variable called dimensions
  Tot = 0 #Assigns 0 to a variable called Tot
  for i in range(0,Dimensions[0]): #Indexex the dimensions variable to the first column via 0 and also runs through the whole 1000 numbers 
    for j in range(0,Dimensions[1]): #Indexes second collumn or value of Dimensions via [1] and runs through all numbers 
      Tot = Tot + M[i,j] #Indexs the coordinates of i an j of the matrix and adds the of the element to the variable Tot
  return (Tot)

#Stores current time as a variable then runs the function and subtracts start time to get speed of function and returns the running tiem 
start_time = time.time()
SumAllElements(M)
runningtime = time.time() - start_time
print("Using loops, the time taken is:", runningtime)


start_time = time.time()
M.sum
runningtime = time.time() - start_time
print("Using the in-built vectorized function, the time taken is:", runningtime)


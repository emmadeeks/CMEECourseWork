#!/usr/bin/env python3 

"""Programme that tests mathmatical functions and outputs the results """

__appname__ = '[boilerplate]'
__author__ = 'Emma Deeks (ead19@ic.ac.uk)'
__version__ = '0.0.1'
__licence__ = "License for this code/program"

## imports ##
import sys # Module to interface our program with the operating system 

# What does each of foo_x do? 

def foo_1(x):
    return x ** 0.5 #Takes the square root of x

# If x is larger than y retur x and then return y underneth 
def foo_2(x,y):
    if x > y: 
        return x 
    return y 

# Orders the three numbers ans reutnrs the numbers ordered
def foo_3(x,y,z):
    if x > y:
        tmp = y
        y = x
        x = tmp
    if y > z: 
        tmp = z
        z = y
        y = tmp
    return [x, y, z]

#one to x +1 and times them by themselves and outputs the results
def foo_4(x):
    result = 1
    for i in range(1, x + 1):
        result = result * i 
    return result 

# A recursrive function that calculates the factorial of x
def foo_5(x):
    if x ==1:
        return 1
    return x * foo_5(x - 1)

#Calculate the facorial of x in  different way 
def foo_6(x):
    facto = 1
    while x >= 1: 
        facto = facto * x
        x = x -1
    return facto


def main(argv):
    print("The square root of x is:", foo_1(3))
    print("The highest value is:", foo_2(5,7))
    print("Numbers ordered:", foo_3(3,9,8))
    print("Result:", foo_4(7))
    print("Factoral of value is:", foo_5(8))
    print("Factoral of value is:", foo_6(9))


if __name__ == "__main__": 
    """Makes sure the "main" function is called from command line"""  
    status = main(sys.argv)
    sys.exit(status)


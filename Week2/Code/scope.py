#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: scope.py
#Desc: A collection of three blocks of code illustrating variable scope.
#Arguments: No input
#Outputs: Output of variable scope functions
#Date: Oct 2019 

""" A collection of three functions illustrating variable scope. This script has functions that assigns global and local functions in different ways  """

_a_global = 10 # a global variable

if _a_global >= 5:     #if this global variable is = or above 5
    _b_global = _a_global + 5 # also a global variable

def a_function():
    """ Function that sets a global variable and a local variable and prints these variables to screen """
    _a_global = 5 # a local variable
    
    if _a_global >= 5:
        _b_global = _a_global + 5 # also a local variable
    
    _a_local = 4
    
    print("Inside the function, the value of _a_global is ", _a_global)
    print("Inside the function, the value of _b_global is ", _b_global)
    print("Inside the function, the value of _a_local is ", _a_local)
    
    return None

a_function()

print("Outside the function, the value of _a_global is ", _a_global)
print("Outside the function, the value of _b_global is ", _b_global)


###### Second code block 

_a_global = 10 #global variable

def a_function(): 
    """ Prints the local and global variables """ 
    _a_local = 4

    print("Inside the function, the value of _a_local is ", _a_local)
    print("Inside the function, the value of _a_global is ", _a_global)

    return None 

a_function()

print("Outside the function, the value of _a_global is", _a_global)

# Third code block 

_a_global = 10

print("Outside the function, the value of _a_global is", _a_global)

def a_function():
    """ Prints the local and global variables """ 
    global _a_global
    _a_global = 5
    _a_local = 4

    print("Inside the function, the value of _a_global is ", _a_global)
    print("Inside the function, the value _a_local is ", _a_local)

    return None

a_function()

print("Outside the function, the value of _a_global is", _a_global)

#Fourth code block 
_a_global = 10

def a_function():
    """ Defines a function with a function of global and local variables """

    def _a_function2():
        global _a_global
        _a_global = 20 
    
    print("Before calling a_function, balue of _a_global is", _a_global)

    _a_function2()

    print ("After calling _a_function2, value of _a_global is ", _a_global)
    
a_function()

print("The value of a_global in main workspace / namespace is ", _a_global)

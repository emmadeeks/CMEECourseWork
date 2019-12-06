#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: cfexercises2.py
#Desc:  A series of conditionals containing for and while loops
#Arguments: No input
#Outputs: Output of for a while loops
#Date: Oct 2019 

""" A series of conditionals containing for and while loops """

#for j in range of 12 dont print every 3 
for j in range(12):
    if j % 3 == 0: 
        print('prints every 3rd value out of 12')

# for the range in 15 if j is a multiple of 5 or 4 print 
for j in range(15):
    if j % 5 == 3:
        print('Multiple of 5')
    elif j % 4 == 3:
        print('Multiple of 4')

#Whilst z is not equal to 15 print hello in incredments of 3 
z = 0
while z != 15: 
    print('not equal to 15 yet!')
    z = z + 3

# makes z 12 and whil z is not equal to 100 if z is equal to 31 
# If z is equal to 31 print 
z = 12 
while z < 100:
    if z == 31:
        for k in range(7): 
            print('z is equal to 31 so print this 7 times')
    elif z == 18: 
        print('z is equal to 18!')
    z = z + 1
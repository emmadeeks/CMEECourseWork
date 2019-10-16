# FOR loops in python

#Prints 1 5 times 
for i in range(5):
    print(1)

#This creates a list and then just prints it in a list as k goes through the stuff in my_list
my_list = [0, 2, "geronimo!", 3.0, True, False]
for k in my_list:
    print(k)

# this for loop will go through summands and add each part of the list to total 0 and print it 
# It then adds cuitively 
total = 0
summands = [0, 1, 11, 111, 1111]
for s in summands:
    total = total + s
    print(total)

# WHILE loops in Python 

# Limit is 100 and z = 0, print z + 1 as z is now that variable 
z = 0 
while z < 100:
    z = z + 1
    print(z)

#will just print 'True' indefinately because nothing is making b False 
b = True 
while b: 
    print("GERONIMO! infinite loop! ctrl+c to stop!")
# ctrl + c to stop! 

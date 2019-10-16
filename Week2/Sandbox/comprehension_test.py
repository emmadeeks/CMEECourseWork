## Finds list those taxa that are oak trees from a list of species 

#There is a range of 10 and i will run throough the range and print the numbers- remember it starts from 0 
x = [i for i in range(10)]
print(x)

#Makes an empty vector (not vector LIST) called x and fils it by running through the range 10 and appending the list  
x = []
for i in range(10):
    x.append(i)
print(x)

#i.lower means print in lower case and so just print the list made in lower case 
x = [i.lower() for i in ["LIST", "COMPREHENSIONS", "ARE", "COOL"]]
print(x)

#Makes a list then specifies the range is x and fills it in lower case and prints it
x = ["LIST", "COMPREHENSIONS", "ARE", "COOL"]
for i in range(len(x)): #explicit loop
    x[i] = x[i].lower()
print(x)

#Same as above but just in a different way 
x = ["LIST", "COMPREHENSIONS", "ARE", "COOL"]
x_new = []
for i in x: #implicit loop 
    x_new.append(i.lower())
print(x_new)

#This is a nested loop and it makes a matrix. flatten matrix i believe will just ake the matrix less 3d
matrix = [[1,2,3], [4,5,6], [7,8,9]]
flattened_matrix = []
for row in matrix: 
    for n in row: 
        flattened_matrix.append(n)
print(flattened_matrix)

#same just in a list comprehension
matrix = [[1,2,3], [4,5,6], [7,8,9]]
flattened_matrix = [n for row in matrix for n in row]
print(flattened_matrix)

#create a set of all the first letters in a sequence of words using a loop
words = (["These", "are", "some", "words"])
first_letters = set() #creates an empty set object
for w in words: 
    first_letters.add(w[0]) #only include the first letter 
print(first_letters)

#Letters are unordered and they need to be put in order so use a comprehension
#Note this still doesnt order them just does the same thing
words = (["These", "are", "some", "words"])
first_letters = set()
for w in words: 
    first_letters.add(w[0]) #adding only the first letter 
print(first_letters)
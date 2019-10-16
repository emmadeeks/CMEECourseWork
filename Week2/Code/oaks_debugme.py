import csv
import sys
import doctest
import re

#Define function
def is_an_oak(name):
    
    """Tests the is_an_oak function to check output is correct. A function that outputs oak species from a given list 

    >>> is_an_oak('Fagus sylvatica')
    False

    >>> is_an_oak('quercus sylvatica')
    True

    >>> is_an_oak('quercs sylvatica')
    False

    """

    if re.match(r'\bquercus\b', name, re.IGNORECASE): 
        return True  #If input is exactly 'quercus' ignoring case than accept otherwise reject
    else:
        return False

def main(argv): 
    f = open('../Data/TestOaksData.csv','r')
    i =f.readline() 
    p = f.readlines()[0:] #Excludes header line of file
    g = open('../results/JustOaksData.csv','w')
    taxa = csv.reader(p)
    csvwrite = csv.writer(g)
    g.write(i) 
    oaks = set()
    for row in taxa:
        print(row)
        print ("The genus is: ") 
        print(row[0] + '\n')
        if is_an_oak(row[0]):
            print('FOUND AN OAK!\n')
            csvwrite.writerow([row[0], row[1]])    
      
    return 0   


if (__name__ == "__main__"):
    status = main(sys.argv)
    print(is_an_oak.__doc__) #Allows for doctests 
    doctest.testmod() 


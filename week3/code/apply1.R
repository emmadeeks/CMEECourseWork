#Function: Builds a random matrix 
#and makes a mean of each row in that matrix. 

## Build a random matrix
M <- matrix(rnorm(100), 10, 10)

## Take the mean of each row and prints it
RowMeans <- apply(M, 1, mean)
print (RowMeans)

## Now the variance and prints it
RowVars <- apply(M, 1, var)
print (RowVars)

# By column and prints it 
ColMeans <- apply(M, 2, mean)
print (ColMeans)

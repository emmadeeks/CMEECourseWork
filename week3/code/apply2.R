#How to use apply to define your own functions. 
#This function is an if statement that states if a number is lower than 0 multiple it by 100 
#and return the output
#Output: Matrix of random numbers that the function has been applied over.

SomeOperation <- function(v){ #What does this function?
  if (sum(v) > 0){
    return (v * 100)
  }
  return(v)
}

M <- matrix(rnorm(100), 10, 10)
print (apply(M, 1, SomeOperation))

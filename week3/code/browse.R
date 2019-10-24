#Runs a simulation of exponential growth 
#Runs a vector of length generations 

Exponential <- function(NO = 1, r=1, generations = 10){
  N <- rep(NA, generations) #Creates a vector 
  N[1] <- NO
  for (t in 2:generations){
    N[t] <- N[t-1] * exp(r)
    browser()
  }
  return(N)
}

plot(Exponential(), type = "1", main = "Exponential growth")
#Runs the first iteration of the for loop and the console will enter the browser mode
#Once in browser mode you can debug like in python by running through each line of code 
#E.g. n: single step, c: exit browser and continue, Q: exit browser and abort, return to top level
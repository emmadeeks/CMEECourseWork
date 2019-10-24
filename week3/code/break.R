#Breaking out of loops, often need to break out of loops when a certain conditions is met 
i <- 0 #Initatlise i
while(i <Inf) {
  if (i == 10) {
    break
  } # Break out of the while loop!
  else {
    cat("i equals ", i, "\n")
    i <- i +1 #Update i
  }
}

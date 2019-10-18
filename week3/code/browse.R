Exponential <- function(NO = 1, r=1, generations = 10){
  N <- rep(NA, generations)
  N[1] <- NO
  for (t in 2:generations){
    N[t] <- N[t-1] * exp(r)
    browser()
  }
  return(N)
}

plot(Exponential(), type = "1", main = "Exponential growth")

# Runs the stochastic Ricker equation with gaussian fluctuations
#two scripts, one stochastic Ricker model and another and improved version of this model which is vectorised
#Output: Speed comparion of both scripts

stochrick<-function(p0=runif(1000,.5,1.5),r=1.2,K=1,sigma=0.2,numyears=100)
{
  #initialize
  N<-matrix(NA,numyears,length(p0))
  N[1,]<-p0

  for (pop in 1:length(p0)){#loop through the populations

    for (yr in 2:numyears){ #for each pop, loop through the years

      N[yr,pop] <- N[yr-1,pop] * exp(r * (1 - N[yr - 1,pop] / K) + rnorm(1,0,sigma))

    }

  }
  return(N)

}

print("Stochastic Ricker takes:")
print(system.time(res2<-stochrick()))


###### Improved function ########

stochrickvect<-function(p0=runif(1000,.5,1.5),r=1.2,K=1,sigma=0.2,numyears=100)
{
  #initialize
  N<-matrix(NA,numyears,length(p0))
  N[1,]<-p0

#loop through the populations---- Takes out pop in script
#speeds up loop as its now been vectorised

for (yr in 2:numyears){ #for each pop, loop through the years

      N[yr,] <- N[yr-1,] * exp(r * (1 - N[yr - 1,] / K) + rnorm(1,0,sigma)) #Take out the pop part of the for loop as it is not nessearily needed for the loop and takes extra time

  }

  return(N)
}



# Now write another function called stochrickvect that vectorizes the above
# to the extent possible, with improved performance:

print("Vectorized Stochastic Ricker takes:")
print(system.time(res2<-stochrickvect()))


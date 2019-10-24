#Using try to catch errors in the script 

#Function that rus a simulation that involves sampling from a synthetic population with 
#replacement and takes its mean, but only if at least 30 unique samples are obtained 
doit <- function(x){
	temp_x <- sample(x, replace = TRUE)
	if(length(unique(temp_x)) > 30) {#only take mean if sample was sufficient
		 print(paste("Mean of this sample was:", as.character(mean(temp_x))))
		} 
	else {
		stop("Couldn't calculate mean: too few unique values!")
		}
	}

#Generates a random population 
popn <- rnorm(50)

#Using apply to run the function
lapply(1:15, function(i) doit(popn))

#Using apply and using try 
result <- lapply(1:15, function(i) try(doit(popn), FALSE))

#About is long but it shows you which runs ran into error and why 
result <- vector("list", 15) #Preallocate/Initialize
for(i in 1:15) {
	result[[i]] <- try(doit(popn), FALSE)
	}
#Errors put into variable 
print(result)
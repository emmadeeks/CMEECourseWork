rm(list=ls()) #Clear global environment 
#Set working directory
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/week8/data")

# Get thee required packages
require('minpack.lm')
library('dplyr')
library('tidyr')


######################################## PREPARES DATA ###################
#Explore the data 
data <- read.csv('modified_CRat.csv', header = TRUE) #reads in data

data <- data[, -1] #removes first column 
#Subset data with a nice looking curve to model with 
Data2Fit <- subset(data, ID == 39982) #One curve, this is testing the data with just one curve
# Plot the curve 
plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue) #plotting the curve 

data2 <- data %>%
  nest(data= -ID) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that


####################### OBTAINING STARTING VALUES- OBTAINING STARTING VALUES ######################

a.line <- subset(Data2Fit, ResDensity <= mean(ResDensity))
plot(a.line$ResDensity, a.line$N_TraitValue)

#plot slope/ linear regressopn of cut slope 
lm <- summary(lm(N_TraitValue ~ ResDensity, a.line))
# Extracts slope value
a <- lm$coefficients[2]
# h parameter is the maximum of the slope so you take the biggest value 
h <- max(Data2Fit$N_TraitValue)

q = -1 




#Holling type II functional response
#This is making a function of the second model we looked at 
powMod <- function(x, a, h) { #These are parameters
  return( (a*x ) / (1+ (h*a*x))) # This is the equation
}


#Generalised functional response model

GenMod <- function(x, a, h, q) { #These are parameters
  return( (a* x^(q+1) ) / (1+ (h*a*x^(q+1)))) # This is the equation 
}


################################### FITTING MODELS #############################

PowFit <- nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a=a, h=h)) #as shown in the example document this is how to fit the model using NLLs function in model fitting package

QuaFit <- lm(N_TraitValue ~ poly(ResDensity,2), data = Data2Fit) #only use lm for this as it is just a lm 

GenFit <- nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = Data2Fit, start = list(a=a, h=h, q= q)) #also use minipack for this as well 

############################## PLOTTING MODELS ###################################

Lengths <- seq(min(Data2Fit$ResDensity),max(Data2Fit$ResDensity)) #so it runs through the points 

#as suggested in the model fitting exercise fit the models using the coefficients of the starting values 
Predic2PlotPow <- powMod(Lengths,coef(PowFit)["a"],coef(PowFit)["h"]) #apply the functions and save to a variable
Predic2PlotQua <- predict.lm(QuaFit, data.frame(ResDensity = Lengths))
Predic2PlotGen <- GenMod(Lengths,coef(GenFit)["a"],coef(GenFit)["h"], coef(GenFit)["q"])

# plot this lines onto a plot for visual comparison 
plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)
lines(Lengths, Predic2PlotGen, col = 'green', lwd = 2.5)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)
lines(Lengths, Predic2PlotQua, col = 'red', lwd = 2.5)
legend("topleft", legend = c("Holling Type II", "Quadratic", "Generalised FR"), lwd=2, lty=1, col=2:4)

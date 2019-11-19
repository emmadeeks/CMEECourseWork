rm(list=ls()) #Clear global environment 
#Set working directory
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/week8/code")
data <- read.csv('modified_CRat.csv')
head(data)


Data2Fit <- subset(data, ID == 39982)
plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)

dim(Data2Fit)

#Holling type II functional response

powMod <- function(a, h, x) {
  return( (a*x ) / (1+ (h*a*x)))
}

a.line <- subset(Data2Fit, ResDensity <= mean(ResDensity))
plot(a.line$ResDensity, a.line$N_TraitValue)
lm <- summary(lm(N_TraitValue ~ ResDensity, a.line))
a <- lm$coefficients[2]
h <- max(Data2Fit$N_TraitValue)

PowFit <- nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a=a, h=h))

Lengths <- seq(min(Data2Fit$ResDensity),max(Data2Fit$ResDensity))
coef(PowFit)["a"]
coef(PowFit)["h"]

Predic2PlotPow <- powMod(Lengths,coef(PowFit)["a"],coef(PowFit)["h"])
plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)



# Phenomenological quadratic model 
QuaFit <- lm(N_TraitValue ~ poly(ResDensity,2), data = Data2Fit)
Predic2PlotQua <- predict.lm(QuaFit, data.frame(ResDensity = Lengths))
plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)
lines(Lengths, Predic2PlotQua, col = 'red', lwd = 2.5)


AIC(PowFit) - AIC(QuaFit)

#Generalised functional response model

GenMod <- function(a, h, x, q) {
  return( (a*x^q+1 ) / (1+ (h*a*x^q+1)))
}

q = 0.25
GenFit <- nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = Data2Fit, start = list(a=a, h=h, q=q))

Predic2PlotPow <- GenMod(Lengths,coef(GenFit)["a"],coef(GenFit)["h"], coef(GenFit)["q"])
plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)
lines(Lengths, Predic2PlotPow, col = 'red', lwd = 2.5)







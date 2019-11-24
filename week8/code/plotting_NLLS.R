rm(list=ls()) #Clear global environment 
#Set working directory
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/week8/data")

# Get thee required packages
require('minpack.lm')
library('dplyr')
library('tidyr')


######################################## PREPARES DATA ###################
#Explore the data 
data <- read.csv('modified_CRat.csv', header = TRUE)

data <- data[, -1]
#Subset data with a nice looking curve to model with 
Data2Fit <- subset(data, ID == 39982) #One curve
# Plot the curve 
plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)

data2 <- data %>%
  nest(-ID)



###################################### OBTAINING STARTING VALUES ###############

#This is basically cutting the last points of the graph off 
# After the highest point 
# Because a needs to fit to just the slope
paraopt = as.data.frame(matrix(nrow = 1, ncol = 4))
finalframe = as.data.frame(matrix(nrow = 1, ncol = 4))
for (ii in data2$data[[1]]){
  a.line <- subset(ii, ResDensity <= mean(ResDensity))
  lm <- summary(lm(ii$N_TraitValue ~ ResDensity, a.line))
  a <- lm$coefficients[2]
  h <- max(ii$N_TraitValue)
  for(i in 1:100){
    anew = rnorm(1, mean = a, sd=1)
    hnew = rnorm(1, mean = h, sd=1)
    PowFit <- nlsLM(N_TraitValue ~ powMod(data2$data$ResDensity, a, h), data = Data2Fit, start = list(a= anew, h= hnew))
    AIC = AIC(PowFit)
    p <- c(data2$ID[[i]], anew, hnew, AIC)
    paraopt = rbind(paraopt, p)
    AICmin <- paraopt[which.min(paraopt$V4), ]
  tt <- c(AICmin)
  finalframe = rbind(finalframe, tt)
  }
}


for (ii in data2){
  a.line <- subset(ii, ResDensity <= mean(ResDensity))
  lm <- summary(lm(ii$N_TraitValue ~ ResDensity, a.line))
  a <- lm$coefficients[2]
  h <- max(ii$N_TraitValue)

for (ID in data2){
  print(ID)
  a.line <- subset(Data2Fit, ResDensity <= mean(ResDensity))
  lm <- summary(lm(N_TraitValue ~ ResDensity, a.line))
  a <- lm$coefficients[2]
  h <- max(Data2Fit$N_TraitValue
}

for (j in data2$ID){
  for (i in data2$data[[j]]){
    print(i)
   }
}

min_values <- paraopt[which.min(paraopt$V4), ]

AICmin <- paraopt[which.min(paraopt$V4), ]
hN <- min_values$V3
aN <- min_values$V2


for(i in 1:100){
  anew = rnorm(1, mean = a, sd=1)
  hnew = rnorm(1, mean = h, sd=1)
  PowFit <- nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a= anew, h= hnew))
  AIC = AIC(PowFit)
  p <- c(i, anew, hnew, AIC)
  paraopt = rbind(paraopt, p)
}





a.line <- subset(Data2Fit, ResDensity <= mean(ResDensity))
plot(a.line$ResDensity, a.line$N_TraitValue)

#plot slope/ linear regressopn of cut slope 
lm <- summary(lm(N_TraitValue ~ ResDensity, a.line))
# Extracts slope value
a <- lm$coefficients[2]
# h parameter is the maximum of the slope so you take the biggest value 
h <- max(Data2Fit$N_TraitValue)

q = -1 

################################# OPTIMISING STARTING VALUES ##################

paraopt = as.data.frame(matrix(nrow = 1, ncol = 4))




for(i in 1:100){
  anew = rnorm(1, mean = a, sd=1)
  hnew = rnorm(1, mean = h, sd=1)
  PowFit <- nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a= anew, h= hnew))
  AIC = AIC(PowFit)
  p <- c(i, anew, hnew, AIC)
  paraopt = rbind(paraopt, p)
}

min_values <- paraopt[which.min(paraopt$V4), ]


hN <- min_values$V3
aN <- min_values$V2



####################################### DEFINING FUNCTIONS OF MODELS ##############

#Holling type II functional response
#This is making a function of the second model we looked at 
powMod <- function(x, a, h) { #These are parameters
  return( (a*x ) / (1+ (h*a*x))) # This is the equation
}


#Generalised functional response model

GenMod <- function(x, a, h, q) { #These are parameters
  return( (a* x^(q+1) ) / (1+ (h*a*x^(q+1))))
}


################################### FITTING MODELS #############################

PowFit <- nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a=a, h=h))

QuaFit <- lm(N_TraitValue ~ poly(ResDensity,2), data = Data2Fit)

GenFit <- nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = Data2Fit, start = list(a=a, h=h, q= q))

############################## PLOTTING MODELS ###################################

Lengths <- seq(min(Data2Fit$ResDensity),max(Data2Fit$ResDensity))

Predic2PlotPow <- powMod(Lengths,coef(PowFit)["a"],coef(PowFit)["h"])
Predic2PlotQua <- predict.lm(QuaFit, data.frame(ResDensity = Lengths))
Predic2PlotGen <- GenMod(Lengths,coef(GenFit)["a"],coef(GenFit)["h"], coef(GenFit)["q"])


plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)
lines(Lengths, Predic2PlotGen, col = 'green', lwd = 2.5)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)
lines(Lengths, Predic2PlotQua, col = 'red', lwd = 2.5)

########################## LOOPING FUNCTIONS ############

################ CORRECT FOR LOOP FOR RUNNING THROUGH ALL FUNCTIONS MAYBE #########
data2 <- data %>%
  nest(-ID)


modelvec<-c("PowFit","QuaFit","GenFit")
modelvec<-data.frame("Model"=modelvec,
              "AIC"=rep(NA,length(modelvec)),
              "BIC"=rep(NA,length(modelvec)))
for(i in 1:length(data2$data)){
  datatry <- data2$data[[i]]
  PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
  QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
  GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
  
  modelvec[1,2:3]<-ifelse(class(PowFit)=="try-error",rep(NA,2),c(AIC(PowFit),BIC(PowFit)))
  modelvec[2,2:3]<-ifelse(class(QuaFit)=="try-error",rep(NA,2),c(AIC(QuaFit),BIC(QuaFit)))
  modelvec[3,2:3]<-ifelse(class(GenFit)=="try-error",rep(NA,2),c(AIC(GenFit),BIC(GenFit)))
  
  cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
  
}


####################################### NOTES ############################

for(i in 1:length(data$ID)){
  anew = rnorm(1, mean = a, sd=1)
  hnew = rnorm(1, mean = h, sd=1)
  PowFit <- nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a= anew, h= hnew))
  QuaFit <- lm(N_TraitValue ~ poly(ResDensity,2), data = Data2Fit)
  GenFit <- nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = Data2Fit, start = list(a=a, h=h, q= q))
  AIC = AIC(PowFit)
  p <- c(i, anew, hnew, AIC)
  paraopt = rbind(paraopt, p)
}

################### CORRECT FORLOOP FOR RUNNING THROUGH FUNCTIONS #######
trydata = as.data.frame(matrix(nrow = 1, ncol = 3))
for(i in 1:length(data2$data)){
  datatry <- data2$data[[i]]
  PowFit <- nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h))
  AIC = AIC(PowFit)
  BIC = BIC(PowFit)
  IDtry = data2$ID[[i]]
  p <- c(IDtry, AIC, BIC)
  trydata = rbind(trydata, p)
}






# AICHolling = AIC(PowFit)
# BICHolling = BIC(PowFit)
# AICQuadratic = AIC(QuaFit)
# BICQuadratic = BIC(QuaFit)
# AICGeneralised = AIC(GenFit)
# BICGeneralised = BIC(GenFit)
# min_valuesAIC <- min(AICHolling, AICQuadratic, AICGeneralised)
# IDtry = data2$ID[[i]]
# p <- c(IDtry, min_valuesAIC, model)
# trydata = rbind(trydata, p)



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



# distinct
alldata <- data %>% 
  distinct(ID, .keep_all = T)

alldata <- subset(data, data$ID)

for (id in unique.id){
  curr_data <- subset(data, ID== id)
  print(paste())
}


for (i in data){
  Data2Fittry <- subset(data, ID == i)
}


for (i in data){
  Data2Fittry <- subset(data, ID == i)
  PowFit <- nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a=a, h=h))
  
  QuaFit <- lm(N_TraitValue ~ poly(ResDensity,2), data = Data2Fit)
  
  GenFit <- nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = Data2Fit, start = list(a=a, h=h, q= q))
  Lengths <- seq(min(Data2Fit$ResDensity),max(Data2Fit$ResDensity))
  
  
}


for (id in data){
  a.line <- subset(Data2Fit, ResDensity <= mean(ResDensity))
  plot(a.line$ResDensity, a.line$N_TraitValue)
  
  #plot slope/ linear regressopn of cut slope 
  lm <- summary(lm(N_TraitValue ~ ResDensity, a.line))
  # Extracts slope value
  a <- lm$coefficients[2]
  # h parameter is the maximum of the slope so you take the biggest value 
  h <- max(Data2Fit$N_TraitValue)
  
  q = -1 
}



Predic2PlotPow <- powMod(Lengths,coef(PowFit)["a"],coef(PowFit)["h"])
Predic2PlotQua <- predict.lm(QuaFit, data.frame(ResDensity = Lengths))
Predic2PlotGen <- GenMod(Lengths,coef(GenFit)["a"],coef(GenFit)["h"], coef(GenFit)["q"])

plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)
lines(Lengths, Predic2PlotGen, col = 'green', lwd = 2.5)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)
lines(Lengths, Predic2PlotQua, col = 'red', lwd = 2.5)


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



################ CORRECT FOR LOOP FOR RUNNING THROUGH ALL FUNCTIONS MAYBE #########
a<-c("PowFit","QuaFit","GenFit")
a<-data.frame("Model"=a,
              "AIC"=rep(NA,length(a)),
              "BIC"=rep(NA,length(a)))
for(i in 1:length(data2$data)){
  datatry <- data2$data[[i]]
  PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
  QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
  GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
  
  a[1,2:3]<-ifelse(class(PowFit)=="try-error",rep(NA,2),c(AIC(PowFit),BIC(PowFit)))
  a[2,2:3]<-ifelse(class(QuaFit)=="try-error",rep(NA,2),c(AIC(QuaFit),BIC(QuaFit)))
  a[3,2:3]<-ifelse(class(GenFit)=="try-error",rep(NA,2),c(AIC(GenFit),BIC(GenFit)))
  
  cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",a[which(a$AIC==min(a$AIC,na.rm = T)),1]," ; min BIC:",a[which(a$BIC==min(a$BIC,na.rm = T)),1],"\n"))

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



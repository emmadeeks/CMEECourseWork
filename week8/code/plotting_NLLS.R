rm(list=ls()) #Clear global environment 
#Set working directory
<<<<<<< HEAD
#setwd("/Users/emmadeeks/Desktop/CMEECourseWork/week8/data")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
=======
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/week8/data")

>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
# Get thee required packages
require('minpack.lm')
library('dplyr')
library('tidyr')


######################################## PREPARES DATA ###################
<<<<<<< HEAD
#Load in data
data <- read.csv('modified_CRat.csv', header = TRUE) #reads in data

data <- data[, -1] #removes first column 

data2 <- data %>%
  nest(data= -ID) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

########################  CREATE FUNCTION FOR OBTAINING INITIAL START VALUES ###########

# A function that is designed to find the initial starting values before optimisation 
# 
obtaining_start_values <- function(data) {
  ### Store the maximum handling time into a variable called h 
  # Store maximum y value
  h <- max(data$N_TraitValue)
  ### Remove points after this maximum handling time to fit models to the growth period
  # Find mean y value
  meanTraitValue <- mean(data$N_TraitValue)
  # Find the maximum y value
  maxTraitValue <- max(data$N_TraitValue)
  # Subset the data to contain only x values lower than the mean and maximum traitvalues 
  DataBelowMean <- subset(data, N_TraitValue < meanTraitValue)
  DataBelowMax <- subset(data, N_TraitValue < maxTraitValue)
  ### Calculate the linear regression of the cut slopes
  lmMean <- summary(lm(N_TraitValue ~ ResDensity, DataBelowMean))
  lmMax <- summary(lm(N_TraitValue ~ ResDensity, DataBelowMax))
  ### Store the search rate
  # Store the best value for the gradient
  if (lmMean$r.squared >= lmMax$r.squared){
    a <- lmMean$coefficients[2]
  } else {a <- lmMax$coefficients[2]}
  ### Store a and h values in a list to return
  ah <- c(a, h)
  ### Return the a and h values
  return(ah)
=======
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
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}



<<<<<<< HEAD
############# INPUTTING STARTING VALUES INTO ##########################

# This for loop applies the function on the data read in to find initial starting values 

finalframe = as.data.frame(matrix(nrow = 1, ncol = 3)) # Make dataframe to put starting values into 


for(i in 1:length(data2$ID)){
  toadd <- c() # intiialise empty vector 
  datatry <- data2$data[[i]] #index the data and save to variable- indexing withing nested data
  ID <- data2$ID[[i]] #save the id 
  okay <- obtaining_start_values(datatry) #apply the function on datatry
  toadd <- c(ID, okay[1], okay[2]) #create vector of values to add to dataframe 
  finalframe <- rbind(finalframe, toadd) # bind rows 
}

datatouse <- finalframe[-1, ]  # remove first row 

datcol<-c("ID","a","h") #add labels 
colnames(datatouse) <- datcol


######################### DEFINING FUNCTIONS OF MODELS ##############
=======


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
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7

#Holling type II functional response
#This is making a function of the second model we looked at 
powMod <- function(x, a, h) { #These are parameters
  return( (a*x ) / (1+ (h*a*x))) # This is the equation
}


#Generalised functional response model

GenMod <- function(x, a, h, q) { #These are parameters
<<<<<<< HEAD
  return( (a* x^(q+1) ) / (1+ (h*a*x^(q+1)))) # This is the equation 
}


data2 <- merge(data2, datatouse, by= "ID")



############## OPTIMISING STARTING VALUES ##################

## Create empty dataframe to put optimised starting values in 
optimising <-data.frame("ID"= NA,
                    "a"= NA,
                   "h"= NA, "minAIC"= NA, stringsAsFactors = T)

# for loop to optimise start values 
for(i in 1:length(data2$data)){
  first <- data2[i,] #select each data category 
  #and fit the model to it like before in the example 
  # use try because it stops any null values being included
  a <- first$a #make a variable
  h <- first$h #make h variable 
  datatry <- data2$data[[i]] # put subsection of data into a variable name 
  avalues <- rnorm(10, a, 10)  #distribute values randomly 
  hvalues <- rnorm(10, h, 10) # randomly distribute values in order to sample best starting values 
  id <- first$ID # get id for each ID saved 
  TempTable1 <- data.frame("avalues" = NA, "hvalues" = NA, "AIC_values" = NA) ## Create a table for the Holling Type II model
  TempTable2 <- data.frame("avalues" = NA, "hvalues" = NA, "AIC_values" = NA) ## Create a table for the generalised mdoel 
  for (j in 1:10){
    ### Fit models
    AICHol <- c() #initialise vectors 
    AICGen <- c()
    a <- avalues[j] #got hrough each of the 10 random a and h values 
    h <- hvalues[j]
    q <- 0.78
    # Apply model functions with try construct to catch errors 
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q=q)), silent=T)
    ### Tell for loop to ignore errors and input NA into table and if no error then calculate AIC of the model 
    AICHol <- ifelse(class(PowFit) == "try-error", NA, AIC(PowFit))
    AICGen <- ifelse(class(GenFit) == "try-error", NA, AIC(GenFit))
    # Put values into the table in order to sample and find lowest AIC later 
    temptable1 <- list(a, h, AICHol)
    TempTable1 <- rbind(TempTable1, temptable1)
    temptable2 <- list(a, h, AICGen)
    TempTable2 <- rbind(TempTable2, temptable2)
  }
  # find the minimum AIC value for the optimising table 
  TempTable1 <- rbind(TempTable1, TempTable2)
  minAIC <- TempTable1[which.min(TempTable1$AIC_values),]
  min1 <- minAIC[1]
  min2 <- minAIC[2]
  min3 <- minAIC[3]
  add <- list(id, min1, min2, min3)
  optimising <- rbind(optimising, add)
}




################################# OPTIMISING STARTING VALUES AND PUTTING THROUGH CODE AGAIN ############

dataagain <- data %>%
  nest(data= -ID) 
dataoptimised <- merge(dataagain, optimising, by= "ID")


# create two new columns and convert into a data frame with the AICs and BICs 
modelvec2<-c("PowFit","QuaFit","GenFit", "CubFit")
modelvec2<-data.frame("ID "=rep(NA),
                     "Habitat"=rep(NA),
                     "ResDimension"=rep(NA),
                     "ConDimension"=rep(NA),
                     "Model"=modelvec2,
                     "AIC"=rep(NA),
                     "BIC"=rep(NA))

modelvec_back<-c("PowFit","QuaFit","GenFit", "CubFit") 

# create two new columns and convert into a data frame with the AICs and BICs 
modelvec_back<-data.frame("Model"=modelvec_back,
                     "AIC"=rep(NA),
                     "BIC"=rep(NA))


# Create new data frame to store values
optimisedtable <- c("ID", "a", "h", "minAIC", "minBIC", "AIC", "BIC", "Habitat", "Res_Dim", "Con_Dim")
optimisedtable<-data.frame("ID"= NA,
                    "a"= NA,
                    "h"= NA, "minAIC"= NA, "minBIC"= NA, "AIC"= NA, "BIC"= NA, "Habitat"= NA, "Res_Dim" = NA, "Con_Dim" = NA, stringsAsFactors = T)


for(i in 1:length(dataoptimised$data)){
  index <- c()
  add <- c()
  datatry <- dataoptimised$data[[i]] #select each data category 
  #and fit the model to it like before in the example 
  # use try because it stops any null values being included
  a <- dataoptimised$a[i]
  h <- dataoptimised$h[i]
  habitat <- datatry$Habitat[1]
  res_dim <- datatry$Res_MovementDimensionality[1]
  con_dim <- datatry$Con_MovementDimensionality[1]
  q = 0.78
  modelvec<-c("PowFit","QuaFit","GenFit", "CubFit")
  modelvec<-data.frame("ID "=rep(NA),
                       "Habitat"=rep(NA),
                       "ResDimension"=rep(NA),
                       "ConDimension"=rep(NA),
                       "Model"=modelvec,
                       "AIC"=rep(NA),
                       "BIC"=rep(NA))
  datatry <- dataoptimised$data[[i]] #select each data category 
  #and fit the model to it like before in the example 
  # use try because it stops any null values being included
  a <- dataoptimised$a[i]
  h <- dataoptimised$h[i]
  q = 0.78
  PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
  QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
  GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q=q)), silent=T)
  CubFit <- try(lm(N_TraitValue ~ poly(ResDensity,3), data = datatry), silent = T)
  #select each index of the modelvec dataframe, one for each model 
  # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
  modelvec[1,6]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (AIC(PowFit)))
  modelvec[2,6]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (AIC(QuaFit)))
  modelvec[3,6]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (AIC(GenFit)))
  modelvec[4,6]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (AIC(CubFit)))
  modelvec[1,7]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (BIC(PowFit)))
  modelvec[2,7]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (BIC(QuaFit)))
  modelvec[3,7]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (BIC(GenFit)))
  modelvec[4,7]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (BIC(CubFit)))
  modelvec[1:4,1] <- data2$ID[[i]]
  modelvec[1:4,2] <- datatry$Habitat[[1]]
  modelvec[1:4,3] <- datatry$Res_MovementDimensionality[[1]]
  modelvec[1:4,4] <- datatry$Con_MovementDimensionality[[1]]
  modelvec_back[1,2]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (AIC(PowFit)))
  modelvec_back[2,2]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (AIC(QuaFit)))
  modelvec_back[3,2]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (AIC(GenFit)))
  modelvec_back[4,2]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (AIC(CubFit)))
  modelvec_back[1,3]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (BIC(PowFit)))
  modelvec_back[2,3]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (BIC(QuaFit)))
  modelvec_back[3,3]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (BIC(GenFit)))
  modelvec_back[4,3]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (BIC(CubFit)))
  okAIC <- modelvec_back[which(modelvec_back$AIC==min(modelvec_back$AIC,na.rm = T)), 1]
  okBIC <- modelvec_back[which(modelvec_back$BIC==min(modelvec_back$BIC,na.rm = T)), 1]
  okAIC <- as.character(okAIC[1][[1]])
  okBIC <- as.character(okBIC[1][[1]])
  minAIC <- min(modelvec_back[,2], na.rm=T)
  minBIC <- min(modelvec_back[,3], na.rm=T)
  add <- list(data2$ID[[i]], a, h, minAIC, minBIC, okAIC[1], okBIC[1], habitat, res_dim, con_dim)
  optimisedtable <- rbind(optimisedtable, add)
  modelvec2 <- bind_rows(modelvec2, modelvec)
  
  
}

for (i in 1:4){
  modelvec2 <- modelvec2[-1, ]
}



optimisedtable$mecphe <- ifelse(grepl("QuaFit|CubFit", optimisedtable$AIC), "Phenomenological", "Mechanistic")

optimisedtable <- optimisedtable[-1, ]

optimisedtable <- apply(optimisedtable,2,as.character)

write.csv(optimisedtable, file = "optimisedtable.csv")
write.csv(modelvec2, file = "MergedOptTable.csv")



################# MAKING EXAMPLE PLOTS FOR METHODS #########
#Subset data with a nice looking curve to model with 
Data2Fit <- subset(data, ID == 39982) #One curve
# Plot the curve 
#plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)
#This is basically cutting the last points of the graph off 
# After the highest point 
# Because a needs to fit to just the slope
a.line <- subset(Data2Fit, ResDensity <= mean(ResDensity))
#plot(a.line$ResDensity, a.line$N_TraitValue)

lm <- summary(lm(N_TraitValue ~ ResDensity, a.line))
# Extracts slope value
a <- lm$coefficients[2]
# h parameter is the maximum of the slope so you take the biggest value 
h <- max(Data2Fit$N_TraitValue)
q = 0.78
#PowFit <- nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a=a, h=h))

# optimising a and h values  
Lengths <- seq(min(Data2Fit$ResDensity),max(Data2Fit$ResDensity))


PowFit <- (nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a=a, h=h)))
QuaFit <- (lm(N_TraitValue ~ poly(ResDensity,2), data = Data2Fit))
GenFit <- (nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = Data2Fit, start = list(a=a, h=h, q=q)))
CubFit <- (lm(N_TraitValue ~ poly(ResDensity,3), data = Data2Fit))

Predic2PlotPow <- powMod(Lengths,coef(PowFit)["a"],coef(PowFit)["h"])
Predic2PlotQua <- predict.lm(QuaFit, data.frame(ResDensity = Lengths))
Predic2PlotCub <- predict.lm(CubFit, data.frame(ResDensity = Lengths))
Predic2PlotGen <- GenMod(Lengths,coef(GenFit)["a"],coef(GenFit)["h"], coef(GenFit)["q"])

plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue)
lines(Lengths, Predic2PlotGen, col = 'green', lwd = 2.5)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)
lines(Lengths, Predic2PlotQua, col = 'red', lwd = 2.5)
lines(Lengths, Predic2PlotCub, col = 'pink', lwd = 2.5)
=======
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


>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7

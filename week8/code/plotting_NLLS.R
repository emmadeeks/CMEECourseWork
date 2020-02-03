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

data2 <- data %>%
  nest(data= -ID) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that


obtaining_start_values <- function(data){
h <- c()
removeddata <- c()
CurveData <- c()
lm <- c()
a <- c()
### Identify the peak of the curve (This is equal to the peak handling time)
### Store the peak handling time in a variable (h)
h <- max(data$N_TraitValue)
### Remove points after this value in order to fit models to the growth period
# Store x value for corresponding peak y value
removeddata = data$ResDensity[which.max(data$N_TraitValue)]
# Subset the data to contain only x values lower than this point
CurveData <- subset(data, ResDensity < removeddata)
### Calculate the linear regression of the cut slope
lm <- summary(lm(N_TraitValue ~ ResDensity, CurveData))
### Extract the value for the gradient, which is equal to the search rate
a <- lm$coefficients[2]
return(c(a, h))
} 

############# INPUTTING STARTING VALUES INTO 
finalframe = as.data.frame(matrix(nrow = 1, ncol = 3))
for(i in 1:length(data2$ID)){
  toadd <- c()
  datatry <- data2$data[[i]]
  ID <- data2$ID[[i]]
  okay <- obtaining_start_values(datatry)
  toadd <- c(ID, okay[1], okay[2])
  finalframe <- rbind(finalframe, toadd)
}

datatouse <- finalframe[-1, ]  

datcol<-c("ID","a","h")
colnames(datatouse) <- datcol
#datatouse <- na.omit(datatouse)

####################################### DEFINING FUNCTIONS OF MODELS ##############

#Holling type II functional response
#This is making a function of the second model we looked at 
powMod <- function(x, a, h) { #These are parameters
  return( (a*x ) / (1+ (h*a*x))) # This is the equation
}


#Generalised functional response model

GenMod <- function(x, a, h, q) { #These are parameters
  return( (a* x^(q+1) ) / (1+ (h*a*x^(q+1)))) # This is the equation 
}



################ CORRECT FOR LOOP FOR RUNNING THROUGH ALL FUNCTIONS MAYBE #########
# nest the data to id 
#data2 <- data %>%
 # nest(-ID)

data2 <- merge(data2, datatouse, by= "ID")

#create a vector with model names 
modelvec<-c("PowFit","QuaFit","GenFit") 

# create two new columns and convert into a data frame with the AICs and BICs 
modelvec<-data.frame("Model"=modelvec,
                     "AIC"=rep(NA,length(modelvec)),
                     "BIC"=rep(NA,length(modelvec)))

#paraopt = as.data.frame(matrix(nrow = 1, ncol = 7), stringsAs)
# Create new data frame to store values
paraopt <- c("ID", "a", "h", "minAIC", "minBIC", "AIC", "BIC")
paraopt<-data.frame("ID"= NA,
                    "a"= NA,
                    "h"= NA, "minAIC"= NA, "minBIC"= NA, "AIC"= NA, "BIC"= NA, stringsAsFactors = T)

#col <- c("ID", "a", "h", "min AIC", "min BIC", "AIC", "BIC")
#colnames(paraopt) <- col
#paraopt <- paraopt[-1 , ]


for(i in 1:length(data2$data)){
  index <- c()
  add <- c()
  datatry <- data2$data[[i]] #select each data category 
  #and fit the model to it like before in the example 
  # use try because it stops any null values being included
  a <- data2$a[i]
  h <- data2$h[i]
  q = -1
  PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
  QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
  GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
  #CubFit <- try(lm(N_TraitValue ~ poly(ResDensity,3), data = datatry), silent = T)
  #select each index of the modelvec dataframe, one for each model 
  # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
  modelvec[1,2]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (AIC(PowFit)))
  modelvec[2,2]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (AIC(QuaFit)))
  modelvec[3,2]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (AIC(GenFit)))
  #modelvec[4,2]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (AIC(CubFit)))
  modelvec[1,3]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (BIC(PowFit)))
  modelvec[2,3]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (BIC(QuaFit)))
  modelvec[3,3]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (BIC(GenFit)))
  #modelvec[4,3]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (BIC(CubFit)))
  okAIC <- modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)), 1]
  okBIC <- modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)), 1]
  okAIC <- as.character(okAIC[1][[1]])
  okBIC <- as.character(okBIC[1][[1]])
  #index <- apply(modelvec,2, min)
  minAIC <- min(modelvec[,2], na.rm=T)
  minBIC <- min(modelvec[,3], na.rm=T)
  add <- c(data2$ID[[i]], a, h, okAIC, okBIC, minAIC, minBIC)
  #add <- ifelse(okAIC[1]==length(1), c(data2$ID[[i]], a, h, okAIC[1], okBIC[1], index[2], index[3]), )
  #tryCatch(, warning = function(c) print(paste("warning on id", i)))
  paraopt <- rbind(paraopt, add)
  #cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
  
}







############## OPTIMISING STARTING VALUES ##################
optimising <-data.frame("ID"= NA,
                    "a"= NA,
                   "h"= NA, "minAIC"= NA, stringsAsFactors = T)

#data2 <- merge(data2, datatouse, by= "ID")

for(i in 1:length(data2$data)){
  first <- data2[i,] #select each data category 
  #and fit the model to it like before in the example 
  # use try because it stops any null values being included
  a <- first$a #make a variable
  h <- first$h #make h variable 
  datatry <- data2$data[[i]] # put subsection of data into a variable name 
  avalues <- rnorm(10, a, 10)  #distribute values randomly 
  hvalues <- rnorm(10, h, 10)
  id <- first$ID
  TempTable <- data.frame("avalues" = NA, "hvalues" = NA, "AIC_Hol_values" = NA)
  for (j in 1:10){
    ### Fit models
    AICHol <- c()
    a <- avalues[j]
    h <- hvalues[j]
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    # Holling II model
    AICHol <- ifelse(class(PowFit) == "try-error", NA, AIC(PowFit))
    temptable <- c(a, h, AICHol)
    TempTable <- rbind(TempTable, temptable)
  }
  minAIC <- min(TempTable[,3], na.rm = T)
  add <- c(id, a, h, minAIC)
  optimising <- rbind(optimising, add)
}




################################# OPTIMISING STARTING VALUES AND PUTTING THROUGH CODE AGAIN ############
data <- read.csv('modified_CRat.csv', header = TRUE) #reads in data

data <- data[, -1] #removes first column 


dataagain <- data %>%
  nest(data= -ID) 
dataoptimised <- merge(dataagain, optimising, by= "ID")


#create a vector with model names 
modelvec<-c("PowFit","QuaFit","GenFit") 

# create two new columns and convert into a data frame with the AICs and BICs 
modelvec<-data.frame("Model"=modelvec,
                     "AIC"=rep(NA,length(modelvec)),
                     "BIC"=rep(NA,length(modelvec)))

optimisedtable<-data.frame("ID"= NA,
                    "a"= NA,
                    "h"= NA, "minAIC"= NA, "minBIC"= NA, "AIC"= NA, "BIC"= NA, stringsAsFactors = T)

for(i in 1:length(dataoptimised$data)){
  index <- c()
  add <- c()
  datatry <- dataoptimised$data[[i]] #select each data category 
  #and fit the model to it like before in the example 
  # use try because it stops any null values being included
  a <- dataoptimised$a[i]
  h <- dataoptimised$h[i]
  q = -1
  PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
  QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
  GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
  #select each index of the modelvec dataframe, one for each model 
  # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
  modelvec[1,2]<-ifelse(class(PowFit)=="try-error",rep(NA,2), (AIC(PowFit)))
  modelvec[2,2]<-ifelse(class(QuaFit)=="try-error",rep(NA,2), (AIC(QuaFit)))
  modelvec[3,2]<-ifelse(class(GenFit)=="try-error",rep(NA,2), (AIC(GenFit)))
  modelvec[1,3]<-ifelse(class(PowFit)=="try-error",rep(NA,2), (BIC(PowFit)))
  modelvec[2,3]<-ifelse(class(QuaFit)=="try-error",rep(NA,2), (BIC(QuaFit)))
  modelvec[3,3]<-ifelse(class(GenFit)=="try-error",rep(NA,2), (BIC(GenFit)))
  okAIC <- modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)), 1]
  okBIC <- modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)), 1]
  okAIC <- as.character(okAIC[1][[1]])
  okBIC <- as.character(okBIC[1][[1]])
  #index <- apply(modelvec,2, min)
  minAIC <- min(modelvec[,2], na.rm=T)
  minBIC <- min(modelvec[,3], na.rm=T)
  add <- c(data2$ID[[i]], a, h, okAIC, okBIC, minAIC, minBIC)
  #add <- ifelse(okAIC[1]==length(1), c(data2$ID[[i]], a, h, okAIC[1], okBIC[1], index[2], index[3]), )
  #tryCatch(, warning = function(c) print(paste("warning on id", i)))
  optimisedtable <- rbind(optimisedtable, add)
  #cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
  
}



optimisedtable %>% group_by(optimisedtable$minAIC) %>% summarise(count=n())

paraopt %>% group_by(paraopt$minAIC) %>% summarise(count=n())




######################## NOTES #################################

PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, startingvalue, hvalue), data = datatry, start = list(a=startingvalue, h=hvalue)), silent=T)
a <- first$a
h <- first$h
data <- first$data[[1]]
PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)






for(i in 1:length(data2$data)){
  datatry <- data2$data[[i]] #select each data category 
  #and fit the model to it like before in the example 
  # use try because it stops any null values being included
  a <- data2$a[i]
  h <- data2$h[i]
  avalues <- rnorm(10, a, 0.00000000001)
  hvalues <- rnorm(10, h, 0.000000000001)
  id <- data2$ID[i]
  TempTable <- data.frame("avalues" = NA, "hvalues" = NA, "AIC_Hol_values" = NA)
  for (i in 1:length(avalues)){
    ### Fit models
    AICHol <- c()
    anew <- avalues[i]
    hnew <- hvalues[i]
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, anew, hnew), data = datatry, start = list(a=anew, h=hnew)), silent=T)
    # Holling II model
    AICHol <- ifelse(class(PowFit) == "try-error", NA, AIC(PowFit))
    temptable <- c(anew, hnew, AICHol)
    TempTable <- rbind(TempTable, temptable)
  }
  minAIC <- min(TempTable[,3])
  add <- c(id, a, h, minAIC)
  optimising <- rbind(optimising, add)
}

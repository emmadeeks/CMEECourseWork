rm(list=ls()) #Clear global environment 
#Set working directory

#setwd("/Users/emmadeeks/Desktop/CMEECourseWork/week8/data")
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#setwd("/Users/emmadeeks/Desktop/CMEECourseWork/week8/code")

# Get thee required packages
require('minpack.lm')
library('dplyr')
library('tidyr')


######################################## PREPARES DATA ###################

#Load in data
data <- read.csv('../data/modified_CRat.csv', header = TRUE) #reads in data

data <- data[, -1] #removes first column 

data2 <- data %>%
  nest(data= -ID) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

########################  CREATE FUNCTION FOR OBTAINING INITIAL START VALUES ###########

# A function that is designed to find the initial starting values before optimisation 


obtaining_start_values <- function(data) {
  ### Store the peak handling time in a variable (h)
  # Store maximum y value
  h <- max(data$N_TraitValue)
  ### Remove points after this value in order to fit models to the growth period
  # Find mean y value
  meanTraitValue <- mean(data$N_TraitValue)
  # Find max y value
  maxTraitValue <- max(data$N_TraitValue)
  # Subset the data to contain only x values lower than these point
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
}



############# INPUTTING STARTING VALUES INTO 
finalframe = as.data.frame(matrix(nrow = 1, ncol = 3))
suppressWarnings(
for(i in 1:length(data2$ID)){
  toadd <- c()
  datatry <- data2$data[[i]]
  ID <- data2$ID[[i]]
  okay <- obtaining_start_values(datatry)
  toadd <- c(ID, okay[1], okay[2])
  finalframe <- rbind(finalframe, toadd)
}
)
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



############## OPTIMISING STARTING VALUES ##################
optimising <-data.frame("ID"= NA,
                        "a"= NA,
                        "h"= NA, "minAIC"= NA, stringsAsFactors = T)

#data2 <- merge(data2, datatouse, by= "ID"
suppressWarnings(
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
  TempTable1 <- data.frame("avalues" = NA, "hvalues" = NA, "AIC_values" = NA)
  TempTable2 <- data.frame("avalues" = NA, "hvalues" = NA, "AIC_values" = NA)
  for (j in 1:10){
    ### Fit models
    AICHol <- c()
    AICGen <- c()
    a <- avalues[j]
    h <- hvalues[j]
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
    # Holling II model
    #AICgen <- ifelse(class(GenFit) == "try-error", NA, AIC(GenFit))
    AICHol <- ifelse(class(PowFit) == "try-error", NA, AIC(PowFit))
    AICGen <- ifelse(class(GenFit) == "try-error", NA, AIC(GenFit))
    temptable1 <- list(a, h, AICHol)
    TempTable1 <- rbind(TempTable1, temptable1)
    temptable2 <- list(a, h, AICGen)
    TempTable2 <- rbind(TempTable2, temptable2)
  }
  #minAIC <- min(TempTable[,3], na.rm = T)
  #minAIC <- TempTable[which(TempTable[3] == minAIC), 1:2]
  TempTable1 <- rbind(TempTable1, TempTable2)
  minAIC <- TempTable1[which.min(TempTable1$AIC_values),]
  #minAIC2 <- TempTable2[which.min(TempTable2$AIC_Gen_values),]
  #newmin <- rbind(minAIC1, minAIC2)
  #finalmin <- newmin[which.min[3],]
  min1 <- minAIC[1]
  min2 <- minAIC[2]
  min3 <- minAIC[3]
  add <- list(id, min1, min2, min3)
  optimising <- rbind(optimising, add)
}
)


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

suppressWarnings(
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
)
for (i in 1:4){
  modelvec2 <- modelvec2[-1, ]
}



optimisedtable$mecphe <- ifelse(grepl("QuaFit|CubFit", optimisedtable$AIC), "Phenomenological", "Mechanistic")

optimisedtable <- optimisedtable[-1, ]

optimisedtable <- apply(optimisedtable,2,as.character)

write.csv(optimisedtable, file = "../data/optimisedtable.csv")
write.csv(modelvec2, file = "../data/MergedOptTable.csv")




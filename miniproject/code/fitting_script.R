rm(list=ls()) #Clear global environment 
#This script assumes the users working directory is in the 'Code' directory of the miniproject

#!/usr/bin/env Rscript 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: fitting_script.R
#Desc: Script that fits the four models to the functional response curves and outputs two tables, one approriate for statistics and the other approriate for plotting. also outputs a sample plot for the methods section on the curve fitting	
#Arguments: Takes as input the 'modified_CRat.csv' script created in the preparing_data.py script
#Outputs: Two tables, one entitled 'optiisedtable.csv' and the other 'MergedOptTable.csv' into the data directory of the miniproject directory to be used by the potting_script.py. Also produces plot in results directory for the latex document 'miniproject_write.tex' in order to view the model fits
#Date: March 2020   



# Get thee required packages
require('minpack.lm') #for model fitting linear models 
library('dplyr') #For data wrangling
library('tidyr') # For data wrangling


######################################## PREPARES DATA ###################

#Load in data
data <- read.csv('../data/modified_CRat.csv', header = TRUE) #reads in data

data <- data[, -1] #removes first column 

data2 <- data %>%
  nest(data= -ID) #this is nesting data by ID, by nesting the data i can suset the day by ID and go into that specific data

########################  CREATE FUNCTION FOR OBTAINING INITIAL START VALUES ###########

# A function that is designed to find the initial starting values before optimisation- this is to speed up running time 

obtaining_start_values <- function(data) {
  ### Store the peak handling time in a variable (h)
  # Store maximum y value
  h <- max(data$N_TraitValue)
  ### Remove points after this value in order to fit models to the growth period
  # Find mean y value
  meanTraitValue <- mean(data$N_TraitValue)
  # Find max y value
  maxTraitValue <- max(data$N_TraitValue)
  # Subset the data to contain only x values lower or higher than these points
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



############# INPUTTING STARTING VALUES INTO A DATAFRAME TO USE PER ID #############
# In order to fit the most models each individual curve should have its own starting value based on its individual curve shape
# A data frame is created to store the starting values in 
finalframe = as.data.frame(matrix(nrow = 1, ncol = 3))
# suppress any warnings as this is marked down in miniproject criteria 
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
#remove first row of dataframe
datatouse <- finalframe[-1, ]  
#create column names of data 
datcol<-c("ID","a","h")
colnames(datatouse) <- datcol

# merge the data with the starting values for each ID with the main dataframe so each ID has its starting value
data2 <- merge(data2, datatouse, by= "ID")


####################################### DEFINING FUNCTIONS OF MODELS ##############

#Holling type II functional response
#This is making a function of the second model we looked at 
powMod <- function(x, a, h) { #These are parameters
  return( (a*x ) / (1+ (h*a*x))) # This is the equation
}


#Generalised functional response model
# Similar to holling type II but includes q which is a shape parameter 
GenMod <- function(x, a, h, q) { #These are parameters
  return( (a* x^(q+1) ) / (1+ (h*a*x^(q+1)))) # This is the equation 
}


############## OPTIMISING STARTING VALUES ##################
#create a table to input optised start values into 
optimising <-data.frame("ID"= NA,
                        "a"= NA,
                        "h"= NA, "minAIC"= NA, stringsAsFactors = T)

# suppress any warnings as these are marked down in miniproject guidelines
suppressWarnings(
for(i in 1:length(data2$data)){
  first <- data2[i,] #select each data category 
  a <- first$a #make a variable
  h <- first$h #make h variable 
  datatry <- data2$data[[i]] # put subsection of data into a variable name 
  avalues <- rnorm(10, a, 10)  #select 10 randonmly selected variables within a 10 range of a and h to get the values to run through the mdoel 
  hvalues <- rnorm(10, h, 10) 
  id <- first$ID #save id as variable 
  TempTable1 <- data.frame("avalues" = NA, "hvalues" = NA, "AIC_values" = NA) #create a temporary dataframe to put sampled values into to find lowest AIC
  TempTable2 <- data.frame("avalues" = NA, "hvalues" = NA, "AIC_values" = NA)
  # for loop for looping through and applying models over 10 of the randonly selected values 
  for (j in 1:10){
    ### Fit models
    AICHol <- c()
    AICGen <- c()
    a <- avalues[j] #save random value as a and h for this iteration of the loop
    h <- hvalues[j]
    #and fit the model to it like before in the example 
    #use try because it stops any null values being included
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
    # Holling II model
    # Calculate the AIC of the modelfits 
    AICHol <- ifelse(class(PowFit) == "try-error", NA, AIC(PowFit)) #use if else statement for dealing with NA values 
    AICGen <- ifelse(class(GenFit) == "try-error", NA, AIC(GenFit))
    temptable1 <- list(a, h, AICHol) #put the values into the first temporary dataframe for later comparison to find lowest AIC value
    TempTable1 <- rbind(TempTable1, temptable1)
    temptable2 <- list(a, h, AICGen) # Do the same for the generalised linear model 
    TempTable2 <- rbind(TempTable2, temptable2)
  }
  TempTable1 <- rbind(TempTable1, TempTable2) #bind tables together to find minimum AIC value- chose to ignore which model it was 
  minAIC <- TempTable1[which.min(TempTable1$AIC_values),] 
  min1 <- minAIC[1] #input minimum AIC values and its associated a and h values into the optimising dataframe for use in later for loop 
  min2 <- minAIC[2]
  min3 <- minAIC[3]
  add <- list(id, min1, min2, min3)
  optimising <- rbind(optimising, add)
}
)

#nest data again in order to merge with new, optimised data and subset by ID like last time 
dataagain <- data %>%
  nest(data= -ID) 
dataoptimised <- merge(dataagain, optimising, by= "ID")

#create dataframe for fidning the best model fits of each curve based on AIC and BIC values 
modelvec2<-c("PowFit","QuaFit","GenFit", "CubFit")
modelvec2<-data.frame("ID "=rep(NA),
                      "Habitat"=rep(NA),
                      "ResDimension"=rep(NA),
                      "ConDimension"=rep(NA),
                      "Model"=modelvec2,
                      "AIC"=rep(NA),
                      "BIC"=rep(NA))

#create another column to format data in a different way for later use in statistics. 
modelvec_back<-c("PowFit","QuaFit","GenFit", "CubFit") 
# create two new columns and convert into a data frame with the AICs and BICs 
modelvec_back<-data.frame("Model"=modelvec_back,
                          "AIC"=rep(NA),
                          "BIC"=rep(NA))


# Create new data frame to store values overall
optimisedtable <- c("ID", "a", "h", "minAIC", "minBIC", "AIC", "BIC", "Habitat", "Res_Dim", "Con_Dim")
optimisedtable<-data.frame("ID"= NA,
                           "a"= NA,
                           "h"= NA, "minAIC"= NA, "minBIC"= NA, "AIC"= NA, "BIC"= NA, "Habitat"= NA, "Res_Dim" = NA, "Con_Dim" = NA, stringsAsFactors = T)

suppressWarnings(
for(i in 1:length(dataoptimised$data)){
  add <- c()
  datatry <- dataoptimised$data[[i]] #select each data category 
  a <- dataoptimised$a[i] #select the optimsied startnng value based on that individual ID 
  h <- dataoptimised$h[i]
  habitat <- datatry$Habitat[1] #also select the habitats and the dimensionality of the consumer and resource for later analysis in plotting script 
  res_dim <- datatry$Res_MovementDimensionality[1]
  con_dim <- datatry$Con_MovementDimensionality[1]
  q = 0.78 # set q to 0.78 as recommended in Pawar, 2012 
  #create temporary dataframe for inputting values of that loop 
  modelvec<-c("PowFit","QuaFit","GenFit", "CubFit")
  modelvec<-data.frame("ID "=rep(NA),
                       "Habitat"=rep(NA),
                       "ResDimension"=rep(NA),
                       "ConDimension"=rep(NA),
                       "Model"=modelvec,
                       "AIC"=rep(NA),
                       "BIC"=rep(NA))
  # run model fitting of the four models on the data 
  # using try to catch and ignore errors 
  PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
  QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
  GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q=q)), silent=T)
  CubFit <- try(lm(N_TraitValue ~ poly(ResDensity,3), data = datatry), silent = T)
  #select each index of the modelvec dataframe, one for each model 
  # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
  # input the AIC and BIC values of the temporary data frame into the dataframe 
  modelvec[1,6]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (AIC(PowFit)))
  modelvec[2,6]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (AIC(QuaFit)))
  modelvec[3,6]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (AIC(GenFit)))
  modelvec[4,6]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (AIC(CubFit)))
  modelvec[1,7]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (BIC(PowFit)))
  modelvec[2,7]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (BIC(QuaFit)))
  modelvec[3,7]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (BIC(GenFit)))
  modelvec[4,7]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (BIC(CubFit)))
  modelvec[1:4,1] <- data2$ID[[i]] # also inputting id into the model vec as well as habitat and dimensionality 
  modelvec[1:4,2] <- datatry$Habitat[[1]]
  modelvec[1:4,3] <- datatry$Res_MovementDimensionality[[1]]
  modelvec[1:4,4] <- datatry$Con_MovementDimensionality[[1]]
  # inputting AIC and BIC values into the other dataframe as well in order to create two dataframes, one for plotting and another for statistics 
  modelvec_back[1,2]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (AIC(PowFit)))
  modelvec_back[2,2]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (AIC(QuaFit)))
  modelvec_back[3,2]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (AIC(GenFit)))
  modelvec_back[4,2]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (AIC(CubFit)))
  modelvec_back[1,3]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (BIC(PowFit)))
  modelvec_back[2,3]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (BIC(QuaFit)))
  modelvec_back[3,3]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (BIC(GenFit)))
  modelvec_back[4,3]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (BIC(CubFit)))
  # Finding minimum AICs and BICs 
  okAIC <- modelvec_back[which(modelvec_back$AIC==min(modelvec_back$AIC,na.rm = T)), 1]
  okBIC <- modelvec_back[which(modelvec_back$BIC==min(modelvec_back$BIC,na.rm = T)), 1]
  # making the index of the model name e.g. cubfit, genfit, into a character for easy reading 
  okAIC <- as.character(okAIC[1][[1]])
  okBIC <- as.character(okBIC[1][[1]])
  # doing the same for the other data frame 
  minAIC <- min(modelvec_back[,2], na.rm=T)
  minBIC <- min(modelvec_back[,3], na.rm=T)
  # creating list and binding this list into the data for the optimised data frame
  add <- list(data2$ID[[i]], a, h, minAIC, minBIC, okAIC[1], okBIC[1], habitat, res_dim, con_dim)
  optimisedtable <- rbind(optimisedtable, add)
  #adding to second dataframe 
  modelvec2 <- bind_rows(modelvec2, modelvec)
}
)
#small forloop to remove first four rows of NAs in the modelvec2 dataframe 
for (i in 1:4){
  modelvec2 <- modelvec2[-1, ]
}


# adding a new column to say what overall model was each fit to compare whether phenomenological or mechanistic models are a better fit 
optimisedtable$mecphe <- ifelse(grepl("QuaFit|CubFit", optimisedtable$AIC), "Phenomenological", "Mechanistic")

optimisedtable <- optimisedtable[-1, ]
#making table as a character so you can read which model best fit each curve 
optimisedtable <- apply(optimisedtable,2,as.character)

# saving both models as csv files for the pltting script in python. 
write.csv(optimisedtable, file = "../data/optimisedtable.csv")
write.csv(modelvec2, file = "../data/MergedOptTable.csv")

########################################## PLOTTING EXAMPLE CURVES FOR METHODS ##############################################

#Subset data with a nice looking curve to model with 
Data2Fit <- subset(data, ID == 39982) #One curve

#create the starting parameters, save a variable aline as the data below the mean 
a.line <- subset(Data2Fit, ResDensity <= mean(ResDensity))
#calculate linear regression on this line
lm <- summary(lm(N_TraitValue ~ ResDensity, a.line))
# Extracts slope value
a <- lm$coefficients[2]
# h parameter is the maximum of the slope so you take the biggest value 
h <- max(Data2Fit$N_TraitValue)
#make q = -1 for purposes of model visualisation 
q = -1

Lengths <- seq(min(Data2Fit$ResDensity),max(Data2Fit$ResDensity))

# fit data to models 
PowFit <- (nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = Data2Fit, start = list(a=a, h=h)))
QuaFit <- (lm(N_TraitValue ~ poly(ResDensity,2), data = Data2Fit))
GenFit <- (nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q=q), data = Data2Fit, start = list(a=a, h=h, q=q)))
CubFit <- (lm(N_TraitValue ~ poly(ResDensity,3), data = Data2Fit))

#plot the models 
Predic2PlotPow <- powMod(Lengths,coef(PowFit)["a"],coef(PowFit)["h"])
Predic2PlotQua <- predict.lm(QuaFit, data.frame(ResDensity = Lengths))
Predic2PlotCub <- predict.lm(CubFit, data.frame(ResDensity = Lengths))
Predic2PlotGen <- GenMod(Lengths,coef(GenFit)["a"],coef(GenFit)["h"], coef(GenFit)["q"])

#create pdf with example plot inside 
pdf(file = "../results/Example_modelplot.pdf")
plot(Data2Fit$ResDensity, Data2Fit$N_TraitValue, xlab = "Resource density", ylab="Consumption Rate", pch = 19, cex=1.3)
lines(Lengths, Predic2PlotGen, col = 'darkgreen', lwd = 3)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 3)
lines(Lengths, Predic2PlotQua, col = 'red', lwd = 3)
lines(Lengths, Predic2PlotCub, col = 'gold', lwd = 3) 
legend("topleft", c("Generalised, q = -1","Holling II", "Quadratic", "Cubic"), fill=c("darkgreen","blue", "red", "gold"))
dev.off()
                                                                                                         
print("models fitted and associated AIC and BIC values calculated, onto the python plotting and analysis script.")


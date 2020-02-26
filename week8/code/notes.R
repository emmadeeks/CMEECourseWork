


####################################### NOTES ############################
######################ORGINAL FOR LOOP WITHOUT INDEXING INDIVIDUAL STARTING VALUES 
#for the number of rows in the data part of the nested data 
for(i in 1:length(data2$data)){
  datatry <- data2$data[[i]] #select each data category 
  #and fit the model to it like before in the example 
  # use try because it stops any null values being included 
  PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
  QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
  GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
  #select each index of the modelvec dataframe, one for each model 
  # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
  modelvec[1,2:3]<-ifelse(class(PowFit)=="try-error",rep(NA,2),c(AIC(PowFit),BIC(PowFit)))
  modelvec[2,2:3]<-ifelse(class(QuaFit)=="try-error",rep(NA,2),c(AIC(QuaFit),BIC(QuaFit)))
  modelvec[3,2:3]<-ifelse(class(GenFit)=="try-error",rep(NA,2),c(AIC(GenFit),BIC(GenFit)))
  
  cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
  
}








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


###################################### OBTAINING STARTING VALUES ###############

#This is basically cutting the last points of the graph off 
# After the highest point 
# Because a needs to fit to just the slope
paraopt = as.data.frame(matrix(nrow = 1, ncol = 4)) #create an empty dataframe
finalframe = as.data.frame(matrix(nrow = 1, ncol = 4))
for (ii in data2$data[[ii]]){
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
  
  for(i in 1:length(data2$data)){
    index <- c()
    add <- c()
    datatry <- data2$data[[i]] #select each data category 
    #and fit the model to it like before in the example 
    # use try because it stops any null values being included
    touse <- datatouse[i,]
    a <- touse$V2
    h <- touse$V3
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
    #select each index of the modelvec dataframe, one for each model 
    # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
    modelvec[1,2:3]<-ifelse(class(PowFit)=="try-error",rep(NA,2),c(AIC(PowFit), BIC(PowFit)))
    modelvec[2,2:3]<-ifelse(class(QuaFit)=="try-error",rep(NA,2),c(AIC(QuaFit), BIC(QuaFit)))
    modelvec[3,2:3]<-ifelse(class(GenFit)=="try-error",rep(NA,2),c(AIC(GenFit), BIC(GenFit)))
    ok <- modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]
    index <- apply(modelvec,2, min)
    add <- c(data2$ID[[i]], a, h, ok, index[2], index[3])
    paraopt <- rbind(paraopt, add)
    cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
    
  }
  
  
  
  
  for(i in 1:length(data2$data)){
    minAIC <- c()
    minBIC  <- c()
    datatry <- data2$data[[i]] #select each data category 
    #and fit the model to it like before in the example 
    # use try because it stops any null values being included
    touse <- datatouse[i,]
    a <- touse$V2
    h <- touse$V3
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
    minAIC <- min(modelvec[ ,2], na.rm = TRUE)
    minBIC <- min(modelvec[ ,3], na.rm = TRUE)
    AICmodel <- modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),]
    BICmodel <- modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),]
    add <- c(data2$ID[[i]], a, h, minAIC, minBIC, AICmodel[1], BICmodel[1])
    paraopt <- rbind(paraopt, add)
    cat(paste0("id: ",data2$ID[[i]]," ; min AIC:", modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:", modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
    
  }
  
  
  
  
  
  
  
  
  
  
  
  for(i in 1:length(data2$data)){
    index <- c()
    add <- c()
    datatry <- data2$data[[i]] #select each data category 
    #and fit the model to it like before in the example 
    # use try because it stops any null values being included
    touse <- datatouse[i,]
    a <- touse$V2
    h <- touse$V3
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
    paraopt <- rbind(paraopt, add)
    #cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
    
  }
  
  
  
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
    q = -1 
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
      #GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
      # Holling II model
      #AICgen <- ifelse(class(GenFit) == "try-error", NA, AIC(GenFit))
      AICHol <- ifelse(class(PowFit) == "try-error", NA, AIC(PowFit))
      temptable <- c(a, h, AICHol)
      TempTable <- rbind(TempTable, temptable)
    }
    #minAIC <- min(TempTable[,3], na.rm = T)
    #minAIC <- TempTable[which(TempTable[3] == minAIC), 1:2]
    minAIC <- TempTable[which.min(TempTable$AIC_Hol_values),]
    min1 <- minAIC[1]
    min2 <- minAIC[2]
    min3 <- minAIC[3]
    add <- list(id, min1, min2, min3)
    optimising <- rbind(optimising, add)
  }
  
  
  
  
  
  
  
  
  
  
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
  
  
  
  
  
  
  
  
  
  for(i in 1:length(dataoptimised$data)){
    index <- c()
    add <- c()
    datatry <- dataoptimised$data[[i]] #select each data category 
    #and fit the model to it like before in the example 
    # use try because it stops any null values being included
    a <- dataoptimised$a[i]
    h <- dataoptimised$h[i]
    q = 0.78
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
    CubFit <- try(lm(N_TraitValue ~ poly(ResDensity,3), data = datatry), silent = T)
    #select each index of the modelvec dataframe, one for each model 
    # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
    modelvec[1,2]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (AIC(PowFit)))
    modelvec[2,2]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (AIC(QuaFit)))
    modelvec[3,2]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (AIC(GenFit)))
    modelvec[4,2]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (AIC(CubFit)))
    modelvec[1,3]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (BIC(PowFit)))
    modelvec[2,3]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (BIC(QuaFit)))
    modelvec[3,3]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (BIC(GenFit)))
    modelvec[4,3]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (BIC(CubFit)))
    okAIC <- modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)), 1]
    okBIC <- modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)), 1]
    okAIC <- as.character(okAIC[1][[1]])
    okBIC <- as.character(okBIC[1][[1]])
    #index <- apply(modelvec,2, min)
    minAIC <- min(modelvec[,2], na.rm=T)
    minBIC <- min(modelvec[,3], na.rm=T)
    add <- list(data2$ID[[i]], a, h, modelvec[2,2], modelvec[4,2], modelvec[1,2], modelvec[3,2])
    #add <- ifelse(okAIC[1]==length(1), c(data2$ID[[i]], a, h, okAIC[1], okBIC[1], index[2], index[3]), )
    #tryCatch(, warning = function(c) print(paste("warning on id", i)))
    optimisedtable <- rbind(optimisedtable, add)
    #cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
    
  }
  
  
  
  
  
  
  
  
  #create a vector with model names 
  modelvec<-c("PowFit","QuaFit","GenFit", "CubFit") 
  
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
    q = 0.78
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
    CubFit <- try(lm(N_TraitValue ~ poly(ResDensity,3), data = datatry), silent = T)
    #select each index of the modelvec dataframe, one for each model 
    # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
    modelvec[1,2]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (AIC(PowFit)))
    modelvec[2,2]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (AIC(QuaFit)))
    modelvec[3,2]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (AIC(GenFit)))
    modelvec[4,2]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (AIC(CubFit)))
    modelvec[1,3]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (BIC(PowFit)))
    modelvec[2,3]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (BIC(QuaFit)))
    modelvec[3,3]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (BIC(GenFit)))
    modelvec[4,3]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (BIC(CubFit)))
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
  
  
  
  
  
  modelvec2<-data.frame("Model"=modelvec2,
                        "AIC"=rep(NA),
                        "BIC"=rep(NA),
                        "ID "=rep(NA))
  
  
  
  optimisedtable<-data.frame("ID"= NA,
                             "a"= NA,
                             "h"= NA, "AIC_Qua"= NA, "AIC_Cub"= NA, "AIC_Hol"= NA, "AIC_Gen"= NA, stringsAsFactors = T)
  
  for(i in 1:length(dataoptimised$data)){
    index <- c()
    add <- c()
    modelvec<-c("PowFit","QuaFit","GenFit", "CubFit") 
    modelvec<-data.frame("Model"=modelvec,
                         "AIC"=rep(NA),
                         "habitat"=rep(NA),
                         "ID "=rep(NA))
    datatry <- dataoptimised$data[[i]] #select each data category 
    #and fit the model to it like before in the example 
    # use try because it stops any null values being included
    a <- dataoptimised$a[i]
    h <- dataoptimised$h[i]
    q = 0.78
    habitat <- datatry$Habitat[1]
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
    CubFit <- try(lm(N_TraitValue ~ poly(ResDensity,3), data = datatry), silent = T)
    #select each index of the modelvec dataframe, one for each model 
    # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
    modelvec[1,2]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (AIC(PowFit)))
    modelvec[2,2]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (AIC(QuaFit)))
    modelvec[3,2]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (AIC(GenFit)))
    modelvec[4,2]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (AIC(CubFit)))
    #modelvec[1,3]<-ifelse(class(PowFit)=="try-error",rep(NA,1), (BIC(PowFit)))
    #modelvec[2,3]<-ifelse(class(QuaFit)=="try-error",rep(NA,1), (BIC(QuaFit)))
    #modelvec[3,3]<-ifelse(class(GenFit)=="try-error",rep(NA,1), (BIC(GenFit)))
    #modelvec[4,3]<-ifelse(class(CubFit)=="try-error",rep(NA,1), (BIC(CubFit)))
    modelvec[1,3]<- habitat
    modelvec[2,3]<- habitat
    modelvec[3,3]<- habitat
    modelvec[4,3]<- habitat
    modelvec[1,4]<- data2$ID[[i]]
    modelvec[2,4]<- data2$ID[[i]]
    modelvec[3,4]<- data2$ID[[i]]
    modelvec[4,4]<- data2$ID[[i]]
    #okAIC <- modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)), 1]
    #okBIC <- modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)), 1]
    #okAIC <- as.character(okAIC[1][[1]])
    #okBIC <- as.character(okBIC[1][[1]])
    #index <- apply(modelvec,2, min)
    #minAIC <- min(modelvec[,2], na.rm=T)
    #minBIC <- min(modelvec[,3], na.rm=T)
    #add <- list(data2$ID[[i]], a, h, modelvec[2,2], modelvec[4,2], modelvec[1,2], modelvec[3,2])
    #add <- ifelse(okAIC[1]==length(1), c(data2$ID[[i]], a, h, okAIC[1], okBIC[1], index[2], index[3]), )
    #tryCatch(, warning = function(c) print(paste("warning on id", i)))
    modelvec2 <- bind_rows(modelvec2, modelvec)
    #cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
    
  }
  
  
  data <- read.csv('modified_CRat.csv', header = TRUE) #reads in data
  
  
  data <- data[, -1] #removes first column 
  
  
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
  
  
  optimisedtable<-data.frame("ID"= NA,
                             "a"= NA,
                             "h"= NA, "AIC_Qua"= NA, "AIC_Cub"= NA, "AIC_Hol"= NA, "AIC_Gen"= NA, stringsAsFactors = T)
  
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
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
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
    #index <- apply(modelvec,2, min)
    minAIC <- min(modelvec_back[,2], na.rm=T)
    minBIC <- min(modelvec_back[,3], na.rm=T)
    add <- list(data2$ID[[i]], a, h, minAIC, minBIC, okAIC[1], okBIC[1], habitat, res_dim, con_dim)
    #add <- ifelse(okAIC[1]==length(1), c(data2$ID[[i]], a, h, okAIC[1], okBIC[1], index[2], index[3]), )
    #tryCatch(, warning = function(c) print(paste("warning on id", i)))
    optimisedtable <- rbind(optimisedtable, add)
    modelvec2 <- bind_rows(modelvec2, modelvec)
    
    
  }
  
  for (i in 1:4){
    modelvec2 <- modelvec2[-1, ]
  }
  
  
  
  modelvec_back<-c("PowFit","QuaFit","GenFit", "CubFit") 
  
  # create two new columns and convert into a data frame with the AICs and BICs 
  modelvec_back<-data.frame("Model"=modelvec_back,
                            "AIC"=rep(NA),
                            "BIC"=rep(NA))
  
  #paraopt = as.data.frame(matrix(nrow = 1, ncol = 7), stringsAs)
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
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
    CubFit <- try(lm(N_TraitValue ~ poly(ResDensity,3), data = datatry), silent = T)
    #select each index of the modelvec dataframe, one for each model 
    # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
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
    #index <- apply(modelvec,2, min)
    minAIC <- min(modelvec_back[,2], na.rm=T)
    minBIC <- min(modelvec_back[,3], na.rm=T)
    add <- list(data2$ID[[i]], a, h, minAIC, minBIC, okAIC[1], okBIC[1], habitat, res_dim, con_dim)
    #add <- ifelse(okAIC[1]==length(1), c(data2$ID[[i]], a, h, okAIC[1], okBIC[1], index[2], index[3]), )
    #tryCatch(, warning = function(c) print(paste("warning on id", i)))
    optimisedtable <- rbind(optimisedtable, add)
    #cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
    
  }
  
  optimisedtable$mecphe <- ifelse(grepl("QuaFit|CubFit", optimisedtable$AIC), "Mechanistic", "Phenological")
  
  #duplicating <- data[,-2]
  #duplicating <- data[,-3]
  
  #mergeddataframe <- merge(optimisedtable, duplicating, by = "ID")
  
  
  
  #optimisedtable <- as.data.frame(optimisedtable)
  #mergeddataframe <- mergeddataframe[-1, ]
  
  optimisedtable <- apply(optimisedtable,2,as.character)
  
  
  
  
  ####
  
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
    PowFit <- try(nlsLM(N_TraitValue ~ powMod(ResDensity, a, h), data = datatry, start = list(a=a, h=h)), silent=T)
    QuaFit <- try(lm(N_TraitValue ~ poly(ResDensity,2), data = datatry), silent=T)
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
    CubFit <- try(lm(N_TraitValue ~ poly(ResDensity,3), data = datatry), silent = T)
    #select each index of the modelvec dataframe, one for each model 
    # if the powfit is an error then repeat two more times, if it isnt an error then output the two AIC values 
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
    #index <- apply(modelvec,2, min)
    minAIC <- min(modelvec_back[,2], na.rm=T)
    minBIC <- min(modelvec_back[,3], na.rm=T)
    add <- list(data2$ID[[i]], a, h, minAIC, minBIC, okAIC[1], okBIC[1], habitat, res_dim, con_dim)
    #add <- ifelse(okAIC[1]==length(1), c(data2$ID[[i]], a, h, okAIC[1], okBIC[1], index[2], index[3]), )
    #tryCatch(, warning = function(c) print(paste("warning on id", i)))
    optimisedtable <- rbind(optimisedtable, add)
    #cat(paste0("id: ",data2$ID[[i]]," ; min AIC:",modelvec[which(modelvec$AIC==min(modelvec$AIC,na.rm = T)),1]," ; min BIC:",modelvec[which(modelvec$BIC==min(modelvec$BIC,na.rm = T)),1],"\n"))
    
  }
  
  
  
  
  
  
  for(i in 1:length(dataoptimised$data)){
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
    GenFit <- try(nlsLM(N_TraitValue ~ GenMod(ResDensity, a, h, q), data = datatry, start = list(a=a, h=h, q= q)), silent=T)
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
    modelvec2 <- bind_rows(modelvec2, modelvec)
    
    
  }
  
  
  
  #optimisedtable %>% group_by(optimisedtable$minAIC) %>% summarise(count=n())
  
  #paraopt %>% group_by(paraopt$minAIC) %>% summarise(count=n())
  #datamerge <- read.csv('MergedOptTable.csv', header = TRUE) #reads in data
  
  
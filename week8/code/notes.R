


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
  
  
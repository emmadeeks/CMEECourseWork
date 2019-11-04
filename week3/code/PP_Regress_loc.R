MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

############## csv ##############
#Creates an empty dataframe

Orange = as.data.frame(matrix(nrow = 1, ncol = 7))

#For loop that uses two inputs to the for loop which is the two things you are subsetting
#t is the variable for the data once its been subsetted once, and predator lifestage is assigned to i
#and then subsetted again and type of feeding interaction is assigned to j
#a linear regression is run on the subsette ddata
#p is a variable then then puts the outputs of the vector in order
#p then goes into the re made dataframe.
for(i in levels(MyDF$Predator.lifestage)){
  for(j in levels(MyDF$Type.of.feeding.interaction)){
    t = subset(MyDF, MyDF$Predator.lifestage == i)
    t = subset(t, t$Type.of.feeding.interaction == j)
    if (dim(t)[1] > 0){
      mylm <- summary(lm(Predator.mass~Prey.mass, data = t))
      p <- c(i,j,mylm$coefficients[1], mylm$coefficients[2], mylm$adj.r.squared, mylm$fstatistic[1], mylm$coefficients[8])
      Orange = rbind(Orange, p)
    }
  }
}

#takes off certain rows and then writes csv to a file
Orange <- Orange[-c(1, 17),]
colnames(Orange) <- c("Predator_Lifestage", "Feeding_interaction", "Intercept", "Slope", "Adjusted_R_sequared", "FStat", "Pvalue")
write.csv(Orange, file = "../results/PP_Regress_Results.csv")
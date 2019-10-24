# Script that creates and saves a graph as a pdf file that exactly replicates a graph
#also calculates the regression of the data when its been subsetted two times and outputs the results to a table called PP_Regress_Results.csv.
#Output: PDF file of replicate graph and also csv file with the linear results
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

require(ggplot2)
#This is the correct code
#starts a pdf and plots the graph in a faceted way which is subsetted by the feeding type
#Assigns axis titles
#Seperates the colour based on Predator lifestage, changes shape of points as well as size
#geom smooth using linear regression
#theme bw assumes a basic output
#Puts figure legend at the bottom
#Puts the horizontal key flat and turns off the pdf and saves it
pdf("../results/PP_Regress.pdf")
print(qplot(Prey.mass, Predator.mass, facets = Type.of.feeding.interaction ~., data = MyDF, log="xy",
      xlab= "Prey Mass in grams", ylab= "Predator Mass in grams",
      colour = Predator.lifestage, shape = I(3), alpha = I(.5)) +
  geom_smooth(method = "lm",fullrange= TRUE) +
  theme_bw() +
  theme(legend.position="bottom", panel.border = element_rect(colour = "grey"), legend.title = element_text(size=9, face = "bold")) +
  guides(colour = guide_legend(nrow = 1)))
dev.off()

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

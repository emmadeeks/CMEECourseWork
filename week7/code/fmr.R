#!/usr/bin/env Rscript 

# Plots log(field metabolic rate) against log(body mass) for the Nagy et al 
# 1999 dataset to a file fmr.pdf.

""" Similar to TestR.R as it is an R script that is run through python"""
""" Plots log field metabolic rate against the log body mass and saves it as a pdf """
cat("Reading CSV\n") #Reads string with a newline

nagy <- read.csv('../data/NagyEtAl1999.csv', stringsAsFactors = FALSE) #Reads in csv and saves to a variable 

cat("Creating graph\n") # Comments creating graph with a new line
pdf('../results/fmr_plot.pdf', 11, 8.5) #Opens pdf to plot 
col <- c(Aves='purple3', Mammalia='red3', Reptilia='green3') # assigning colours
plot(log10(nagy$M.g), log10(nagy$FMR.kJ.day.1), pch=19, col=col[nagy$Class], 
     xlab=~log[10](M), ylab=~log[10](FMR)) #Plotting the log of the two variables 
for(class in unique(nagy$Class)){ # Finds the unique values in the column 'class' of the nagy dataset
  model <- lm(log10(FMR.kJ.day.1) ~ log10(M.g), data=nagy[nagy$Class==class,]) #Takes log of each the two y axis the number of unique values
  abline(model, col=col[class]) # fitting linear regression lines 
}
dev.off()

cat("Finished in R! Plot saved in Results directory\n")
